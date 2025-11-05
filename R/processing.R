library(RPostgres)
library(jsonlite)
library(sp)
library(tidyverse)
library(tmap)
library(geojsonio)
library(lubridate)
library(future.apply)
library(sf)
library(dplyr)


# store credential locally in .env file (see ./example.env for example)
env <- new.env()
source(here::here('.env'), local = env)

#' Create age group filtering variable based on age_min and age_max
#'
#' @param df Dataframe that contains age_min and age_max
#'
#' @return Dataframe

create_ageGroup <- function(df) {
  df <- df |>
    mutate(
      age_5year = ifelse(
        (age_max - age_min < 6) | (age_min == 65 & age_max == 999),
        T,
        F
      ),
      age_10year = ifelse(
        (age_max - age_min < 11 & age_max - age_min > 6) |
          (age_min == 60 & age_max == 999),
        T,
        F
      ),
      age_13plus = ifelse((age_min == 13 & age_max == 999), T, F),
      age_18plus = ifelse((age_min == 18 & age_max == 999), T, F),
      age_20plus = ifelse((age_min == 20 & age_max == 999), T, F),
      age_15to49 = ifelse((age_min == 15 & age_max == 49), T, F),
      age_15to64 = ifelse((age_min == 15 & age_max == 64), T, F),
      age_20to59 = ifelse((age_min == 20 & age_max == 59), T, F),
      age_18to34 = ifelse((age_min == 18 & age_max == 34), T, F)
    )
  return(df)
}

#' Calculate population from audience data
#'
#' @param audience data.table. Audience data.
#' @param baseline_pop data.table. Baseline population data.
#' @param out_file character. File path to save results.
#' @return data.table. Population data.

calculate_pop_from_audience <- function(audience, baseline_pop, out_file) {
  # Step 1: calculate ratio of audience to pop at the beginning of the time period
  audience_start <- audience |>
    group_by(meta_key, geo_name, agesex) |>
    filter(collection_date == min(collection_date)) |>
    left_join(baseline_pop) |>
    mutate(
      across(c('mau_lower', 'mau_upper'), ~ .x / pop, .names = "{.col}_ratio")
    )

  # Step 2: apply penetration rate to the daily audience

  # only keep days when we have data for all governorates
  n_region <- length(unique(audience$meta_key))

  pop_estimates <- audience |>
    group_by(agesex, collection_date) |>
    filter(n() == length(unique(audience$meta_key))) |>
    ungroup() |>
    left_join(
      audience_start |>
        select(meta_key, geo_name, agesex, pop, ends_with('_ratio'))
    ) |>
    mutate(
      rawPop_mau_lower = mau_lower / mau_lower_ratio,
      rawPop_mau_upper = mau_upper / mau_upper_ratio
    )

  # Step 3: calculate daily scaling factor
  scale_factors <- pop_estimates |>
    group_by(collection_date, agesex) |>
    mutate(across(
      c(starts_with('raw'), pop),
      ~ sum(.x),
      .names = '{.col}_total'
    )) |>
    ungroup() |>
    mutate(across(
      starts_with('rawPop') & ends_with('total'),
      ~ pop_total / .x,
      .names = 'rescale_{.col}'
    )) |>
    select(collection_date, meta_key, geo_name, agesex, starts_with('rescale'))

  # Step 4: rescale population estimates to match national totals
  pop_estimates <- pop_estimates |>
    left_join(scale_factors) |>
    mutate(
      pop_mau_lower = rawPop_mau_lower * rescale_rawPop_mau_lower_total,
      pop_mau_upper = rawPop_mau_upper * rescale_rawPop_mau_upper_total
    ) |>
    select(
      country,
      collection_date,
      meta_key,
      geo_name,
      agesex,
      mau_lower,
      mau_upper,
      pop_mau_lower,
      pop_mau_upper
    )

  #---- check that sum of all governorates matches national total ----#

  pop_total <- baseline_pop |>
    filter(meta_key %in% unique(pop_estimates$meta_key)) |>
    group_by(agesex) |>
    summarise(pop_total = sum(pop)) |>
    pull(pop_total)

  all_almost_equal <- pop_estimates |>
    group_by(collection_date, agesex) |>
    summarise(
      total_mau_lower = sum(pop_mau_lower),
      total_mau_upper = sum(pop_mau_upper)
    ) |>
    summarise(
      all_almost_equal = all(
        abs(total_mau_lower - pop_total) <= 5,
        abs(total_mau_upper - pop_total) <= 5
      ) # equal within 5 people
    ) |>
    pull(all_almost_equal)

  if (all(all_almost_equal, na.rm = T)) {
    message(
      "The sum of all governorates population across the period matches the national total"
    )
  } else {
    message(
      "The sum of all governorates population across the period doesnt match the national total"
    )
  }

  #---- save to disk ----#
  write.csv(pop_estimates, out_file, row.names = F)

  return(pop_estimates)
}
