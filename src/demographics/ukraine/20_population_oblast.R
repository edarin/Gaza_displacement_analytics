# cleanup
rm(list = ls())
gc()
cat("\014")
try(dev.off(), silent = T)
options(scipen = 999)

# libraries
library(tidyverse)

# working directory
env <- new.env()
source(here::here('.env'), local = env)
source(here::here('R', 'processing.R'))

#---- data ----#

# load baseline population data
baseline_pop <- read.csv(file.path(
  env$wd,
  'in',
  'baseline_population',
  'cod_ps',
  'ukr',
  'pop_by_oblast_v2_0.csv'
))


# load audience data
audience <- read_csv(file.path(
  env$wd,
  "out",
  "ukraine",
  "daily_audience",
  "ukr_facebook_adm1_audience.csv"
)) |>
  select(
    country,
    collection_date,
    meta_key,
    geo_name,
    age_min,
    age_max,
    agesex,
    dau,
    mau_lower,
    mau_upper
  ) |>
  arrange(
    country,
    collection_date,
    meta_key,
    geo_name,
    age_min,
    age_max,
    agesex
  )


# prepare audience -------------------------------------------------------

# compute the 20+ years old audience
# make sure to have the two age group for every observation
audience <- audience |>
  group_by(country, collection_date, meta_key, geo_name) |>
  filter(n() == 2) |>
  ungroup()

audience_13_19 <- audience |>
  filter(age_max == 19)

audience_20_999 <- audience |>
  filter(age_max == 999) |>
  mutate(
    dau = dau - audience_13_19$dau,
    mau_lower = mau_lower - audience_13_19$mau_lower,
    mau_upper = mau_upper - audience_13_19$mau_upper,
    age_min = 20,
    agesex = 'T_20Plus'
  )

# prepare baseline population --------------------------------------------

baseline_pop_20_999 <- baseline_pop |>
  select(
    fbkey,
    paste0('F_', paste(seq(20, 75, 5), seq(24, 79, 5), sep = '_')),
    'F_80_Plus',
    paste0('M_', paste(seq(20, 75, 5), seq(24, 79, 5), sep = '_')),
    'M_80_Plus'
  ) |>
  group_by(fbkey) |>
  mutate(
    pop = rowSums(across(everything())),
    agesex = 'T_20Plus',
    meta_key = fbkey
  ) |>
  ungroup() |>
  select(meta_key, agesex, pop)


# compute population estimates -------------------------------------------

calculate_pop_from_audience(
  audience_20_999,
  baseline_pop_20_999,
  file.path(env$wd, 'out', 'ukraine', 'population', 'ukr_pop_estimates.csv')
)
