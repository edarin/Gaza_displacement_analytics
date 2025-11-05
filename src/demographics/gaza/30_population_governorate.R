# cleanup
rm(list = ls())
gc()
cat("\014")
try(dev.off(), silent = T)
options(scipen = 999)

# libraries
library(dplyr)
library(tidyverse)

# working directory
env <- new.env()
source(here::here('.env'), local = env)

#---- options ----#
country <- 'gaza'
with_death_migration <- T
scenario_name <- 'base'

# paths
outdir <- file.path(env$wd, 'out', country)
dir.create(file.path(outdir, 'population'), showWarnings = F, recursive = T)

# load data
baseline_pop <- read.csv(file.path(
  outdir,
  'baseline_population',
  'gaza_base_pop_long.csv'
))

if (with_death_migration) {
  deaths_migration <- read.csv(file.path(
    outdir,
    'baseline_population',
    'gaza_deaths_migration.csv'
  ))

  # adjust baseline population by deaths/migration
  pop_national <- baseline_pop |>
    filter(grepl('18Plus|20Plus', agesex)) |>
    group_by(agesex) |>
    summarise(pop_nat = sum(pop)) |>
    left_join(
      deaths_migration |>
        select(date, agesex, net_change) |>
        rename(collection_date = date)
    ) |>
    mutate(
      pop_nat = pop_nat + net_change
    ) |>
    select(-net_change)
} else {
  pop_national <- baseline_pop |>
    filter(grepl('18Plus|20Plus', agesex)) |>
    group_by(agesex) |>
    summarise(pop_nat = sum(pop))
}

#---- set platform ----#
platform <- 'facebook'

# load audience data
audience <- read.csv(file.path(
  outdir,
  'daily_audience',
  paste0('daily_audience_', scenario_name, '.csv')
)) |>

  # replace mau=1000 with mau=500
  mutate(
    mau = ifelse(mau == 1000, 500, mau),
    mau_lower = ifelse(mau_lower == 1000, 500, mau_lower),
    mau_upper = ifelse(mau_upper == 1000, 500, mau_upper)
  )

# create baseline penetration rate

baseline_penrate <- audience |>
  filter(collection_date == min(collection_date)) |>
  left_join(
    baseline_pop
  ) |>
  mutate(
    baseline_penrate = mau_lower / pop
  ) |>
  select(ADM2_PCODE, agesex, baseline_penrate)


#---- calculate adult population sizes by age-sex and governorate ----#
population_adults <- audience |>
  filter(grepl('18Plus|20Plus', agesex)) |>
  # reduce columns
  select(ADM2_PCODE, ADM2_EN, collection_date, agesex, mau_lower) |>

  # join baseline penetration rates
  left_join(baseline_penrate) |>
  group_by(collection_date, agesex) |>
  mutate(
    time_penrate = sum(mau_lower / baseline_penrate)
  ) |>
  ungroup() |>
  # join baseline population totals for Gaza by age-sex group
  left_join(
    pop_national |>
      select(collection_date, agesex, pop_nat)
  ) |>
  # calculate population sizes
  mutate(
    time_penrate = time_penrate / pop_nat,
    pop_mau_lower = mau_lower / (time_penrate * baseline_penrate)
  ) |>
  # select columns to keep
  select(ADM2_PCODE, ADM2_EN, collection_date, agesex, mau_lower, pop_mau_lower)

#---- check sums ----#

# total population
population_adults |>
  group_by(collection_date) |>
  mutate(
    pop_mau_lower = sum(pop_mau_lower)
  ) |>
  select(collection_date, pop_mau_lower) |>
  distinct() |>
  left_join(
    pop_national |>
      group_by(collection_date) |>
      summarise(pop_nat = sum(pop_nat))
  ) |>
  print(n = 100)

# by agesex
population_adults |>
  filter(collection_date %in% c('2023-10-13', '2023-10-15')) |>
  group_by(collection_date, agesex) |>
  mutate(
    total_mau_lower = sum(pop_mau_lower)
  ) |>
  select(collection_date, agesex, total_mau_lower) |>
  arrange(agesex, collection_date) |>
  distinct() |>
  left_join(
    pop_national |>
      group_by(collection_date, agesex) |>
      summarise(pop_nat = sum(pop_nat))
  ) |>
  print(n = 100)


#---- save to disk ----#
write.csv(
  population_adults,
  file.path(
    'data',
    paste0('population_', country, '_', scenario_name, '.csv')
  ),
  row.names = F
)
