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
scenario_name <- 'missing_K_200' # options: 'base', 'constant_national', 'missing_Ng&G_200', 'missing_R_200'
show_check <- F
print(scenario_name)
# paths
outdir <- file.path(env$wd, 'out', country)
dir.create(file.path(outdir, 'population'), showWarnings = F, recursive = T)

# load data
baseline_pop <- read.csv(file.path(
  outdir,
  'baseline_population',
  'gaza_base_pop_long.csv'
))

if (scenario_name != 'constant_national') {
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
    select(collection_date, agesex, pop_nat)
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
  paste0('daily_audience.csv')
)) |>

  # replace mau=1000 with mau=500
  mutate(
    mau = ifelse(mau == 1000, 500, mau),
    mau_lower = ifelse(mau_lower == 1000, 500, mau_lower),
    mau_upper = ifelse(mau_upper == 1000, 500, mau_upper)
  )

# add scenario

if (scenario_name == 'missing_Ng&G_200') {
  missing_factor <- 2
  audience <- audience |>
    mutate(
      mau_lower = ifelse(
        ADM2_EN %in%
          c('North Gaza', 'Gaza') &
          collection_date >= as.Date('2023-10-17'),
        mau_lower * missing_factor,
        mau_lower
      ),
    )
}
if (scenario_name == 'missing_R_200') {
  missing_factor <- 2
  audience <- audience |>
    mutate(
      mau_lower = ifelse(
        ADM2_EN == 'Rafah' &
          collection_date >= as.Date('2024-02-01'),
        mau_lower * missing_factor,
        mau_lower
      ),
    )
}

if (scenario_name == 'missing_K_200') {
  missing_factor <- 2
  audience <- audience |>
    mutate(
      mau_lower = ifelse(
        ADM2_EN == 'Khan Younis' &
          collection_date >= as.Date('2024-02-01'),
        mau_lower * missing_factor,
        mau_lower
      ),
    )
}

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
    pop_national
  ) |>
  # calculate population sizes
  mutate(
    time_penrate = time_penrate / pop_nat,
    pop_mau_lower = mau_lower / (time_penrate * baseline_penrate)
  ) |>
  # select columns to keep
  select(ADM2_PCODE, ADM2_EN, collection_date, agesex, mau_lower, pop_mau_lower)

#---- check sums ----#

if (show_check) {
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
}

#---- save to disk ----#
if (scenario_name == 'base') {
  filepath <- file.path(
    'data',
    paste0('population_', country, '_', scenario_name, '.csv')
  )
} else {
  filepath <- file.path(
    'data',
    'scenario',
    paste0('population_', country, '_', scenario_name, '.csv')
  )
}

write.csv(
  population_adults,
  file = filepath,
  row.names = F
)
