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
  'out',
  'egypt',
  'baseline_population',
  'EG_governorate_sex_age_2023.csv'
))


# load audience data
audience <- read_csv(file.path(
  env$wd,
  "out",
  "egypt",
  "daily_audience",
  "EG_facebook_adm1_audience.csv"
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
# make sure to have one observation per day
audience_20_999 <- audience |>
  group_by(country, collection_date, meta_key, geo_name) |>
  filter(n() == 1) |>
  ungroup()


# prepare baseline population --------------------------------------------

baseline_pop_20_999 <- baseline_pop |>
  mutate(
    geo_name = case_when(
      governorate == 'Behera' ~ 'Beheira',
      governorate == 'Fayoum' ~ 'Faiyum',
      governorate == 'Menoufia' ~ 'Monufia',
      governorate == 'Menia' ~ 'Minya',
      governorate == 'Kalyoubia' ~ 'Qalyubia',
      governorate == 'ElWadi El Gidid' ~ 'New Valley',
      governorate == 'Kafr-El-Sheikh' ~ 'Kafr el-Sheikh',
      governorate == 'Suhag' ~ 'Sohag',
      governorate == 'Port-Said' ~ 'Port Said',
      governorate == 'Beni-Suef' ~ 'Beni Suef',
      governorate == 'Sharkia' ~ 'Al Sharqia',
      governorate == 'Asyout' ~ 'Asyut',
      T ~ governorate
    ),
    geo_name = paste(geo_name, 'Governorate'),
    pop = population
  ) |>
  left_join(
    audience_20_999 |>
      distinct(meta_key, geo_name)
  )


# compute population estimates -------------------------------------------

calculate_pop_from_audience(
  audience_20_999,
  baseline_pop_20_999,
  file.path(env$wd, 'out', 'egypt', 'population', 'eg_pop_estimates.csv')
)
