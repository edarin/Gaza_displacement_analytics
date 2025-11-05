# cleanup
rm(list = ls())
gc()
cat("\014")
try(dev.off(), silent = T)
options(scipen = 999)

# libraries
library(dplyr)
library(tidyverse)
library(zoo)

# working directory
env <- new.env()
source(here::here('.env'), local = env)

# prep deatjsh data
deaths <- read.csv(file.path(
  env$wd,
  'in',
  'moh',
  'gaza_deaths.csv'
))

deaths <- deaths |>
  mutate(
    date = as.Date(Date, format = '%d/%m/%Y')
  ) |>
  bind_rows(
    tibble(
      date = as.Date('2023-10-13', format = '%Y-%m-%d'),
      Men_Fatalities = 0,
      Women_Fatalities = 0,
      Elderly_Fatalities = 0
    )
  ) |>
  mutate(
    M_18Plus = Men_Fatalities + 0.5 * Elderly_Fatalities,
    F_18Plus = Women_Fatalities + 0.5 * Elderly_Fatalities,
    M_20Plus = Men_Fatalities + 0.5 * Elderly_Fatalities,
    F_20Plus = Women_Fatalities + 0.5 * Elderly_Fatalities,
    T_18Plus = M_18Plus + F_18Plus,
    T_20Plus = M_20Plus + F_20Plus
  ) |>
  select(
    date,
    M_18Plus,
    F_18Plus,
    M_20Plus,
    F_20Plus,
    T_18Plus,
    T_20Plus
  ) |>
  pivot_longer(
    cols = -date,
    names_to = 'agesex',
    values_to = 'deaths'
  )

# interpolate missing dates
all_dates <- data.frame(
  date = seq.Date(
    from = as.Date('13/10/2023', format = '%d/%m/%Y'),
    to = max(deaths$date),
    by = 'day'
  )
)
all_dates <- all_dates |>
  crossing(
    agesex = c(
      'M_18Plus',
      'F_18Plus',
      'M_20Plus',
      'F_20Plus',
      'T_18Plus',
      'T_20Plus'
    )
  )

deaths <- all_dates |>
  left_join(deaths, by = c('date', 'agesex')) |>
  arrange(agesex, date) |>
  group_by(agesex) |>
  mutate(
    deaths = na.approx(deaths)
  )

# prepare migration data
migration <- read.csv(file.path(
  env$wd,
  'in',
  'paltrade',
  'gaza_broderCrossing.csv'
))

migration <- migration |>
  mutate(
    date = as.Date(Date, format = '%d/%m/%Y')
  ) |>
  mutate(
    net_migration = Exits - Entries
  ) |>
  bind_rows(
    tibble(
      date = as.Date('2023-10-13', format = '%Y-%m-%d'),
      Exits = 0,
      Entries = 0,
      net_migration = 0
    ),
    tibble(
      date = c(
        as.Date('2024-04-30', format = '%Y-%m-%d'),
        as.Date('2024-05-31', format = '%Y-%m-%d')
      ),
      Exits = NA,
      Entries = NA,
      net_migration = c(17000, 17000)
    )
  ) |>
  mutate(
    T_20Plus = net_migration * 0.53,
    T_18Plus = net_migration * 0.53,
    M_20Plus = T_20Plus * 0.5,
    F_20Plus = T_20Plus * 0.5,
    M_18Plus = T_18Plus * 0.5,
    F_18Plus = T_18Plus * 0.5
  ) |>
  select(
    date,
    M_18Plus,
    F_18Plus,
    M_20Plus,
    F_20Plus,
    T_18Plus,
    T_20Plus
  ) |>
  pivot_longer(
    cols = -date,
    names_to = 'agesex',
    values_to = 'migration'
  )

# interpolate missing dates
all_dates <- data.frame(
  date = seq.Date(
    from = as.Date('13/10/2023', format = '%d/%m/%Y'),
    to = max(migration$date),
    by = 'day'
  )
)
all_dates <- all_dates |>
  crossing(
    agesex = c(
      'M_18Plus',
      'F_18Plus',
      'M_20Plus',
      'F_20Plus',
      'T_18Plus',
      'T_20Plus'
    )
  )
migration <- all_dates |>
  left_join(migration, by = c('date', 'agesex')) |>
  arrange(agesex, date) |>
  group_by(agesex) |>
  mutate(
    migration = na.approx(migration)
  )


ggplot() +
  geom_line(data = migration, aes(x = date, y = migration, color = agesex))

#
# combine deaths and migration
deaths_migration <- deaths |>
  filter(date <= max(migration$date)) |>
  left_join(migration, by = c('date', 'agesex')) |>
  mutate(
    net_change = -(migration + deaths)
  )

# write to disk

outdir <- file.path(env$wd, 'out', 'gaza', 'baseline_population')

write.csv(
  x = deaths_migration,
  file = file.path(outdir, 'gaza_deaths_migration.csv'),
  row.names = F
)
