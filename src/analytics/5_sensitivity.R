# cleanup
rm(list = ls())
gc()
cat("\014")
try(dev.off(), silent = T)
options(scipen = 999)
library(tidyverse)
library(ggh4x)

env <- new.env()
source(".env", local = env)
source('R/toolbox.R')
col_governorate <- c("#001D23", "#457C36", "#83A40E", "#e7d97e", "#ffca79")
outdir <- file.path(env$wd, "out", "gaza")

# Load population data -----------------------------------------------------
agesex_select <- c('M_18Plus', 'F_18Plus', 'T_18Plus')

pop_gaza <- read_csv(file.path(
  'data',
  paste0('population_gaza_base.csv')
)) |>
  filter(agesex %in% agesex_select) |>
  mutate(scenario = 'baseline')

pop_scenario_list <- list.files(
  file.path(
    'data',
    'scenario'
  ),
  full.names = T
)

pop_scenario <- bind_rows(
  pop_gaza,
  lapply(pop_scenario_list, function(df_path) {
    df <- read_csv(df_path)
    df <- df |>
      filter(agesex %in% agesex_select)
    df$scenario <- str_remove(basename(df_path), '.csv') |>
      str_remove('population_gaza_')
    return(df)
  })
)

print(unique(pop_scenario$scenario))

baseline_pop <- read.csv(file.path(
  outdir,
  'baseline_population',
  'gaza_base_pop_long.csv'
))

deaths_migration <- read.csv(file.path(
  outdir,
  'baseline_population',
  'gaza_deaths_migration.csv'
))


# Prepare population data for analysis ----------------------------------------------
pop_scenario <- pop_scenario |>
  arrange(collection_date) |>
  group_by(ADM2_EN, agesex, scenario) |>
  tq_mutate(
    select = pop_mau_lower,
    mutate_fun = rollapply,
    width = 2,
    align = "right",
    FUN = mean,
    col_rename = "pop"
  ) |>
  mutate(
    pop = ifelse(is.na(pop), pop_mau_lower, pop),
    ADM2_EN = factor(
      ADM2_EN,
      levels = c("North Gaza", "Gaza", "Deir Al-Balah", "Khan Younis", "Rafah")
    )
  ) |>
  select(-pop_mau_lower)

pop_scenario_displacement <- pop_scenario |>
  group_by(ADM2_EN, scenario, agesex) |>
  arrange(collection_date) |>
  mutate(
    pop_displacement = pop - pop[collection_date == min(collection_date)]
  ) |>
  filter(pop_displacement >= 0) |>
  group_by(scenario, collection_date, agesex) |>
  summarise(
    pop_displacement = sum(pop_displacement, na.rm = T)
  )


# Assess sensitivity to national population change ------------------------------------------------------------
# adjust baseline population by deaths/migration
pop_national <- baseline_pop |>
  filter(agesex %in% agesex_select) |>
  group_by(agesex) |>
  summarise(pop_baseline = sum(pop)) |>
  left_join(
    deaths_migration |>
      select(date, agesex, net_change) |>
      rename(collection_date = date)
  ) |>
  mutate(
    pop_nat = pop_baseline + net_change
  )

ggplot(
  pop_scenario |>
    filter(scenario %in% c('baseline', 'constant_national')) |>
    filter(agesex == 'T_18Plus') |>
    mutate(
      scenario = case_when(
        scenario == 'baseline' ~ 'With Deaths/Migration',
        scenario == 'constant_national' ~ 'Without Deaths/Migration'
      )
    )
) +
  geom_line(
    aes(
      x = collection_date,
      y = pop,
      color = ADM2_EN,
      linetype = scenario
    ),
    linewidth = 1.2
  ) +
  theme_minimal() +
  labs(
    title = "Impact on population estimates of fixing national population",
    x = "",
    y = "Number of people",
    color = "Governorate",
    linetype = "Scenario"
  ) +
  scale_color_manual(values = col_governorate) +
  theme(
    panel.background = element_rect(
      fill = "grey97",
      color = "grey97"
    )
  ) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

ggplot(
  pop_scenario_displacement |>
    filter(scenario %in% c('baseline', 'constant_national')) |>
    filter(agesex %in% c('M_18Plus', 'F_18Plus')) |>
    left_join(
      pop_national |>
        filter(agesex %in% c('M_18Plus', 'F_18Plus')) |>
        mutate(collection_date = as.Date(collection_date)) |>
        mutate(
          baseline = pop_nat,
          constant_national = pop_baseline
        ) |>
        select(-pop_baseline, -net_change, -pop_nat) |>
        pivot_longer(
          cols = c(baseline, constant_national),
          names_to = "scenario",
          values_to = "pop_national"
        )
    ) |>
    pivot_longer(
      cols = c(pop_displacement, pop_national),
      names_to = "metric",
      values_to = "value"
    ) |>
    mutate(
      metric = case_when(
        metric == "pop_displacement" ~ "Displaced Population",
        metric == "pop_national" ~ "National Population"
      ),
      agesex = case_when(
        agesex == "M_18Plus" ~ "Male  18+",
        agesex == "F_18Plus" ~ "Female 18+"
      ),
      scenario = case_when(
        scenario == "baseline" ~ "With Deaths/Migration",
        scenario == "constant_national" ~ "Without Deaths/Migration"
      )
    )
) +
  geom_line(aes(
    x = collection_date,
    y = value,
    color = scenario
  )) +
  theme_minimal() +
  facet_grid(metric ~ agesex, scales = "free_y") +
  labs(
    title = "Impact on population displacement estimates of fixing national population",
    x = "",
    y = "Number of people",
    color = "Scenario"
  ) +
  scale_color_manual(values = c("orange", "black")) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

# Assess sensitivity to missing users in North Gaza & Gaza ------------------------------------------------------------

ggplot(
  pop_scenario |>
    filter(scenario %in% c('baseline', 'missing_Ng&G_200')) |>
    filter(agesex == 'T_18Plus') |>
    mutate(pop = ifelse(mau_lower <= 500, NA, pop)) |>
    mutate(
      scenario = case_when(
        scenario == 'baseline' ~ 'Baseline',
        scenario ==
          'missing_Ng&G_200' ~ 'Missing half users\nin North Gaza & Gaza'
      )
    )
) +
  geom_line(aes(
    x = collection_date,
    y = pop,
    color = ADM2_EN,
    linetype = scenario
  )) +
  theme_minimal() +
  labs(
    title = "Impact on population estimates of missing users in North Gaza & Gaza",
    x = "",
    y = "Number of people",
    color = "Governorate",
    linetype = "Scenario"
  ) +
  scale_color_manual(values = col_governorate) +
  theme(
    panel.background = element_rect(
      fill = "grey97",
      color = "grey97"
    )
  ) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12)
  )


ggplot(
  pop_scenario_displacement |>
    filter(scenario %in% c('baseline', 'missing_Ng&G_200')) |>
    filter(agesex %in% c('M_18Plus', 'F_18Plus')),
) +
  geom_line(aes(
    x = collection_date,
    y = pop_displacement,
    color = scenario
  )) +
  theme_minimal() +
  facet_wrap(~agesex, scales = "free_y") +
  labs(
    title = "Impact on population displacement estimates due to missing users in North Gaza & Gaza",
    x = "",
    y = "Number of people",
    color = "Scenario"
  ) +
  scale_color_manual(values = c("orange", "black"))

# stats

annotations <- tibble(
  x = rep(as.Date(c('2024-02-20')), 2),
  y = c(105, -105),
  label = c(
    "Double of governorate population",
    "Half of governorate population"
  ),
  metric = c("Percentage", "Percentage")
)

ggplot(
  pop_scenario |>
    filter(scenario %in% c('baseline', 'missing_Ng&G_200')) |>
    filter(agesex == 'T_18Plus') |>
    filter(collection_date > as.Date('2023-10-17')) |>
    pivot_wider(
      id_cols = c(collection_date, ADM2_EN),
      names_from = scenario,
      values_from = pop
    ) |>
    mutate(
      diff = baseline - `missing_Ng&G_200`,
      diff_perc = (baseline - `missing_Ng&G_200`) / baseline * 100
    ) |>
    pivot_longer(
      cols = c(diff, diff_perc),
      names_to = "metric",
      values_to = "value"
    ) |>
    mutate(
      metric = case_when(
        metric == "diff" ~ "People",
        metric == "diff_perc" ~ "Percentage"
      )
    )
) +
  geom_line(
    aes(
      x = collection_date,
      y = value,
      color = ADM2_EN
    ),
    linewidth = 1.2
  ) +
  geom_line(
    data = tibble(
      collection_date = unique(pop_scenario$collection_date),
      metric = "Percentage",
      value = -100
    ),
    aes(
      x = collection_date,
      y = value
    ),
    linetype = "dashed"
  ) +
  geom_line(
    data = tibble(
      collection_date = unique(pop_scenario$collection_date),
      metric = "Percentage",
      value = 100
    ),
    aes(
      x = collection_date,
      y = value
    ),
    linetype = "dashed"
  ) +
  theme_minimal() +
  labs(
    title = "Impact on population estimates of missing users in North Gaza and Gaza",
    subtitle = "The population estimates are compared to baseline scenario",
    y = "Difference with baseline population",
    x = '',
    color = "Governorate"
  ) +
  scale_color_manual(values = col_governorate) +
  facet_wrap(. ~ metric, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      metric == "Percentage" ~ scale_y_continuous(
        limits = c(-105, 105),
        labels = scales::percent_format(scale = 1)
      )
    )
  ) +
  geom_text(
    data = annotations,
    aes(x = x, y = y, label = label),
    color = "black",
    size = 4
  ) +
  geom_abline(
    intercept = 0,
    slope = 0,
    color = "grey50"
  ) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12)
  )


# Assess sensitivity to missing users in Khan Younis starting in Feb ------------------------------------------------------------

ggplot(
  pop_scenario |>
    filter(scenario %in% c('baseline', 'missing_K_200')) |>
    filter(agesex == 'T_18Plus') |>
    #mutate(pop = ifelse(mau_lower <= 500, NA, pop)) |>
    mutate(
      scenario = case_when(
        scenario == 'baseline' ~ 'Baseline',
        scenario ==
          'missing_K_200' ~ 'Missing half users\nin Khan Younis (from Feb 2023)'
      )
    )
) +
  geom_line(aes(
    x = collection_date,
    y = mau_lower,
    color = ADM2_EN,
    linetype = scenario
  )) +
  theme_minimal() +
  labs(
    title = "Impact on governorate population estimates due to missing users in Khan Yunis",
    x = "",
    y = "Number of People",
    color = "Governorate",
    linetype = "Scenario"
  ) +
  scale_color_manual(values = col_governorate) +
  theme(
    panel.background = element_rect(
      fill = "grey97",
      color = "grey97"
    )
  )

annotations <- tibble(
  x = rep(as.Date(c('2024-02-20')), 2),
  y = c(105, -105),
  label = c(
    "Double of governorate population",
    "Half of governorate population"
  ),
  metric = c("Percentage", "Percentage")
)

ggplot(
  pop_scenario |>
    filter(scenario %in% c('baseline', 'missing_K_200')) |>
    filter(agesex == 'T_18Plus') |>
    filter(collection_date > as.Date('2023-10-17')) |>
    pivot_wider(
      id_cols = c(collection_date, ADM2_EN),
      names_from = scenario,
      values_from = pop
    ) |>
    mutate(
      diff = baseline - `missing_K_200`,
      diff_perc = (baseline - `missing_K_200`) / baseline * 100
    ) |>
    pivot_longer(
      cols = c(diff, diff_perc),
      names_to = "metric",
      values_to = "value"
    ) |>
    mutate(
      metric = case_when(
        metric == "diff" ~ "People",
        metric == "diff_perc" ~ "Percentage"
      )
    )
) +
  geom_line(
    aes(
      x = collection_date,
      y = value,
      color = ADM2_EN
    ),
    linewidth = 1.2
  ) +
  geom_line(
    data = tibble(
      collection_date = unique(pop_scenario$collection_date),
      metric = "Percentage",
      value = -100
    ),
    aes(
      x = collection_date,
      y = value
    ),
    linetype = "dashed"
  ) +
  geom_line(
    data = tibble(
      collection_date = unique(pop_scenario$collection_date),
      metric = "Percentage",
      value = 100
    ),
    aes(
      x = collection_date,
      y = value
    ),
    linetype = "dashed"
  ) +
  theme_minimal() +
  labs(
    title = "Impact on population estimates of missing half of the users in Khan Younis",
    subtitle = "The population estimates are compared to baseline scenario",
    y = "Difference with baseline population",
    x = '',
    color = "Governorate"
  ) +
  scale_color_manual(values = col_governorate) +
  facet_wrap(. ~ metric, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      metric == "Percentage" ~ scale_y_continuous(
        limits = c(-105, 105),
        labels = scales::percent_format(scale = 1)
      )
    )
  ) +
  geom_text(
    data = annotations,
    aes(x = x, y = y, label = label),
    color = "black",
    size = 4
  ) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

# Assess sensitivity to network connectivity ------------------------------------------------------------

connectivity <- read_csv(file.path(
  'data',
  paste0('netblock_gaza.csv')
)) |>
  mutate(
    date = as.Date(date, format = "%d/%m/%Y"),
    location = factor(
      location |> str_remove(" Governorate"),
      levels = c(
        "Gaza",
        "Deir al-Balah",
        "Khan Yunis",
        "Rafah"
      )
    )
  ) |>
  filter(!is.na(location))

ggplot(connectivity) +
  geom_line(aes(
    x = date,
    y = network_connectivity,
    color = location
  )) +
  theme_minimal() +
  labs(
    title = "Evolution of the network connectivity in Gaza",
    x = "",
    y = "Network Connectivity Index",
    color = "Governorate",
    caption = "Source: Netblock.org.\nLocations correspond to Netblock's definition of governorates."
  ) +
  scale_color_manual(values = col_governorate[-1]) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

# Compare penetration rate at national level -----

ict_damage <- read_csv(file.path(
  'data',
  paste0('worldbank_ict_damages.csv')
)) |>
  mutate(
    collection_date = as.Date(collection_date, format = "%d/%m/%y")
  ) |>
  mutate(
    ict_remain = (100 - ict_damage * 100) / 10
  )

pop_gaza_penRate <- pop_gaza |>
  group_by(collection_date) |>
  summarise(
    mau_lower = sum(mau_lower),
    penrate = sum(mau_lower) / sum(pop_mau_lower) * 100
  )

ggplot(
  pop_gaza_penRate,
  aes(x = collection_date, y = penrate)
) +
  geom_line(color = 'orange', linewidth = 1.5) +
  geom_line(
    data = ict_damage,
    aes(x = collection_date, y = ict_remain),
    color = 'black',
    linetype = 'dashed',
    linewidth = 1.5
  ) +
  scale_y_continuous(
    "Penetration Rate",
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(
      ~ . * 10,
      name = "ICT Network Remaining",
      labels = scales::percent_format(scale = 1)
    )
  ) +
  theme_minimal() +
  labs(
    title = "Evolution of the Facebook user population compared to ICT network availability",
    x = "",
    y = "Penetration Rate",
    caption = "Black dashed line indicates estimated remaining ICT network (%) from World Bank data."
  ) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12)
  )
