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
    title = "",
    x = "",
    y = "Number of people",
    color = "Governorate",
    linetype = "Scenario"
  ) +
  scale_color_manual(values = col_governorate) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12)
  )
ggsave(
  plot = last_plot(),
  filename = "./docs/pic/timeserie_constant_national.png",
  width = 25,
  height = 15,
  units = "cm"
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
  geom_line(
    aes(
      x = collection_date,
      y = value,
      linetype = scenario
    ),
    color = "#8D8A00",
    linewidth = 1
  ) +
  theme_minimal() +
  facet_grid(metric ~ agesex, scales = "free_y") +
  labs(
    title = "",
    x = "",
    y = "Number of people",
    linetype = "Scenario"
  ) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = 'bottom'
  )

ggsave(
  plot = last_plot(),
  filename = "./docs/pic/displacement_constant_national.png",
  width = 15,
  height = 15,
  units = "cm"
)

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
  pivot_wider(
    id_cols = c(collection_date, agesex),
    names_from = scenario,
    values_from = c(pop_displacement, pop_national)
  ) |>
  mutate(
    diff_displacement = pop_displacement_constant_national -
      pop_displacement_baseline,
    diff_national = pop_national_constant_national - pop_national_baseline,
    perc_displacement = diff_displacement / pop_displacement_baseline * 100,
    perc_national = diff_national / pop_national_baseline * 100
  ) |>
  View()

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
          'missing_Ng&G_200' ~ '50% connectivity drop\nin North Gaza & Gaza'
      ) |>
        factor(
          levels = c(
            'Baseline',
            '50% connectivity drop\nin North Gaza & Gaza'
          )
        ),
      pop = ifelse(mau_lower <= 500, NA, pop)
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
    title = "",
    x = "",
    y = "Number of people",
    color = "Governorate",
    linetype = "Scenario"
  ) +
  scale_color_manual(values = col_governorate, guide = 'none') +
  theme(
    legend.position = 'bottom',
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

ggplot2::ggsave(
  filename = "./docs/pic/timeserie_missing_Ng&G_200.png",
  width = 20,
  height = 15,
  units = "cm"
)

ggplot(
  pop_scenario_displacement |>
    filter(scenario %in% c('baseline', 'missing_Ng&G_200')) |>
    filter(agesex %in% c('T_18Plus')) |>
    mutate(
      scenario = case_when(
        scenario == 'baseline' ~ 'Baseline',
        scenario ==
          'missing_Ng&G_200' ~ '50% connectivity drop\nin North Gaza & Gaza'
      ) |>
        factor(
          levels = c(
            'Baseline',
            '50% connectivity drop\nin North Gaza & Gaza'
          )
        )
    )
) +
  geom_line(
    aes(
      x = collection_date,
      y = pop_displacement,
      linetype = scenario
    ),
    col = "#325B29",
    linewidth = 1
  ) +
  theme_minimal() +
  labs(
    title = "",
    x = "",
    y = "Number of people displaced",
    linetype = "Scenario"
  ) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = 'bottom'
  )

ggplot2::ggsave(
  filename = "./docs/pic/displacement_missing_Ng&G_200.png",
  width = 15,
  height = 15,
  units = "cm"
)
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
      diff_perc = (`missing_Ng&G_200` - baseline) / baseline * 100
    )
) +
  geom_line(
    aes(
      x = collection_date,
      y = diff_perc,
      color = ADM2_EN
    ),
    linewidth = 1.2
  ) +
  geom_line(
    data = tibble(
      collection_date = unique(pop_scenario$collection_date),
      diff_perc = -100
    ),
    aes(
      x = collection_date,
      y = diff_perc
    ),
    linetype = "dashed"
  ) +
  geom_line(
    data = tibble(
      collection_date = unique(pop_scenario$collection_date),
      diff_perc = 100
    ),
    aes(
      x = collection_date,
      y = diff_perc
    ),
    linetype = "dashed"
  ) +
  theme_minimal() +
  labs(
    title = "",
    y = "Difference with baseline population",
    x = '',
    color = "Governorate"
  ) +
  scale_color_manual(values = col_governorate) +
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
  scale_y_continuous(
    labels = scales::percent_format(scale = 1)
  ) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

ggsave(
  plot = last_plot(),
  filename = "./docs/pic/sensitivity_missing_Ng&G_200.png",
  width = 15,
  height = 15,
  units = "cm"
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

g_khanyounis <- ggplot(
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
      diff = `missing_K_200` - baseline,
      diff_perc = diff / baseline * 100
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
    title = "",
    caption = "",
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

g_khanyounis
ggplot2::ggsave(
  plot = g_khanyounis,
  filename = "./docs/pic/sensitivity_missing_K_200.png",
  width = 25,
  height = 15,
  units = "cm"
)

# timeseries of khan younis population
ggplot(
  pop_scenario |>
    filter(scenario %in% c('baseline', 'missing_K_200')) |>
    filter(agesex == 'T_18Plus') |>
    #filter(ADM2_EN == 'Khan Younis') |>
    mutate(
      scenario = case_when(
        scenario == 'baseline' ~ 'Baseline',
        scenario ==
          'missing_K_200' ~ '50% drop of connectivity \nin Khan Younis (from Feb 2023)'
      ),
      pop = ifelse(mau_lower <= 500, NA, pop)
    )
) +
  geom_line(
    aes(
      x = collection_date,
      y = pop,
      color = ADM2_EN,
      linetype = scenario |>
        factor(
          levels = c(
            'Baseline',
            '50% drop of connectivity \nin Khan Younis (from Feb 2023)'
          )
        )
    ),
    linewidth = 1.2
  ) +
  theme_minimal() +
  labs(
    title = "",
    x = "",
    y = "Number of people",
    linetype = "Scenario"
  ) +
  scale_color_manual(values = col_governorate, guide = 'none') +
  theme(
    legend.position = 'bottom',
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

ggplot2::ggsave(
  filename = "./docs/pic/timeserie_missing_K_200.png",
  width = 20,
  height = 15,
  units = "cm"
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
  geom_line(
    aes(
      x = date,
      y = network_connectivity,
      color = location
    ),
    size = 1
  ) +
  theme_minimal() +
  labs(
    title = "",
    x = "",
    y = "Network connectivity index",
    color = "Governorate",
    caption = "Source: Netblock.org.\nLocations correspond to Netblock's definition of governorates."
  ) +
  scale_color_manual(values = col_governorate[-1]) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 10),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

ggplot2::ggsave(
  filename = "./docs/pic/network_connectivity.png",
  width = 20,
  height = 15,
  units = "cm"
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

cols <- c(
  "Estimated penetration rate" = "#8D8A00",
  "World Bank ICT network remaining" = "Grey30"
)
g_penRate <- ggplot(
  pop_gaza_penRate |>
    filter(collection_date <= max(ict_damage$collection_date)),
  aes(x = collection_date, y = penrate)
) +
  geom_line(aes(color = "Estimated penetration rate"), linewidth = 1.5) +
  geom_line(
    data = ict_damage,
    aes(
      x = collection_date,
      y = ict_remain,
      color = 'World Bank ICT network remaining'
    ),
    linetype = 'dashed',
    linewidth = 1.5
  ) +
  scale_y_continuous(
    "Penetration rate",
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(
      ~ . * 10,
      name = "ICT network remaining",
      labels = scales::percent_format(scale = 1)
    )
  ) +
  theme_minimal() +
  labs(
    title = "",
    x = ""
  ) +
  theme(
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16), # change font size of legend text
    legend.title = element_text(size = 19),
    axis.title.y = element_text(size = 19, margin = unit(c(0, 5, 0, 0), "mm")),
    plot.caption = element_text(size = 14),
    legend.position = 'bottom'
  ) +
  scale_color_manual(name = '', values = cols)
g_penRate
ggplot2::ggsave(
  plot = g_penRate,
  filename = "./docs/pic/compWorldBank.png",
  width = 20,
  height = 20,
  units = "cm"
)

# Assess sensitivity to location-specific baseline penetration rates ------------------------------------------------------------

rec_phase <- tibble(
  phase_date = c("13 Oct-24 Nov", "24 Nov-22 Jan", "23 Jan-14 May"),
  phase_label = c("North Evacuation", "Densification", "Nowhere Safe"),
  xmin = as.Date(c("2023-10-13", "2023-11-24", "2024-01-23")),
  xmax = as.Date(c("2023-11-24", "2024-01-23", Inf)),
  ymin = -Inf,
  ymax = Inf,
  fill = paste0("gray", c(75, 85, 95))
)

pop_penRate_comp <- pop_scenario |>
  filter(scenario %in% c('baseline', 'penRate_baseline')) |>
  filter(agesex == 'T_18Plus') |>
  mutate(
    scenario = case_when(
      scenario == 'baseline' ~ 'No baseline adjustment',
      scenario ==
        'penRate_baseline' ~ 'Governorate-specific\nbaseline adjustment'
    ) |>
      factor(
        levels = c(
          'No baseline adjustment',
          'Governorate-specific\nbaseline adjustment'
        )
      ),
    pop = ifelse(mau_lower <= 500, NA, pop)
  )

g_timeserie_penRate_baseline <- ggplot(pop_penRate_comp) +
  geom_line(
    aes(
      x = collection_date,
      y = pop,
      color = ADM2_EN,
      linetype = scenario
    ),
    linewidth = 1
  ) +
  geom_rect(
    data = rec_phase,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = phase_label |> fct_reorder(xmin)
    ),
    alpha = 0.4
  ) +
  scale_fill_manual(values = c("gray75", "gray85", "gray95")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal() +
  labs(
    title = "",
    x = "",
    y = "Number of people",
    color = "Governorate",
    linetype = "Scenario",
    fill = 'Phase'
  ) +
  scale_color_manual(values = col_governorate) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  ) +
  guides(
    fill = guide_legend(order = 2),
    color = guide_legend(order = 3),
    linetype = guide_legend(order = 1)
  )
g_timeserie_penRate_baseline

ggsave(
  plot = g_timeserie_penRate_baseline,
  filename = "./docs/pic/timeserie_penRate_baseline.png",
  width = 25,
  height = 15,
  units = "cm"
)


# plot baseline penetration rates by governorate
penRate_baseline <- pop_scenario |>
  filter(scenario %in% c('penRate_baseline')) |>
  filter(agesex == 'T_18Plus') |>
  filter(collection_date == min(collection_date)) |>
  mutate(
    penrate = mau_lower / pop * 100
  )
g_penRate_baseline <- penRate_baseline |>
  ggplot() +
  geom_point(
    aes(
      y = ADM2_EN |> fct_reorder(penrate),
      x = penrate,
      color = ADM2_EN
    ),
    size = 5
  ) +
  geom_vline(
    xintercept = mean(penRate_baseline$penrate),
    colour = 'grey30',
    linetype = 'dashed'
  ) +
  scale_color_manual(values = col_governorate) +
  theme_minimal() +
  labs(
    title = "",
    y = "",
    x = "Basline penetration rate"
  ) +
  guides(color = "none") +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  annotate(
    "text",
    x = mean(penRate_baseline$penrate) + 1.5,
    y = 5.5,
    label = "Mean penetration rate",
    color = 'grey30',
    fontface = 'italic'
  )


g_penRate_baseline

ggplot2::ggsave(
  plot = g_penRate_baseline,
  filename = "./docs/pic/penRate_baseline.png",
  width = 17,
  height = 17,
  units = "cm"
)


pop_penRate_comp |>
  pivot_wider(
    id_cols = c(collection_date, ADM2_EN),
    names_from = scenario,
    values_from = pop
  ) |>
  mutate(
    phase = case_when(
      collection_date >= as.Date('2023-10-13') &
        collection_date < as.Date('2023-11-24') ~ 'North Evacuation',
      collection_date >= as.Date('2023-11-24') &
        collection_date < as.Date('2024-01-23') ~ 'Densification',
      collection_date >= as.Date('2024-01-23') ~ 'Nowhere Safe'
    ),
    ratio = (`Governorate-specific\nbaseline adjustment` -
      `No baseline adjustment`) /
      `No baseline adjustment` *
      100
  ) |>
  group_by(ADM2_EN, phase) |>
  summarise(
    mean_ratio = mean(ratio, na.rm = T)
  )

penRate_baseline |>
  ungroup() |>
  mutate(
    mean = mean(penrate),
    diff = (penrate - mean) / mean * 100
  )
