# cleanup
rm(list = ls())
gc()
cat("\014")
try(dev.off(), silent = T)
options(scipen = 999)


# working directory
env <- new.env()
source(here::here(".env"), local = env)

# load data ---------------------------------------------------------------

pop_files <- c(list.files(
  'data',
  pattern = "pop_estimates",
  full.names = T,
  recursive = T
))

pop <- bind_rows(
  lapply(
    pop_files,
    read_csv
  )
)

gaza <- read_csv(
  file.path(
    "data",
    "population_gaza_base.csv"
  )
) |>
  filter(agesex == "T_20Plus") |>
  rename(geo_name = ADM2_EN) |>
  select(-ADM2_PCODE) |>
  mutate(country = "gaza")

pop_countries <- bind_rows(pop, gaza) |>
  select(country, geo_name, collection_date, pop_mau_lower)

pop_nat <- pop_countries |>
  group_by(country) |>
  filter(collection_date == min(collection_date)) |>
  summarise(pop_nat = sum(pop_mau_lower))

# create population time series ---------------------------------------

pop_countries <- pop_countries |>
  arrange(geo_name, collection_date) |>
  group_by(geo_name) |>
  tq_mutate(
    select = pop_mau_lower,
    mutate_fun = rollapply,
    width = 2,
    align = "right",
    FUN = mean,
    col_rename = "pop"
  ) |>
  mutate(
    pop = ifelse(is.na(pop), pop_mau_lower, pop)
  ) |>
  group_by(country, geo_name) |>
  arrange(collection_date) |>
  mutate(
    day = rank(collection_date, ties.method = "first"),
  )


# compute displacement population ----------------------------------------

# compute displacement magnitude

pop_baseline <- pop_countries |>
  group_by(country) |>
  filter(day == 1) |>
  mutate(pop_baseline = pop) |>
  select(country, geo_name, pop_baseline)

pop_disp <- pop_countries |>
  filter(day <= 212) |>
  left_join(pop_baseline) |>
  group_by(geo_name) |>
  mutate(
    disp = pop - pop_baseline
  ) |>
  filter(disp >= 0) |>
  group_by(country, day, collection_date) |>
  summarise(
    disp = sum(disp, na.rm = T)
  ) |>
  left_join(pop_nat) |>
  mutate(
    disp_perc = disp / pop_nat * 100
  )

pop_disp_long <- pop_disp |>
  filter(day <= 212) |> # this the length of the Gaza war for which we have data
  select(-pop_nat) |>
  pivot_longer(
    cols = c(disp, disp_perc),
    names_to = "metric",
    values_to = "displacement"
  )

# compute displacement rate
pop_disp_rate <- pop_countries |>
  filter(day <= 212) |>
  arrange(geo_name, collection_date) |>
  group_by(geo_name) |>
  mutate(
    disp_rate = (pop - lag(pop))
  ) |>
  filter(disp_rate >= 0) |>
  group_by(country, day, collection_date) |>
  summarise(
    disp_rate = sum(disp_rate, na.rm = T)
  ) |>
  ungroup() |>
  complete(day, country, fill = list(disp_rate = 0)) |>
  left_join(pop_nat) |>
  mutate(
    disp_rate_perc = disp_rate / pop_nat * 100
  )

pop_disp_rate_long <- pop_disp_rate |>
  select(-pop_nat) |>
  pivot_longer(
    cols = c(disp_rate_perc, disp_rate),
    names_to = "metric",
    values_to = "displacement"
  )

# combine disp and rate
pop_disp_long <- bind_rows(pop_disp_long, pop_disp_rate_long) |>
  mutate(
    metric_type = ifelse(
      grepl("perc", metric),
      "% of country population",
      "Number of people"
    ) |>
      factor(levels = c("Number of people", "% of country population")),
    metric = ifelse(
      grepl("rate", metric),
      "Displacement rate",
      "Displaced population"
    ) |>
      factor(levels = c("Displaced population", "Displacement rate")),
    country = case_when(
      country == "gaza" ~ "Gaza",
      country == "EG" ~ "Egypt",
      country == "JO" ~ "Jordan",
      country == "UA" ~ "Ukraine",
      T ~ country
    ) |>
      factor(levels = c("Gaza", "Ukraine", "Egypt", "Jordan")),
  )

# Plot magnitude
g_magnitude <- ggplot(
  pop_disp_long |> filter(metric == "Displaced population"),
  aes(x = day, y = displacement, colour = country)
) +
  facet_grid(metric_type ~ ., scale = "free") +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(y = "", x = "Day into the war", ) +
  scale_y_continuous(labels = label_number()) +
  scale_color_manual(values = c("#ddda08", "#458f35", "grey70", "grey50")) +
  theme(
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 15)
  ) +
  labs(color = "Country") +
  guides(color = "none")
g_magnitude

ggsave(
  filename = file.path(
    "docs",
    "pic",
    "compAcross_magnitude.png"
  ),
  plot = g_magnitude,
  width = 20,
  height = 30,
  units = "cm"
)

# Plot rate
g_rate <- ggplot(
  pop_disp_long |> filter(metric != "Displaced population"),
  aes(x = day, y = displacement, colour = country)
) +
  facet_grid(metric_type ~ ., scale = "free") +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(y = "", x = "Day into the war", ) +
  scale_y_continuous(labels = label_number()) +
  scale_color_manual(values = c("#ddda08", "#458f35", "grey70", "grey50")) +
  theme(
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    legend.text = element_text(size = 22),
    axis.text = element_text(size = 15),
    legend.key.spacing.y = unit(0.3, "cm")
  ) +
  labs(color = "")
g_rate

ggsave(
  filename = file.path(
    "docs",
    "pic",
    "compAcross_rate.png"
  ),
  plot = g_rate,
  width = 25,
  height = 30,
  units = "cm"
)

# Stats
pop_disp_war <- pop_disp_long |>
  filter(country %in% c("Gaza", "Ukraine")) |>
  select(country, day, metric, metric_type, displacement) |>
  pivot_wider(names_from = country, values_from = displacement) |>
  mutate(
    ratio = Gaza / Ukraine
  )

pop_disp_war |>
  group_by(metric, metric_type) |>
  summarise(max(Ukraine), max(Gaza))

# Ratio of the relative metric
pop_disp_war |>
  filter(is.finite(ratio)) |>
  group_by(metric, metric_type) |>
  summarise(
    mean = mean(ratio, na.rm = T),
    max = max(ratio, na.rm = T),
    median = median(ratio, na.rm = T)
  )

# Duration
pop_disp_long |>
  ungroup() |>
  filter(
    country %in%
      c("Gaza", "Ukraine") &
      metric_type == "% of country population" &
      metric == "Displacement rate" &
      displacement >= 1
  ) |>
  group_by(country) |>
  summarise(n())
