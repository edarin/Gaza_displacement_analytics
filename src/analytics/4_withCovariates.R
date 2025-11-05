# cleanup
rm(list = ls())
gc()
cat("\014")
try(dev.off(), silent = T)
options(scipen = 999)


env <- new.env()
source(".env", local = env)
source("R/toolbox.R")


# labels
phase_label <- c("North Evac.", "Densification", "Nowhere Safe")
phase_date <- c("13 Oct-24 Nov", "24 Nov-22 Jan", "23 Jan-14 May")

# data
pop_gaza <- read_csv(file.path(
  "data",
  "population_gaza_base.csv"
))

cov <- read_csv(file.path(
  "data",
  "covariates.csv"
))

# Preprocess population data ----------------------------------------------
pop_gaza <- pop_gaza |>
  arrange(collection_date) |>
  group_by(ADM2_EN, agesex) |>
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
    sex = str_sub(agesex, 1, 1)
  ) |>
  select(-pop_mau_lower)

pop_gaza <- pop_gaza |>
  mutate(
    pop = ifelse(mau_lower < 1000, NA, pop),
    ADM2_EN = factor(
      ADM2_EN,
      levels = c("North Gaza", "Gaza", "Deir Al-Balah", "Khan Younis", "Rafah")
    ),
    phase = factor(
      case_when(
        collection_date < ymd("2023-11-24") ~ phase_label[1],
        collection_date > ymd("2024-01-22") ~ phase_label[3],
        T ~ phase_label[2]
      ),
      levels = c(phase_label[1], phase_label[2], phase_label[3])
    ),
    phase_date = factor(
      case_when(
        collection_date < ymd("2023-11-24") ~ phase_date[1],
        collection_date > ymd("2024-01-22") ~ phase_date[3],
        T ~ phase_date[2]
      ),
      levels = c(phase_date[1], phase_date[2], phase_date[3])
    )
  )

pop_gaza_18plus <- pop_gaza |>
  filter(agesex == "T_18Plus")


# Preprocess covariates data  -------------------------------------------------------------

# Cumulative covariate
cov_df_sum <- bind_cols(
  cov,
  future_lapply(c(7, 14, 30), function(w) compute_rollMetric(cov, sum, w))
)

cov_df_sum <- cov_df_sum |>
  pivot_longer(
    c(contains("days"), "raw"),
    names_to = "sum_stat",
    values_to = "value"
  )

# Time scaling
cov_df_mean <- bind_cols(
  cov,
  future_lapply(c(7, 14, 30), function(w) compute_rollMetric(cov, mean, w))
)
cov_df_mean <- cov_df_mean |>
  pivot_longer(
    contains("days"),
    names_to = "method",
    values_to = "center_std"
  ) |>
  mutate(value = raw) |>
  separate(method, into = c("drop", "window_std"), sep = "_", fill = "right") |>
  select(-raw, -drop) |>
  mutate(sum_stat = "raw")

# Geographical scaling
cov_df_geo <- cov |>
  group_by(variable, t) |>
  mutate(
    center_std = mean(raw, na.rm = T),
    window_std = "spatial",
    sum_stat = "raw"
  ) |>
  rename(value = raw)

# Combine processed covariates
cov_df_scaled <- bind_rows(cov_df_sum, cov_df_mean, cov_df_geo) |>
  group_by(variable, sum_stat, window_std) |>
  mutate(
    center_std = ifelse(is.na(center_std), mean(value, na.rm = T), center_std),
    scale_std = sd(value, na.rm = T),
    value_std = (value - center_std) / scale_std
  ) |>
  select(
    ADM2_EN,
    ADM2_PCODE,
    t,
    collection_date,
    variable,
    sum_stat,
    window_std,
    center_std,
    scale_std,
    value,
    value_std
  )

# compute geographical scaling --------------------------------------------

cov_df_geo <- cov |>
  group_by(variable, t) |>
  mutate(
    center_std = mean(raw, na.rm = T),
    window_std = "spatial",
    sum_stat = "raw"
  ) |>
  rename(value = raw)


# bind scaling together -------------------------------------------
cov_df_scaled <- bind_rows(cov_df_sum, cov_df_mean, cov_df_geo)

cov_df_scaled <- cov_df_scaled |>
  group_by(variable, sum_stat, window_std) |>
  mutate(
    center_std = ifelse(is.na(center_std), mean(value, na.rm = T), center_std),
    scale_std = sd(value, na.rm = T),
    value_std = (value - center_std) / scale_std
  )

cov_df_scaled <- cov_df_scaled |>
  select(
    ADM2_EN,
    ADM2_PCODE,
    t,
    collection_date,
    variable,
    sum_stat,
    window_std,
    center_std,
    scale_std,
    value,
    value_std
  )

# Compare cov to change in pop

n_lags <- 5

cov_pop <- cov_df_scaled |>
  mutate(
    scale_label = paste0(sum_stat, ", ", window_std),
    cov_process = paste0(variable, ",", scale_label),
    process_type = case_when(
      grepl("sum", sum_stat) ~ "Cumulative covariate",
      grepl("days", window_std) ~ "Temporal Scaling",
      grepl("spatial", window_std) ~ "Geographical Scaling",
      T ~ "Raw covariate"
    ) |>
      factor(
        levels = c(
          "Raw covariate",
          "Cumulative covariate",
          "Temporal Scaling",
          "Geographical Scaling"
        )
      )
  ) |>
  select(
    ADM2_EN,
    t,
    collection_date,
    variable,
    process_type,
    scale_label,
    cov_process,
    value_std
  ) |>
  right_join(
    pop_gaza_18plus |>
      mutate(pop_change = pop - lag(pop)) |>
      filter(!is.na(pop_change)) |>
      select(ADM2_EN, collection_date, phase, phase_date, pop_change)
  ) |>
  filter(!(variable == "pwtt" & phase == phase_label[1])) |>
  filter(!(phase == phase_label[1] & grepl("30days", scale_label)))

df_corr_pos <- compute_ccf(
  cov_pop |> filter(pop_change > 0),
  n_lags,
  "positive"
)
df_corr_neg <- compute_ccf(
  cov_pop |> filter(pop_change < 0) |> mutate(pop_change = abs(pop_change)),
  n_lags,
  "negative"
)

# Create cross-correlation table

palette_name <- "rcartocolor::Geyser"

df_corr_pos_w <- df_corr_pos |>
  pivot_wider(
    names_from = phase_date,
    values_from = `-5`:`5`,
    names_glue = "{phase_date}_{.value}"
  ) |>
  select(
    process_type,
    variable,
    scale_label,
    change_type,
    starts_with(phase_date[1]),
    starts_with(phase_date[2]),
    starts_with(phase_date[3])
  )

df_corr_pos_gt <- prepare_gt(df_corr_pos_w, signif_thresh = 35)
df_corr_pos_gt
gtsave(df_corr_pos_gt, paste0("./docs/pic/correlation_ingoing.png"))

df_corr_neg_w <- df_corr_neg |>
  pivot_wider(
    names_from = phase_date,
    values_from = `-5`:`5`,
    names_glue = "{phase_date}_{.value}"
  ) |>
  select(
    process_type,
    variable,
    scale_label,
    change_type,
    starts_with(phase_date[1]),
    starts_with(phase_date[2]),
    starts_with(phase_date[3])
  )
df_corr_neg_gt <- prepare_gt(df_corr_neg_w, signif_thresh = 35)
df_corr_neg_gt

gtsave(df_corr_neg_gt, paste0("./docs/pic/correlation_outgoing.png"))
