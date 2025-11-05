# cleanup
rm(list = ls())
gc()
cat("\014")
try(dev.off(), silent = T)
options(scipen = 999)


env <- new.env()
source(".env", local = env)
source("R/toolbox.R")

# Set global variables ---------------------------------------------------

# directories
country <- "gaza"
outdir <- file.path(env$wd, "out", country)

# colors
col_governorate <- c("#001D23", "#457C36", "#83A40E", "#e7d97e", "#ffca79")
col_gender <- c("navajowhite1", "mistyrose3", "lightsalmon")

# labels
phase_label <- c("North Evac.", "Densification", "Nowhere Safe")
phase_date <- c("13 Oct-24 Nov", "24 Nov-22 Jan", "23 Jan-14 May")

# scenario
scenario_name <- "base"

# Load data --------------------------------------------------------------

pop_gaza <- read_csv(file.path(
  'data',
  paste0('population_', country, '_', scenario_name, '.csv')
))

unrwa <- read_csv(
  file.path(
    "data",
    "unrwa_idp_counts.csv"
  )
)

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
  select(-pop_mau_lower) |>
  ungroup()

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

phase_df <- pop_gaza |>
  distinct(collection_date, phase, phase_date, ADM2_EN) |>
  group_by(phase, phase_date, ADM2_EN) |>
  summarise(collection_date = min(collection_date))

# 1. Magnitude and speed  of displacement in Gaza ------------------------

events <- tibble(
  date = c(
    "2023-10-13",
    "2023-10-27",
    "2023-11-21",
    "2023-12-04",
    "2024-01-22",
    "2024-02-09",
    "2024-03-06",
    "2024-04-09",
    "2024-05-06"
  ),
  text = c(
    "Gaza\nevac.",
    "North\ninvas.",
    "Truce",
    "Khan Younis\nassault",
    "Al Mawasi\nassault",
    "Planning\nRafah assault",
    "North-South\nmilitary road",
    "Khan Younis\nwithdraw",
    "Rafah\nevac."
  ),
  y = rep(1180000, 9)
) |>
  mutate(
    date = ymd(date)
  )

g_timeline <- ggplot(
  pop_gaza_18plus,
  aes(
    x = collection_date,
    y = pop,
    colour = ADM2_EN
  )
) +
  geom_path(size = 1.2) +
  theme_classic() +
  scale_color_manual(values = col_governorate) +
  scale_x_date(
    date_breaks = "10 days",
    labels = function(z) gsub("^0", "", strftime(z, "%d%b")),
    expand = c(0, 0),
    limits = ymd(c("2023-10-05", "2024-05-16"))
  ) +
  labs(x = "", color = "Governorate", y = "") +
  theme(
    panel.background = element_rect(
      fill = "grey98",
      size = 0.5,
      linetype = "solid"
    ),
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 14), # change font size of legend text
    legend.title = element_text(size = 17),
    legend.position = "right",
    legend.justification = "top",
    legend.key.spacing.y = unit(0.15, "cm")
  ) +
  scale_y_continuous(
    labels = label_number(),
    limits = c(0, 1220000),
    expand = c(0, 0)
  ) +
  geom_vline(
    data = events,
    aes(xintercept = date),
    color = "grey70",
    linetype = 2
  ) +
  geom_text(
    data = events,
    aes(x = date + 0.5, y = y, label = text),
    colour = "grey50",
    hjust = 0,
    size = 5
  )

g_timeline

ggplot2::ggsave(
  plot = g_timeline,
  filename = "./docs/pic/timeline.png",
  width = 40,
  height = 20,
  units = "cm"
)

# Stats

# displacement during the first stage
pop_gaza_18plus |>
  filter(collection_date == "2023-10-14" | collection_date == "2023-11-20") |>
  select(ADM2_EN, collection_date, pop) |>
  group_by(ADM2_EN) |>
  mutate(
    pop_diff = pop - lag(pop),
    pop_diff_perc = pop_diff / lag(pop) * 100
  )

# displacement during the truce
pop_gaza_18plus |>
  filter(
    collection_date <= "2023-12-01" &
      collection_date >= "2023-11-19" &
      ADM2_EN %in% c("North Gaza", "Gaza")
  ) |>
  select(ADM2_EN, collection_date, pop) |>
  group_by(ADM2_EN) |>
  mutate(
    pop_baseline = pop[collection_date == "2023-11-20"],
    pop_diff = pop - pop_baseline,
    pop_diff_perc = pop_diff / pop_baseline * 100
  ) |>
  View()

# displacement in Khan Younis
pop_gaza_18plus |>
  filter(
    ADM2_EN %in%
      c("Khan Younis", "Rafah") &
      collection_date %in%
        c("2023-10-14", "2023-11-28", "2024-01-20", "2024-01-22", "2024-02-07")
  ) |>
  select(ADM2_EN, collection_date, pop) |>
  group_by(ADM2_EN) |>
  mutate(
    pop_baseline = pop[collection_date == "2023-10-14"],
    pop_diff = pop - pop_baseline,
    pop_diff_perc = pop_diff / pop_baseline * 100,
    pop_diff_times = pop / pop_baseline
  )

# displacement in Rafah around the peak
pop_gaza_18plus |>
  filter(
    ADM2_EN %in%
      c("Rafah") &
      collection_date >= as.Date("2024-01-09") &
      collection_date <= as.Date("2024-04-20")
  ) |>
  select(ADM2_EN, collection_date, pop) |>
  mutate(
    pop_ref = pop[collection_date == "2024-02-07"],
    date_gap = abs(as.numeric(collection_date - as.Date("2024-02-07"))),
    pop_diff = abs(pop - pop_ref)
  ) |>
  filter(pop_diff > 193000 & pop_diff < 2010000)


# 2. Comparison with UNRWA --------------------------------------------------
unrwa <- unrwa |>
  mutate(
    displacement_adulte = displacement * 0.426821927
  )

pop_gaza_displacement <- pop_gaza |>
  filter(grepl("18Plus", agesex)) |>
  left_join(
    pop_gaza |>
      filter(collection_date == "2023-10-14") |>
      mutate(pop_baseline = pop) |>
      select(ADM2_PCODE, agesex, pop_baseline)
  ) |>
  group_by(ADM2_EN, agesex) |>
  mutate(
    pop_diff = (pop - pop_baseline),
    pop_diffperc = (pop - pop_baseline) / pop_baseline,
    pop_change_pos = pop_diff > 0,
    pop_change_neg = pop_diff < 0
  )

pop_gaza_displacement_T <- pop_gaza_displacement |>
  filter(agesex == "T_18Plus" & pop_diff > 0) |>
  group_by(collection_date, phase) |>
  summarise(displacement = sum(pop_diff))

unrwa_end <- unrwa |>
  group_by(shelter) |>
  filter(collection_date == max(collection_date))

rec_unrwa <- phase_df |>
  distinct(collection_date, phase) |>
  mutate(
    xmin = collection_date,
    ymin = -Inf,
    ymax = Inf
  )
rec_unrwa$xmax <- c(unique(phase_df$collection_date)[2:3], Inf)
rec_unrwa$col <- paste0("gray", c(75, 85, 95))
rec_unrwa$xmin[1] <- min(unrwa$collection_date)

g_displacement <- ggplot(
  pop_gaza_displacement_T,
  aes(x = collection_date, y = displacement, colour = "Estimated")
) +
  geom_rect(
    data = rec_unrwa,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = col),
    alpha = 0.5,
    inherit.aes = F
  ) +
  geom_path(size = 2) +
  theme_minimal() +
  geom_step(
    data = unrwa,
    aes(x = collection_date, y = displacement_adulte, colour = shelter),
    linetype = 2,
    alpha = 0.5,
    size = 1
  ) +
  geom_point(
    data = unrwa_end,
    aes(
      x = collection_date,
      y = displacement_adulte,
      colour = shelter,
      shape = "last reported"
    ),
    size = 8
  ) +
  geom_point(
    data = unrwa,
    aes(x = collection_date, y = displacement_adulte, colour = shelter),
    size = 4
  ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ")) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b %y") +
  scale_color_manual(values = c("#8D8A00", "gold3", "#325B29", "gold1")) +
  scale_fill_manual(values = rec_unrwa$col) +
  scale_shape_manual(values = c(13)) +
  labs(x = "", y = "Displaced adults", colour = "Data source", shape = "") +
  theme(
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16), # change font size of legend text
    legend.title = element_text(size = 19),
    axis.title.y = element_text(size = 19, margin = unit(c(0, 5, 0, 0), "mm"))
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 5)),
    shape = guide_legend(order = 2, override.aes = list(size = 6)),
    fill = "none"
  )

g_displacement

ggplot2::ggsave(
  plot = g_displacement,
  filename = "./docs/pic/compUNRWA.png",
  width = 30,
  height = 20,
  units = "cm"
)

# 3. Displacement by gender ----------------------------------------------

pop_gaza_sex <- pop_gaza |>
  filter(grepl("18Plus", agesex) & !grepl("T", agesex)) |>
  left_join(
    pop_gaza |>
      filter(collection_date == "2023-10-14") |>
      mutate(pop_baseline = pop) |>
      select(ADM2_PCODE, agesex, pop_baseline)
  ) |>
  group_by(ADM2_PCODE, agesex) |>
  mutate(
    pop_diff = (pop - pop_baseline) / pop_baseline
  ) |>
  group_by(ADM2_PCODE, collection_date) |>
  mutate(
    reliable = ifelse(all(!is.na(pop)), T, F)
  ) |> # check if both gender have reliable data
  ungroup() |>
  mutate(
    pop_diff = ifelse(reliable, pop_diff, NA)
  )

# Group the data and apply the function to each group
distances <- pop_gaza_sex |>
  group_by(ADM2_EN) %>%
  group_modify(~ process_group(.x, type_col = "sex")) |>
  mutate(dist_date = ifelse(dist_date_abs > 30, NA, dist_date)) # remove time gaps above one month


# Visualise an example
distances_example <- distances |>
  filter(dist_date_abs < 30) |>
  ungroup() |>
  arrange(ADM2_EN, collection_date) |>
  filter(dist_date_abs > 2) |>
  slice(c(4, 70, 114, 220))

# Side-by-side gendered evolution + examples of distance computation
p_gender <- ggplot(distances_example) +
  geom_line(
    data = pop_gaza_sex |> filter(sex == "F"),
    linewidth = 1.5,
    aes(x = collection_date, y = pop_diff),
    colour = "lightsalmon"
  ) +
  geom_line(
    data = pop_gaza_sex |> filter(sex == "M"),
    size = 1.5,
    aes(x = collection_date, y = pop_diff),
    colour = "mistyrose3"
  ) +
  facet_grid(ADM2_EN ~ ., scales = "free") +
  geom_segment(
    aes(x = date_ref, xend = ymd(collection_date), y = pop_ref, yend = pop_ref),
    arrow = arrow(length = unit(0.10, "cm"), ends = "both", type = "closed"),
    size = 0.5
  ) +
  geom_point(aes(x = collection_date, y = pop_ref), size = 0.3) +
  geom_text(
    aes(
      x = collection_date + dist_date / 2,
      y = pop_ref,
      label = paste0(dist_date_abs, " days")
    ),
    size = 6,
    vjust = 1.3
  ) +
  theme_minimal() +
  labs(y = "Displaced population (in%)", x = "") +
  scale_y_continuous(labels = label_percent()) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b%y") +
  theme(
    strip.text.y.right = element_text(size = 19, angle = 0),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 25),
    legend.position = "None"
  )
p_gender

ggplot2::ggsave(
  plot = p_gender,
  filename = "./docs/pic/dispGender_example.png",
  width = 28,
  height = 23,
  units = "cm"
)

# Plotting time gap

gap_days <- distances |>
  ungroup() |>
  complete(ADM2_EN, date_ref) |>
  select(-collection_date) |>
  mutate(
    collection_date = date_ref,
    phase = factor(
      case_when(
        collection_date < ymd("2023-11-24") ~ phase_label[1],
        collection_date > ymd("2024-01-22") ~ phase_label[3],
        T ~ phase_label[2]
      ),
      levels = c(phase_label[1], phase_label[2], phase_label[3])
    ),
    ADM2_EN = factor(
      ADM2_EN,
      levels = c("North Gaza", "Gaza", "Deir Al-Balah", "Khan Younis", "Rafah")
    ),
    gender = case_when(
      dist_date > 0 ~ "Women",
      dist_date < 0 ~ "Men",
      T ~ "Together"
    ),
    gender = factor(gender, levels = c("Together", "Men", "Women"))
  ) |>
  arrange(ADM2_EN, date_ref)

rec <- gap_days |>
  group_by(ADM2_EN, phase) |>
  reframe(
    xmin = min(date_ref),
    xmax = max(date_ref),
    ymin = c(-Inf, 0),
    ymax = c(0, Inf),
    sex = c("Men", "Women"),
    alpha = c(0.01, 0.006)
  )

gapLine_p <- ggplot(
  gap_days,
  aes(y = dist_date, x = collection_date)
) +
  geom_line(size = 1) +
  facet_grid(ADM2_EN ~ phase, scales = "free_x", space = "free_x") +
  geom_rect(
    data = rec,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sex),
    alpha = 0.5,
    inherit.aes = F
  ) +
  scale_fill_manual(values = c("mistyrose3", "lightsalmon")) +
  theme_minimal() +
  labs(
    y = "Time gap (days)",
    x = "",
    fill = "First group\nexperiencing\nchange"
  ) +
  scale_y_continuous(position = "right", labels = abs) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b%y") +
  theme(
    strip.text.x = element_text(size = 19),
    strip.text.y.right = element_blank(),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 19),
  )
gapLine_p

ggplot2::ggsave(
  plot = gapLine_p,
  filename = "./docs/pic/dispGender_line.png",
  width = 30,
  height = 23,
  units = "cm"
)


# Stats

# summary of time gaps by phase
gap_days |>
  filter(!is.na(dist_date)) |>
  mutate(
    lead = ifelse(dist_date > 0, "F", "M"),
    lead = ifelse(dist_date == 0, "T", lead)
  ) |>
  group_by(phase, lead) |>
  summarise(n(), mean(abs(dist_date))) |>
  ungroup() |>
  group_by(phase) |>
  mutate(total = sum(`n()`), perc = `n()` / total * 100)

# focus on the nowhere safe phase in Rafah

gap_days |>
  filter(!is.na(dist_date)) |>
  filter(phase == "Nowhere Safe") |>
  filter(ADM2_EN == "Rafah") |>
  mutate(
    after_turn = ifelse(collection_date > ymd("2024-04-25"), T, F),
    lead = ifelse(dist_date > 0, "F", "M"),
    lead = ifelse(dist_date == 0, "T", lead)
  ) |>
  group_by(after_turn, lead) |>
  summarise(n(), mean(abs(dist_date))) |>
  ungroup() |>
  group_by(after_turn) |>
  mutate(total = sum(`n()`), perc = `n()` / total * 100)
