library(tidyverse)
library(tidyquant)
library(future.apply)
library(gt)
library(scales)
library(webshot2)

# Compute time gap between subgroup displacement -------------------------

# Function to generate a sequence of values between two population differences (pop_diff)
# in either ascending or descending order, based on the direction determined by the 'from' and 'to' parameters.
seq_bidirectional <- function(from, to, by) {
  if (from < to) {
    # If the starting population difference (from) is less than the ending one (to), create a sequence in ascending order.
    return(seq(from, to, by))
  } else {
    # If the starting population difference (from) is greater than or equal to the ending one (to), create a sequence in descending order.
    return(seq(from, to, -by))
  }
}

# Define the function to compute the shortest time gap (horizontal distance)
# between the displacement of two demographic subgroups (based on population difference and date).
find_shortest_distance <- function(pop_ref, date_ref, df_comp) {
  # Calculate the distances between the reference population and population differences,
  # as well as the distances between the reference date and the collection date.
  closest <- df_comp |>
    mutate(
      dist_pop = abs(pop_ref - pop_diff), # Calculate the absolute distance between the reference population difference (pop_ref) and the current population difference
      dist_date_abs = abs(date_ref - collection_date), # Absolute distance between the reference date and the collection date
      dist_date = collection_date - date_ref # Signed distance to determine which subgroup displacement occurred first (positive means reference displacement happened first)
    ) |>
    filter(dist_pop < 0.01) |> # Filter to keep rows where the population difference is very close (within 0.01)
    filter(row_number(dist_date_abs) == 1) |> # Select only the row with the closest collection date
    mutate(
      pop_ref = pop_ref,
      date_ref = date_ref
    ) # Keep the reference values for output consistency

  return(closest) # Return the closest match based on population difference and date
}

# Define the function to process the data for each demographic subgroup
# and compute the time gap between the displacement of two subgroups (e.g., based on sex).
process_group <- function(df, type_col = "sex") {
  # Select relevant columns for processing: administrative code, subgroup type, collection date, and population difference
  df <- df |>
    select(ADM2_PCODE, !!sym(type_col), collection_date, pop_diff)

  # Separate out rows with missing population differences, marking them as unreliable for further processing
  df_nodata <- df |>
    filter(is.na(pop_diff)) |>
    distinct(collection_date) |> # Ensure distinct collection dates are selected
    mutate(unreliable = TRUE)

  # Filter out rows where population differences are missing (only keep reliable data)
  df <- df |>
    filter(!is.na(pop_diff))

  # Check if there are exactly two unique subgroup types (e.g., male and female in 'sex' column)
  type_values <- df %>%
    pull(type_col) %>%
    unique()

  if (length(type_values) != 2) {
    # Raise an error if there are not exactly two subgroups (e.g., two sexes)
    stop(
      "Each group must contain exactly two unique values in the type column."
    )
  }

  # Assign the two subgroup types (type_A and type_B) based on the unique values in the type column
  type_A <- type_values[1]
  type_B <- type_values[2]

  print(paste0("reference type is: ", type_A)) # Print the reference subgroup for debugging purposes

  # Filter and arrange the data for both subgroups (A and B) based on their collection dates
  A <- df %>%
    filter(!!sym(type_col) == type_A) %>%
    arrange(collection_date)
  B <- df %>%
    filter(!!sym(type_col) == type_B) %>%
    arrange(collection_date)

  # For subgroup B, generate the next population difference value and calculate the displacement sequence
  B <- B |>
    mutate(next_pop = lead(pop_diff)) |> # Calculate the next population difference for displacement calculation
    slice(1:(nrow(B) - 1)) |> # Remove the last row, which doesn't have a next value for displacement
    group_by(ADM2_PCODE, !!sym(type_col)) |>
    rowwise() |>
    summarize(
      L = list(
        tibble(
          collection_date = collection_date + 1, # Increment the collection date by 1 to simulate the next step
          pop_diff = seq_bidirectional(pop_diff + 0.00001, next_pop, 0.00001)
        )
      ), # Create a bidirectional sequence for displacement between current and next population differences
      .groups = "drop"
    ) |>
    unnest(L) |> # Unnest the list of sequences into individual rows
    bind_rows(
      # Bind the first row of subgroup B back into the data for proper alignment
      B |>
        slice(1)
    )

  # Calculate the shortest time gap between the displacement of subgroup A and subgroup B
  distances <- map_dfr(seq_len(nrow(A)), function(x) {
    find_shortest_distance(
      pop_ref = A$pop_diff[x], # Reference population difference from subgroup A
      date_ref = ymd(A$collection_date[x]), # Reference collection date from subgroup A
      df_comp = B # Comparison data for subgroup B
    )
  }) |>
    left_join(df_nodata, by = c("collection_date")) |> # Join with df_nodata to add the 'unreliable' flag for missing data
    filter(is.na(unreliable)) |> # Remove unreliable rows (those with missing population data)
    arrange(collection_date) # Ensure the final dataset is ordered by collection date

  return(distances) # Return the dataset with the computed time gaps between subgroup displacements
}


# Prepare the covariates -------------------------------------------------

compute_rollMetric <- function(df, fun, window, col_name = "") {
  fun_name <- as.character(substitute(fun))
  if (col_name == "") {
    col_name <- paste0(fun_name, "_", window, "days")
  }

  print(col_name)
  var <- df |>
    group_by(variable, ADM2_EN) |>
    arrange(variable, ADM2_EN, collection_date) |>
    tq_mutate(
      select = raw,
      mutate_fun = rollapply,
      width = window,
      FUN = fun,
      align = "right", # means lagging
      fill = "extend",
      col_rename = col_name
    ) |>
    ungroup() |>
    select(last_col())
  return(var)
}


# Apply temporal cross-correlation ---------------------------------------

compute_ccf <- function(
  df,
  n_lags = 5,
  change_type_name = "",
  pop_change_col = "pop_change"
) {
  df |>
    rename(pop_change = pop_change_col) |>
    filter(t > ceiling(n_lags / 2)) |>
    group_by(process_type, variable, scale_label, phase_date) |>
    reframe(
      lag = ccf(
        value_std,
        pop_change,
        n_lags,
        type = "covariance",
        plot = F,
        na.action = na.pass
      )$lag[,, 1],
      ccf_diff_cor = ccf(
        value_std,
        pop_change,
        n_lags,
        type = "correlation",
        plot = F,
        na.action = na.pass
      )$acf[,, 1]
    ) |>
    mutate(
      change_type = change_type_name,
      ccf_diff_cor = round(ccf_diff_cor * 100)
    ) |>
    pivot_wider(names_from = lag, values_from = ccf_diff_cor)
}


# Format cross-correlation table -----------------------------------------

prepare_gt <- function(df_wide, signif_thresh = 30) {
  df_wide <- df_wide |>
    mutate(
      variable_label = factor(
        case_when(
          variable == "acled_all" ~ "ACLED - All",
          variable ==
            "acled_eventAgainstCivilians" ~ "ACLED - Against civilians",
          variable == "acled_withfatalities" ~ "ACLED - Fatalities",
          variable == "damaged" ~ "CD - Building damages",
          variable == "prop_damaged" ~ "CD - Building damages (prop)",
          variable == "evacuationOrders" ~ "FA - Evacuation orders",
          variable == "operationArea" ~ "FA - Ground operations",
          variable == "pwtt" ~ "PWTT - Building damages"
        ),
        levels = c(
          "ACLED - All",
          "ACLED - Against civilians",
          "ACLED - Fatalities",
          "FA - Evacuation orders",
          "FA - Ground operations",
          "CD - Building damages",
          "CD - Building damages (prop)",
          "PWTT - Building damages"
        )
      ),
      .before = variable
    ) |>
    mutate(
      Window = factor(
        str_remove_all(scale_label, "raw|sum_|,|NA| |spatial"),
        levels = c("", "7days", "14days", "30days")
      ),
      .before = scale_label
    ) |>
    arrange(process_type, variable_label, Window) |>
    select(-scale_label, -change_type, -variable) |>
    mutate(` ` = "", .after = paste0(phase_date[1], "_5")) |>
    mutate(`  ` = "", .after = paste0(phase_date[1], "_5"))

  df_wide$significant <- df_wide |>
    select(is.numeric) |>
    mutate(across(everything(), ~ abs(.) > signif_thresh)) |>
    rowMeans(na.rm = T)

  df_wide <- df_wide |>
    filter(significant > 0) |>
    select(-significant) |>
    group_by(process_type)
  # |>
  #   mutate(
  #     across(is.numeric, ~ ifelse(abs(.) < signif_thresh, NA, .))
  #   )

  if (all(df_wide$Window == "")) {
    df_wide <- df_wide |> select(-Window)
  }

  df_gt <- df_wide |>
    # grouped rows
    gt(rowname_col = "variable_label") |>
    tab_spanner(label = "Future", matches("^13.*_[1-9]$"), id = "future_1") |>
    tab_spanner(label = "Future", matches("^24.*_[1-9]$"), id = "future_2") |>
    tab_spanner(label = "Future", matches("^23.*_[1-9]$"), id = "future_3") |>
    tab_spanner(label = "Past", matches("^13.*_-\\d$"), id = "Past_1") |>
    tab_spanner(label = "Past", matches("^24.*_-\\d$"), id = "Past_2") |>
    tab_spanner(label = "Past", matches("^23.*_-\\d$"), id = "Past_3") |>
    tab_spanner(label = phase_date[1], starts_with("13 Oct")) |>
    tab_spanner(label = phase_date[2], starts_with("24 Nov")) |>
    tab_spanner(label = phase_date[3], starts_with("23 Jan")) |>
    # background color
    data_color(
      method = "numeric",
      palette = palette_name,
      domain = c(-100, 100),
      bins = 10,
      na_color = "grey95"
    ) |>
    # column name
    cols_label_with(
      columns = starts_with("W"),
      fn = function(x) " "
    ) |>
    cols_label_with(
      columns = matches("^\\d"),
      fn = function(x) {
        str_split(x, "_")[[1]][2]
      }
    ) |>
    cols_width(
      variable_label ~ px(170),
      starts_with("W") ~ px(40),
      ` ` ~ px(5),
      `  ` ~ px(5),
      ends_with("0") ~ px(20),
      everything() ~ px(18)
    ) |>
    tab_style(
      style = cell_borders(sides = "all", color = "white", weight = px(1)),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style = cell_borders(sides = "all", color = "white", weight = px(1)),
      locations = cells_stubhead()
    ) |>
    tab_style(
      style = cell_borders(sides = "left", color = "black", weight = px(1)),
      locations = cells_body(columns = ends_with("-5"))
    ) |>
    tab_style(
      style = cell_text(weight = "bold", size = px(10)),
      locations = cells_body(columns = ends_with("0"))
    ) |>
    tab_style(
      style = cell_text(weight = "bold", size = px(10)),
      locations = cells_column_spanners(levels = 2)
    ) |>
    tab_options(
      table.font.size = 9,
      row_group.font.size = 13,
      column_labels.font.size = 13,
      footnotes.font.size = 11,
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "grey10",
      column_labels.border.top.width = px(0),
      column_labels.padding = px(3),
      stub.font.size = 12,
      table_body.hlines.width = px(0),
      table_body.border.top.width = px(0),
      table_body.border.bottom.width = px(0),
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "grey10",
      row_group.border.top.width = px(0),
      row_group.padding = px(2),
      row_group.font.weight = "bold",
      row_group.border.right.color = "grey10",
      stub_row_group.border.width = px(1),
      stub_row_group.border.color = "grey10",
      stub.border.color = "grey10",
      stub.border.width = px(1),
      table.border.top.width = px(0),
      table.border.bottom.width = px(0)
    ) |>
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_stub()
    ) |>
    sub_missing(missing_text = "") |>
    tab_footnote(
      footnote = paste(
        "Past indicates correlation between displacement rate and lagged value of covariate (in days).",
        "Future indicates correlation between displacement rate and future value of covariate (in days)",
        "\n Darker red indicates high positive correlation (up to 100), darker blue high negative correlation (up to -100).",
        "Grey indicates lack of variables during the given phase.",
        "Covariates that had all their correlation values below",
        signif_thresh,
        "have been removed."
      )
    )
  df_gt

  return(df_gt)
}
