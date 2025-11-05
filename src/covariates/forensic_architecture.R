library(tidyverse)
library(sf)
library(geojsonio)
library(units)

env <- new.env()
source(".env", local = env)

in_dir <- env$in_dir


# input data -------------------------------------------------------------

governorate_gis <- st_read(paste0(in_dir, "/cod_ab/pse_admbnda_adm2_pamop_20210806.gpkg")) |>
  filter(ADM1_EN == "Gaza Strip") |>
  select(ADM2_PCODE, ADM2_EN, geom)

time_index <- governorate_gis |>
  st_drop_geometry() |>
  group_split(ADM2_PCODE, ADM2_EN) %>%
  map(function(x) {
    tibble(
      collection_date = seq(as.Date("2023-10-07"), as.Date("2024-05-31"), by = "day")
    ) |>
      mutate(
        t = 1:n(),
        ADM2_PCODE = x$ADM2_PCODE,
        ADM2_EN = x$ADM2_EN
      )
  }) |>
  bind_rows()

files <- dir(file.path(in_dir, "forensic_architecture"), full.names = T, pattern = ".geojson")

geojson_list <- lapply(files, geojsonsf::geojson_sf)
names(geojson_list) <- basename(files)


process_geojson <- function(filename, governorate_gis, time_index, output_path, title) {
  data <- geojson_list[[filename]]
  data <- st_make_valid(data)

  if (filename == "GridEvacOrders.geojson") {
    data <- data |>
      filter(!is.na(order_date)) |>
      mutate(collection_date = order_date |> as.Date())
  } else {
    data <- data |>
      mutate(collection_date = map(dates, ~ jsonlite::fromJSON(.) %>% as.Date())) |>
      unnest(collection_date) |>
      select(-dates)
  }

  data <- st_intersection(governorate_gis, data)
  data$area <- st_area(data)

  data_i <- data |>
    st_drop_geometry() |>
    group_by(collection_date, ADM2_PCODE, ADM2_EN) |>
    summarise(value = sum(area)) |>
    right_join(time_index) |>
    mutate(
      value = ifelse(is.na(value), 0, value),
      variable = title
    )

  write_csv(data_i, file.path(output_path))

  ggplot(data_i |> arrange(ADM2_EN, collection_date), aes(x = collection_date, y = value, color = ADM2_EN)) +
    geom_point() +
    geom_path() +
    labs(title = title, y = "Area", x = "Date")
}

# evacuation orders ------------------------------------------------------

evac_title <- "evacuationOrders"
evac_output <- file.path(env$wd, "out", "gaza", "covariates", "fa_evacOrders.csv")
process_geojson(
  "GridEvacOrders.geojson",
  governorate_gis, time_index, evac_output, evac_title
)

# ground operations ------------------------------------------------------

op_title <- "operationArea"
op_output <- file.path(env$wd, "out", "gaza", "covariates", "fa_operations.csv")
process_geojson(
  "2024.10.18_reported_operations_combined_file_subtracted-polygon-dates.geojson",
  governorate_gis, time_index, op_output, op_title
)
