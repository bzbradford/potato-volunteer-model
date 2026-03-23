library(tidyverse)
library(sf)
library(fst)

wi_counties <- read_sf("data/wi-county-bounds-24k.geojson")
wi_counties |> write_rds("app/data/counties.rds")

# initialize Wisconet wrapper
source("app/wisconet.R")
wn <- Wisconet$new()

wn$stations |> write_rds("app/data/stations.rds")

# what to download
select_measures <- tribble(
  ~standard_name              , ~measure_name     , ~type  , ~depth ,
  "60min_air_temp_f_avg"      , "Air temperature" , "air"  ,      0 ,
  "60min_soil_temp_f_avg@2in" , "2in soil temp"   , "soil" ,      2 ,
  "60min_soil_temp_f_avg@4in" , "4in soil temp"   , "soil" ,      4 ,
  "60min_soil_temp_f_avg@8in" , "8in soil temp"   , "soil" ,      8 ,
)

# select and process raw wisconet data
build_hourly <- function(df) {
  df |>
    select(
      station_id,
      station_name,
      latitude,
      longitude,
      collection_time,
      dttm,
      date,
      measure_id,
      standard_name,
      measure_value
    ) |>
    mutate(
      year = year(date),
      yday = yday(date),
      season = case_when(
        yday >= 300 ~ sprintf("%d-%d", year, year + 1),
        yday <= 90 ~ sprintf("%d-%d", year - 1, year),
        TRUE ~ as.character(year)
      ),
      .after = date
    ) |>
    mutate(
      freezing = measure_value < 32,
      killing = measure_value < 27
    ) |>
    left_join(select_measures, join_by(standard_name)) |>
    mutate(across(where(is.character), as.factor))
}

# load existing data
wn_data <- as_tibble(read_fst("data/wisconet_data.fst"))

# check for missing
wn_status <- wn_data |>
  summarize(
    last_dttm = max(dttm) |> with_tz("America/Chicago"),
    .by = station_id
  ) |>
  mutate(age = as.numeric(now() - last_dttm, units = "hours")) |>
  arrange(desc(age))


# update with new data
# wn_data_new <- wn$get_measures_all(
#   fields = select_measures$standard_name,
#   start_time = min(wn_status$last_dttm) + hours(1),
#   end_time = now()
# )

# update with new data using station-specific times
wn_data_new <- wn$get_measures_all(
  fields = select_measures$standard_name,
  start_time = set_names(wn_status$last_dttm + hours(1), wn_status$station_id),
  end_time = now()
)

# bind new data
wn_data <- wn_data |>
  bind_rows(wn_data_new) |>
  arrange(station_id, collection_time, measure_id) |>
  distinct() |>
  filter(measure_value > -50)

# save it
wn_data |> write_fst("data/wisconet_data.fst", compress = 99)

# build data for app
wn_hourly <- build_hourly(wn_data)

# split into files by season
lapply(seq_len(n_distinct(wn_hourly$season)), function(i) {
  seasons <- levels(wn_hourly$season)
  s <- seasons[i]
  df <- wn_hourly |> filter(season == s)
  fname <- sprintf("app/data/%d %s.fst", i, s)
  write_fst(df, fname, compress = 99)
})

list.files("app/data", "*.fst", full.names = T)
