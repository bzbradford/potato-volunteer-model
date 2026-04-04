# rebuild fst cache if season defs or build_hourly were changed
local({
  files <- list.files("data", "*.fst", full.names = TRUE)
  data <- lapply(files, load_data) |>
    bind_rows() |>
    distinct(station_id, collection_time, measure_id, .keep_all = TRUE) |>
    build_hourly()
  lapply(files, file.remove)
  lapply(unique(data$season), function(s) {
    data |>
      filter(season == s) |>
      write_fst(sprintf("data/%s.fst", s), compress = 99)
  })
})

# delete some recent data from HNCK to allow fetching
local({
  file <- last(list.files("data", "*.fst", full.names = TRUE))
  load_data(file) |>
    filter(station_id != "HNCK" | dttm < now() - hours(6)) |>
    write_fst(file, compress = 99)
})
