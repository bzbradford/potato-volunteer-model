
library(tidyverse)
library(leaflet)
library(httr2)



# API ---------------------------------------------------------------------

api_url <- "https://wisconet.wisc.edu/api/v1"
stns_url <- str_glue("{api_url}/stations")
fields_url <- str_glue("{api_url}/fields")
measures_url <- function(stn_id) str_glue("{stns_url}/{stn_id}/measures")


# Endpoints ---------------------------------------------------------------

## Stations ----

get_stns <- function() {
  stns_url %>%
    request() %>%
    req_perform() %>%
    resp_body_json(simplifyVector = T) %>%
    as_tibble() %>%
    mutate(across(earliest_api_date, mdy)) %>%
    select(-c(id, campbell_cloud_id, legacy_id, station_slug)) %>%
    filter(!grepl("TEST", station_id))
}

stns <- get_stns()

leaflet(stns) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, label = ~paste0(station_id, ": ", station_name))


## Fields ----

get_fields <- function(stn_id) {
  fields_url %>%
    request() %>%
    req_perform() %>%
    resp_body_json(simplifyVector = T) %>%
    as_tibble() %>%
    select(where(~!all(is.na(.x) | .x == ""))) %>%
    arrange(measure_type, standard_name)
}

all_fields <- get_fields()


## Measures ----

# parse the measures metadata
parse_fieldlist <- function(fieldlist) {
  fieldlist %>%
    enframe() %>%
    select(-name) %>%
    unnest_wider(value) %>%
    rename(measure_id = id) %>%
    select(where(~!all(is.na(.x) | .x == "")))
}

# parse the data response from Wisconet
parse_data <- function(data) {
  data %>%
    enframe() %>%
    select(-name) %>%
    unnest_wider(value) %>%
    mutate(
      dttm = as_datetime(collection_time),
      dttm_local = with_tz(dttm, tzone = "America/Chicago"),
      .after = collection_time
    ) %>%
    unnest_longer(measures) %>%
    unnest_wider(measures, names_sep = "_") %>%
    rename(measure_id = measures_1, measure_value = measures_2)
}

# convert time to GMT timestamp
time_to_gmt <- function(t) {
  force_tz(t, "America/Chicago") %>%
    with_tz("GMT") %>%
    as.numeric()
}

get_measures <- function(stn_id, fields, start_time, end_time = now()) {
  t <- now()
  message("GET ==> ", stn_id, ": ", start_time, " to ", end_time)
  message("  Fields: ", paste(fields, collapse = ", "))

  if (!exists("stns")) stns <<- get_stns()
  stopifnot(stn_id %in% stns$station_id)

  if (!exists("all_fields")) all_fields <- get_fields()
  stopifnot(all(fields %in% all_fields$standard_name))

  stn <- stns %>% filter(station_id == stn_id)

  tryCatch({
    req <- request(measures_url(stn_id)) %>%
      req_url_query(
        start_time = time_to_gmt(start_time),
        end_time = time_to_gmt(end_time),
        fields = paste(fields, collapse = ",")
      )
    resp <- req_perform(req) %>% resp_body_json()
    resp_fields <- parse_fieldlist(resp$fieldlist)
    resp_data <- parse_data(resp$data)
    resp_joined <- resp_data %>% left_join(resp_fields, join_by(measure_id))
    n_obs <- length(unique(resp_joined$collection_time))
    message("  Received ", n_obs, " observations in ", now() - t)
    data <- tibble(station_id = stn_id) %>% bind_cols(resp_joined)
    stn %>% left_join(data, join_by(station_id))
  }, error = function(e) {
    message("  FAIL: ", e)
    tibble()
  })
}

get_measures_all_stns <- function(fields, start_time, end_time = now()) {
  if (!exists("stns")) stns <<- get_stns()
  stns <- stns %>% filter(earliest_api_date <= start_time)
  message("Found ", nrow(stns), " stations: ", paste(stns$station_id, collapse = ", "))
  resps <- lapply(stns$station_id, function(stn_id) {
    get_measures(stn_id, fields, start_time, end_time)
  })
  bind_rows(resps)
}



# Get soil temps ----------------------------------------------------------

all_fields %>% filter(collection_frequency == "daily") %>% pull(standard_name)

soil_fields <- c(
  "60min_soil_temp_f_avg@2in",
  "60min_soil_temp_f_avg@4in"
)

all(soil_fields %in% all_fields$standard_name)

soil_data <- get_measures_all_stns(
  fields = soil_fields,
  start_time = ymd_hms("2024-11-1 0:0:0"),
  end_time = ymd_hms("2025-3-31 23:00:00")
)

soil_data %>% write_csv("data/soil_temps_hourly_2024.11.01_2025.03.06.csv.gz", na = "")

soil_data %>%
  ggplot(aes(x = dttm_local, y = measure_value, color = station_id)) +
  geom_line() +
  facet_wrap(~standard_name)
