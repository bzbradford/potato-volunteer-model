library(tidyverse)
library(leaflet)
library(httr2)
library(fst)

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

if (FALSE) {
  leaflet(stns) %>%
    addTiles() %>%
    addMarkers(
      lng = ~longitude,
      lat = ~latitude,
      label = ~ paste0(station_id, ": ", station_name)
    )
}


## Fields ----

get_fields <- function(stn_id) {
  fields_url %>%
    request() %>%
    req_perform() %>%
    resp_body_json(simplifyVector = T) %>%
    as_tibble() %>%
    select(where(~ !all(is.na(.x) | .x == ""))) %>%
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
    select(where(~ !all(is.na(.x) | .x == "")))
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
      date = as_date(dttm_local),
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

# get data from Wisconet
get_measures <- function(stn_id, fields, start_time, end_time = now()) {
  t <- now()
  message("GET ==> ", stn_id, ": ", start_time, " to ", end_time)
  message("  Fields: ", paste(fields, collapse = ", "))

  if (!exists("stns")) {
    stns <<- get_stns()
  }
  stopifnot(stn_id %in% stns$station_id)

  if (!exists("all_fields")) {
    all_fields <- get_fields()
  }
  stopifnot(all(fields %in% all_fields$standard_name))

  stn <- stns %>% filter(station_id == stn_id)

  tryCatch(
    {
      req <- request(measures_url(stn_id)) %>%
        req_url_query(
          start_time = time_to_gmt(start_time),
          end_time = time_to_gmt(end_time),
          fields = paste(fields, collapse = ",")
        )
      resp <- req_perform(req) %>% resp_body_json()
      if (length(resp[["data"]]) == 0) {
        stop("No data")
      }
      resp_fields <- parse_fieldlist(resp$fieldlist)
      resp_data <- parse_data(resp$data)
      resp_joined <- resp_data %>% left_join(resp_fields, join_by(measure_id))
      n_obs <- length(unique(resp_joined$collection_time))
      message(sprintf("  Received %s observations in %.3f", n_obs, now() - t))
      data <- tibble(station_id = stn_id) %>% bind_cols(resp_joined)
      stn %>% left_join(data, join_by(station_id))
    },
    error = function(e) {
      message("  FAIL: ", e$message)
      tibble()
    }
  )
}

# get data for all stations
get_measures_all_stns <- function(fields, start_time, end_time = now()) {
  if (!exists("stns")) {
    stns <<- get_stns()
  }
  stns <- stns %>% filter(earliest_api_date <= end_time)
  message(
    "Found ",
    nrow(stns),
    " stations: ",
    paste(stns$station_id, collapse = ", ")
  )
  resps <- lapply(seq_len(nrow(stns)), function(i) {
    message(sprintf("Station %i / %i", i, nrow(stns)))
    stn <- slice(stns, i)
    get_measures(stn$station_id, fields, start_time, end_time)
  })
  bind_rows(resps)
}


# Get data ---------------------------------------------------------------------

all_fields %>% filter(str_detect(standard_name, "soil")) %>% pull(standard_name)

select_measures <- tribble(
  ~standard_name              , ~measure_name     , ~type  , ~depth ,
  "60min_air_temp_f_avg"      , "Air temperature" , "air"  ,      0 ,
  "60min_soil_temp_f_avg@2in" , "2in soil temp"   , "soil" ,      2 ,
  "60min_soil_temp_f_avg@4in" , "4in soil temp"   , "soil" ,      4 ,
  "60min_soil_temp_f_avg@8in" , "8in soil temp"   , "soil" ,      8 ,
)

# get all new data?
# wn_data_new <- get_measures_all_stns(
#   fields = select_measures$standard_name,
#   start_time = ymd_hms("2024-1-1 0:0:0"),
#   end_time = now()
# )

# load existing data
wn_data <- read_fst("data/wn_data.fst") |>
  as_tibble()

# get new data
if (FALSE) {
  # get all measures
  wn_data_new <- get_measures_all_stns(
    fields = select_measures$standard_name,
    start_time = max(wn_data$dttm),
    end_time = now()
  )

  # get one measure
  wn_data_new <- get_measures_all_stns(
    fields = "60min_soil_temp_f_avg@8in",
    start_time = ymd_hms("2023-1-1 0:0:0")
  )

  # get one station
  wn_data_new <- get_measures(
    stn_id = "ANGO",
    fields = select_measures$standard_name,
    start_time = ymd_hms("2023-1-1 0:0:0"),
    end_time = ymd_hms("2023-12-31 23:0:0")
  )

  # add new data
  wn_data <- wn_data %>%
    bind_rows(wn_data_new) %>%
    arrange(station_id, dttm_local, measure_id) %>%
    distinct() %>%
    filter(measure_value > -50)

  # save it
  local({
    df <- as_tibble(wn_data)
    # fname <- sprintf(
    #   "data/wn_data_%s_%s.fst",
    #   format(first(df$dttm_local), "%Y-%m-%d"),
    #   format(last(df$dttm_local), "%Y-%m-%d")
    # )
    fname <- sprintf("data/wn_data.fst")
    message(
      "Saving to '",
      fname,
      "' (",
      format(nrow(df), big.mark = ","),
      " rows)"
    )
    write_fst(wn_data, fname, compress = 99)
  })

  # test plot
  wn_data %>%
    filter(station_id == "HNCK") %>%
    ggplot(aes(x = dttm_local, y = measure_value, color = station_id)) +
    geom_line(lwd = .25) +
    facet_wrap(~standard_name, ncol = 1) +
    theme(legend.position = "none")
}


# Process data -----------------------------------------------------------------

hourly_data <- wn_data %>%
  mutate(
    year = year(date),
    yday = yday(date),
    .after = date
  ) %>%
  mutate(
    season = factor(if_else(
      yday >= 300,
      paste(year, year + 1, sep = "-"),
      if_else(
        yday <= 90,
        paste(year - 1, year, sep = "-"),
        as.character(year)
      )
    )),
    freezing = measure_value < 32,
    killing = measure_value < 27
  ) %>%
  left_join(select_measures)


# Save for app -----------------------------------------------------------------

hourly_data %>%
  fst::write_fst("../app/data/hourly_data.fst", compress = 99)
stns %>% saveRDS("../app/data/stns.rds")
# select_measures %>% saveRDS("../potato-volunteer-app/data/measures.rds")
