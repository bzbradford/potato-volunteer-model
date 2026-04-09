# Potato volunteer risk map for Wisconsin using Wisconet data

#' Potato volunteer survival model
#' Soil temperatures must be below 27F for 120 hours
#' High risk: 2in & 4in < 120 hours
#' Moderate risk: 2in > 120 hours, 4in < 120 hours
#' Low risk 2in & 4in > 120 hours

library(sf)

suppressPackageStartupMessages({
  library(tidyverse)
  library(fst)
  library(mirai) # async
  library(shiny)
  library(shinyWidgets)
  library(bslib)
  library(markdown)
  library(leaflet)
  library(leaflet.extras)
  library(plotly)
})

# dev tools
if (FALSE) {
  remotes::install_github("bzbradford/rwisconet")
  # remotes::install_github("https://github.com/trafficonese/leaflet.extras")
  # renv::install("sf@1.0-24")
  renv::init()
  renv::clean()
  renv::update()
  renv::snapshot()
}

options(shiny.fullstacktrace = FALSE)

# set up the background worker for API requests
daemons(1)

# Setup ------------------------------------------------------------------------

TZ <- "America/Chicago"
wi_counties <- read_rds("data/counties.rds")
stns <- read_rds("data/stations.rds")

# expect station data files like "{id} {season}.fst"
build_index <- function() {
  tibble(
    path = list.files("data", "\\.fst$", full.names = TRUE),
    file = basename(path),
    last_updated = file.mtime(path),
    season = tools::file_path_sans_ext(file)
  ) |>
    arrange(season) |>
    mutate(id = row_number(), .before = 1)
}

# data loader function
load_data <- function(path) {
  if (!file.exists(path)) {
    warning("File '", path, "' not found.")
    return()
  }
  read_fst(path) |>
    as_tibble() |>
    mutate(dttm_local = with_tz(dttm, TZ), .after = dttm) |>
    filter(measure_value > -50)
}

# create index and season choices for UI
index <- build_index()
season_choices <- set_names(index$season)

# Update from Wisconet ---------------------------------------------------------

# what to download
select_measures <- tribble(
  ~standard_name              , ~measure_name     , ~type  , ~depth ,
  "60min_air_temp_f_avg"      , "Air temperature" , "air"  ,      0 ,
  "60min_soil_temp_f_avg@2in" , "2in soil temp"   , "soil" ,      2 ,
  "60min_soil_temp_f_avg@4in" , "4in soil temp"   , "soil" ,      4 ,
  "60min_soil_temp_f_avg@8in" , "8in soil temp"   , "soil" ,      8 ,
)

# determines the season names for a vector of dates, eg '2025', '2025-2026'
# season starts Oct 1
calc_season <- function(date) {
  m <- lubridate::month(date)
  yr <- lubridate::year(date)
  if_else(
    m >= 10,
    sprintf("%d-%d", yr, yr + 1),
    sprintf("%d-%d", yr - 1, yr)
  )
}
# calc_season(today())

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
    mutate(season = calc_season(date), .after = date) |>
    mutate(
      freezing = measure_value < 32,
      killing = measure_value < 27
    ) |>
    left_join(select_measures, join_by(standard_name)) |>
    mutate(across(where(is.character), as.factor))
}

#' Update fst files with new data from Wisconet
#' @returns `seasons_updated` or `NULL`
update_from_wisconet <- function(
  file_index = build_index(),
  data_loader = load_data
) {
  require(tidyverse)
  require(fst)
  require(rwisconet)

  # initialize
  wn <- Wisconet$new()

  # save updated stations list
  stns <- wn$stations
  stns |> write_rds("data/stations.rds")

  # set earliest date
  MIN_DATE <- as_date("2025-10-1") # start of winter season
  MIN_DTTM <- as_datetime(MIN_DATE, tz = TZ)

  # load most recent 2 seasons from index
  if (length(file_index) == 0) {
    recent_data <- tibble()
    wn_status <- tibble(
      station_id = stns$station_id,
      last_dttm = MIN_DTTM,
      age = Inf
    )
  } else {
    recent_data <- tail(file_index$path, 2) |>
      lapply(data_loader) |>
      bind_rows()
    known_stns <- recent_data |>
      summarize(
        last_dttm = with_tz(max(dttm), TZ),
        .by = station_id
      )
    wn_status <- stns |>
      select(station_id) |>
      left_join(known_stns, join_by(station_id)) |>
      replace_na(list(last_dttm = MIN_DTTM)) |>
      mutate(age = as.numeric(now() - last_dttm, units = "hours")) |>
      arrange(desc(age))
  }

  # find stations with stale data
  stns_to_update <- wn_status |> filter(age > 2)

  if (nrow(stns_to_update) == 0) {
    message("Everything up to date.")
    return(invisible())
  }

  # update with new data using station-specific times
  wn_new_data <- wn$get_measures_stations(
    stn_ids = as.character(stns_to_update$station_id),
    fields = select_measures$standard_name,
    start_time = set_names(
      stns_to_update$last_dttm + minutes(30),
      stns_to_update$station_id
    ),
    end_time = now()
  )

  if (nrow(wn_new_data) == 0) {
    message("Tried to get new data but received none!")
    return(invisible())
  }

  # process new data
  hourly_data_new <- build_hourly(wn_new_data)
  seasons_updated <- as.character(unique(hourly_data_new$season))

  # bind new data to existing data
  wn_updated_data <- recent_data |>
    bind_rows(hourly_data_new) |>
    filter(season %in% seasons_updated) |>
    arrange(station_id, collection_time, measure_id) |>
    distinct(station_id, collection_time, measure_id, .keep_all = TRUE) |>
    filter(measure_value > -50)

  # split into files by season and update the cache
  for (s in seasons_updated) {
    wn_updated_data |>
      filter(season == s) |>
      write_fst(sprintf("data/%s.fst", s), compress = 99)
  }

  return(seasons_updated)
}


# Volunteer risk calculation ---------------------------------------------------

#' Potato volunteer survival model
#' Soil temperatures must be below 27F for 120 hours
#' High risk: 2in & 4in < 120 hours
#' Moderate risk: 2in > 120 hours, 4in < 120 hours
#' Low risk 2in & 4in > 120 hours
risk_scores <- c("Very high", "High", "Moderate", "Low")
calc_vol_risk <- function(data) {
  data |>
    summarize(
      killing = sum(killing, na.rm = TRUE),
      .by = c(station_id, station_name, latitude, longitude, season, depth)
    ) |>
    complete(
      nesting(season, station_id, station_name, latitude, longitude),
      depth,
      fill = list(killing = 0)
    ) |>
    pivot_wider(
      names_from = depth,
      names_glue = "killing{.name}",
      values_from = killing
    ) |>
    mutate(
      risk_score = (killing2 >= 120) + (killing4 >= 120) + (killing8 >= 120),
      risk = map(risk_score, ~ risk_scores[.x + 1]) |>
        factor(levels = risk_scores)
    ) |>
    arrange(season, station_id)
}

# calculate volunteer risk scores from hourly data
# map(cached_data, calc_vol_risk)

# Map --------------------------------------------------------------------------

find_closest <- function(lat, lon, df = stns) {
  dists <- (df$latitude - lat)^2 + (df$longitude - lon)^2
  df[which.min(dists), ]
}
# find_closest(45, -89)

build_map <- function() {
  btn1 <- easyButton(
    position = "topleft",
    # icon = "fa-location-pin",
    icon = "fa-magnifying-glass-location",
    title = "Zoom to station",
    onClick = JS(
      "(btn, map) => { Shiny.setInputValue('map_btn', 'zoom', {priority: 'event'}); }"
    )
  )

  btn2 <- easyButton(
    position = "topleft",
    icon = "fa-expand",
    title = "Reset map",
    onClick = JS(
      "(btn, map) => { Shiny.setInputValue('map_btn', 'reset', {priority: 'event'}); }"
    )
  )

  risk <- fct_inorder(risk_scores)
  leaflet(
    options = leafletOptions(
      zoomSnap = 0.25,
      zoomDelta = 0.5
    )
  ) |>
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
    addLayersControl(baseGroups = c("OpenStreetMap", "Satellite")) |>
    addEasyButtonBar(btn1, btn2) |>
    addMapPane("stations", 410) |>
    addMapPane("selected", 420) |>
    addPolygons(
      data = wi_counties,
      color = "black",
      weight = .25,
      fillColor = "white",
      fillOpacity = .0001,
      label = ~ paste(county_name, "County")
    ) |>
    # add_risk_markers() |>
    addLegend(
      position = "bottomright",
      pal = colorFactor("viridis", risk, reverse = TRUE),
      values = risk,
      title = "Volunteer risk",
      opacity = 1
    ) |>
    fit_stns()
}

add_risk_markers <- function(map, data) {
  map |>
    clearGroup("stations") |>
    addCircleMarkers(
      data = data,
      lat = ~latitude,
      lng = ~longitude,
      layerId = ~station_id,
      group = "stations",
      label = ~ str_glue("<b>{station_name}</b><br>Risk: {risk}") |>
        lapply(HTML),
      color = "black",
      weight = 1,
      opacity = 1,
      fillColor = ~ colorFactor("viridis", risk, reverse = TRUE)(risk),
      fillOpacity = 1,
      options = pathOptions(pane = "stations")
    )
}

fit_stns <- function(map) {
  map |>
    fitBounds(
      lat1 = min(stns$latitude) - 0.25,
      lat2 = max(stns$latitude) + 0.25,
      lng1 = min(stns$longitude) - 0.25,
      lng2 = max(stns$longitude) + 0.25
    )
}

# test
if (FALSE) {
  df <- last(hourly_data) |>
    calc_vol_risk()
  build_map() |>
    add_risk_markers(df)
}


# Plots ------------------------------------------------------------------------

## Single plot ----

build_plot <- function(data) {
  require(plotly)
  require(dplyr)

  expand_range <- function(lo, hi, amt = .05) {
    c(lo - abs(hi - lo) * amt, hi + abs(hi - lo) * amt)
  }

  # Color palettes: each element is c(light_hourly, dark_rolling_mean)
  color_palettes <- list(
    c("rgba(100,149,237,0.6)", "rgba(30,60,150,0.9)"), # blue (cornflower / dark blue)
    c("rgba(80,180,80,0.6)", "rgba(20,110,20,0.9)"), # green
    c("rgba(230,130,60,0.6)", "rgba(180,60,10,0.9)") # orange
  )

  yrange <- if (nrow(data) == 0) {
    c(0, 100)
  } else {
    expand_range(
      lo = min(data$measure_value, na.rm = TRUE),
      hi = max(data$measure_value, na.rm = TRUE)
    )
  }

  # Compute rolling mean per measure
  data <- data |>
    group_by(station_id, standard_name) |>
    arrange(dttm_local) |>
    mutate(
      rolling_mean = zoo::rollapply(
        measure_value,
        24,
        \(x) mean(x, na.rm = TRUE),
        partial = TRUE
      )
    )

  measures <- levels(data$measure_name)
  # show_legend <- length(measures) > 1
  show_legend <- FALSE

  p <- plot_ly()

  for (i in seq_along(measures)) {
    m <- measures[i]
    d <- filter(data, measure_name == m)
    colors <- color_palettes[[(i - 1) %% length(color_palettes) + 1]]

    p <- p |>
      add_lines(
        data = d,
        x = ~dttm_local,
        y = ~measure_value,
        line = list(color = colors[1], width = 1),
        name = paste(m, "(hourly)"),
        legendgroup = m,
        showlegend = show_legend,
        hovertemplate = "%{y:.1f}°F"
      ) |>
      add_lines(
        data = d,
        x = ~dttm_local,
        y = ~rolling_mean,
        line = list(color = colors[2], width = 2),
        name = paste(m, "(24-hr)"),
        legendgroup = m,
        showlegend = show_legend,
        hovertemplate = "%{y:.1f}°F"
      )
  }

  p |>
    layout(
      # margin = list(t = 10, r = 10, b = 10, l = 10),
      margin = list(t = 0, r = 0, b = 0, l = 0),
      hovermode = "x unified",
      xaxis = list(
        title = NA,
        showline = TRUE,
        linecolor = "black",
        linewidth = 1,
        mirror = TRUE
      ),
      yaxis = list(
        title = "Temperature (°F)",
        showline = TRUE,
        linecolor = "black",
        linewidth = 1,
        mirror = TRUE,
        zeroline = FALSE,
        range = yrange
      ),
      shapes = list(
        list(
          type = "rect",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = yrange[1],
          y1 = 27,
          fillcolor = "rgba(255,0,0,0.05)",
          line = list(width = 0)
        ),
        list(
          type = "rect",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = 27,
          y1 = 32,
          fillcolor = "rgba(0,0,255,0.05)",
          line = list(width = 0)
        ),
        list(
          type = "rect",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = 32,
          y1 = 999,
          fillcolor = "rgba(0,255,0,0.05)",
          line = list(width = 0)
        ),
        list(
          type = "line",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = 32,
          y1 = 32,
          line = list(color = "gray", dash = "dash", width = 1)
        ),
        list(
          type = "line",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = 27,
          y1 = 27,
          line = list(color = "red", width = 1)
        )
      ),
      annotations = list(
        list(
          x = 0,
          y = 32,
          text = "Freezing (32°F)",
          showarrow = FALSE,
          xref = "paper",
          yref = "y",
          xanchor = "left",
          yanchor = "bottom",
          font = list(size = 9, color = "gray")
        ),
        list(
          x = 0,
          y = 27,
          text = "Killing (27°F)",
          showarrow = FALSE,
          xref = "paper",
          yref = "y",
          xanchor = "left",
          yanchor = "bottom",
          font = list(size = 9, color = "red")
        )
      )
    )
}

if (FALSE) {
  data <- load_data(last(index$path))
  data |>
    filter(station_id == "HNCK", depth == 2) |>
    build_plot()

  data |>
    filter(station_id == "HNCK", depth == 0) |>
    build_plot()

  data |>
    filter(station_id == "HNCK", depth > 0) |>
    build_plot()
}
