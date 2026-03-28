# Potato volunteer risk map for Wisconsin using Wisconet data

#' Potato volunteer survival model
#' Soil temperatures must be below 27F for 120 hours
#' High risk: 2in & 4in < 120 hours
#' Moderate risk: 2in > 120 hours, 4in < 120 hours
#' Low risk 2in & 4in > 120 hours

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(leaflet)
  library(leaflet.extras)
  library(markdown)
  library(shiny)
  library(shinyWidgets)
  library(bslib)
  library(plotly)
  library(fst)
})

if (FALSE) {
  devtools::install_github("https://github.com/trafficonese/leaflet.extras")
  renv::update()
  renv::snapshot()
}

options(shiny.fullstacktrace = FALSE)

# Functions --------------------------------------------------------------------

find_closest <- function(lat, lon, df = stns) {
  dists <- (df$latitude - lat)^2 + (df$longitude - lon)^2
  df[which.min(dists), ]
}
# find_closest(45, -89)

# Load data --------------------------------------------------------------------

wi_counties <- read_rds("data/counties.rds")

# expect station data files like "{id} {season}.fst"
build_index <- function() {
  tibble(
    path = list.files("data", "\\.fst$", full.names = TRUE),
    file = basename(path),
    season = tools::file_path_sans_ext(file) # |> str_remove("^\\d+\\s*")
  ) |>
    arrange(season) |>
    mutate(id = row_number(), .before = 1)
}

index <- build_index()
season_choices <- set_names(index$season)
initial_season <- season_choices[max(which(str_detect(season_choices, "-")))]

# loader function
load_data <- function(path) {
  if (!file.exists(path)) {
    warning("File '", path, "' not found.")
    return()
  }
  read_fst(path) |>
    as_tibble() |>
    mutate(dttm_local = with_tz(dttm, "America/Chicago"), .after = dttm) |>
    filter(measure_value > -50)
}
# load_data(1)

# load hourly fst data
# cached_data <- map(seq_along(index$id), load_data) |>
#   set_names(index$season)

# load data for initial season
hourly_data <- load_data(index$path[index$season == initial_season]) |>
  list() |>
  set_names(initial_season)

# load stations from disk as fallback before Wisconet update runs
stns <- read_rds("data/stations.rds")

# hourly_data[["2025-2026"]] <- hourly_data[["2025-2026"]] |>
#   mutate(across(where(is.character), as.factor))

# Update from Wisconet ---------------------------------------------------------

if (FALSE) {
  last(cached_data) |>
    count(dttm, station_id) |>
    # complete(nesting(station_id, collection_time), fill = list(n = 0)) |>
    # filter(n == 0)
    pivot_wider(names_from = station_id, values_from = n) |>
    pivot_longer(-dttm) |>
    filter(is.na(value)) |>
    arrange(name, dttm)
}


# what to download
select_measures <- tribble(
  ~standard_name              , ~measure_name     , ~type  , ~depth ,
  "60min_air_temp_f_avg"      , "Air temperature" , "air"  ,      0 ,
  "60min_soil_temp_f_avg@2in" , "2in soil temp"   , "soil" ,      2 ,
  "60min_soil_temp_f_avg@4in" , "4in soil temp"   , "soil" ,      4 ,
  "60min_soil_temp_f_avg@8in" , "8in soil temp"   , "soil" ,      8 ,
)

# determines the season names for a vector of dates, eg '2025', '2025-2026'
calc_season <- function(date) {
  d <- lubridate::yday(date)
  yr <- lubridate::year(date)
  case_when(
    d >= 300 ~ sprintf("%d-%d", yr, yr + 1),
    d <= 90 ~ sprintf("%d-%d", yr - 1, yr),
    TRUE ~ as.character(yr)
  )
}

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
      season = calc_season(date),
      .after = date
    ) |>
    mutate(
      freezing = measure_value < 32,
      killing = measure_value < 27
    ) |>
    left_join(select_measures, join_by(standard_name)) |>
    mutate(across(where(is.character), as.factor))
}

# initialize api wrapper
source("wisconet.R")

# Update hourly_data and stns from the Wisconet API.
# Returns list(hourly_data, stns, season_choices) on success, NULL on failure.
update_from_wisconet <- function(hourly_data, stns) {
  tryCatch(
    {
      wn <- Wisconet$new()
      stns <- wn$stations
      stns |> write_rds("data/stations.rds")

      # use the last 2 seasons to check for update needs
      recent_data <- bind_rows(tail(hourly_data, n = 2))

      # find the earliest date for the current season
      date_seed <- tibble(
        date = seq.Date(today() - months(6), today()),
        season = calc_season(date)
      ) |>
        filter(season == last(season))

      # use as start_date for missing or new stations
      fallback_start_date <- as_datetime(
        min(date_seed$date),
        tz = "America/Chicago"
      )

      # get most recent data times, falling back to fallback_start_date for missing stations
      wn_status <- if (length(hourly_data) == 0) {
        tibble(station_id = stns$station_id, last_dttm = fallback_start_date)
      } else {
        known <- recent_data |>
          summarize(
            last_dttm = max(dttm) |> with_tz("America/Chicago"),
            .by = station_id
          )
        # any stations with no history at all get fallback_start_date
        new_stns <- tibble(
          station_id = setdiff(stns$station_id, known$station_id),
          last_dttm = fallback_start_date
        )
        bind_rows(known, new_stns)
      }
      wn_status <- wn_status |>
        mutate(age = as.numeric(now() - last_dttm, units = "hours")) |>
        arrange(desc(age))

      # find stations with stale data
      stns_to_update <- wn_status |> filter(age > 2)

      if (nrow(stns_to_update) == 0) {
        message("Everything up to date.")
        return(list(
          hourly_data = hourly_data,
          stns = stns,
          season_choices = names(hourly_data) |> set_names()
        ))
      }

      print(stns_to_update)

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
        return(list(
          hourly_data = hourly_data,
          stns = stns,
          season_choices = names(hourly_data) |> set_names()
        ))
      }

      hourly_data_new <- build_hourly(wn_new_data)
      seasons_updated <- as.character(unique(hourly_data_new$season))

      # bind new data to existing data
      wn_updated_data <- recent_data |>
        filter(season %in% seasons_updated) |>
        bind_rows(hourly_data_new) |>
        arrange(station_id, collection_time, measure_id) |>
        distinct(station_id, collection_time, measure_id, .keep_all = TRUE) |>
        filter(measure_value > -50)

      # split into files by season and update the local list
      for (s in seasons_updated) {
        df <- wn_updated_data |> filter(season == s)
        hourly_data[[s]] <- df
        write_fst(df, sprintf("data/%s.fst", s), compress = 99)
      }

      list(
        hourly_data = hourly_data,
        stns = stns,
        season_choices = names(hourly_data) |> set_names()
      )
    },
    error = function(e) {
      message("Update from Wisconet failed: ", e$message)
      NULL
    }
  )
}

if (FALSE) {
  result <- update_from_wisconet(hourly_data, stns)
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

if (FALSE) {
  build_map()

  df <- vol_risk[["2025-2026"]]
  df |> build_risk_map()

  hourly_data |>
    filter(station_id == "KNGT")
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
  hourly_data |>
    filter(station_id == "HNCK", depth == 2) |>
    build_plot()

  hourly_data |>
    filter(station_id == "HNCK", depth == 0) |>
    build_plot()

  hourly_data |>
    filter(station_id == "HNCK", depth > 0) |>
    build_plot()
}

# Archive ----------------------------------------------------------------------

## Stacked plot ----
#
# build_plotly <- function(data, stn_id) {
#   stn <- stns |> filter(station_id == stn_id)
#   risk <- volunteer_risk |> filter(station_id == stn_id)
#   stn_data <- data |> filter(station_id == stn_id)
#
#   killing_hrs <- stn_data |>
#     filter(depth > 0) |>
#     summarize(hrs = sum(measure_value < 27, na.rm = TRUE), .by = measure_name)
#
#   p1 <- stn_data |>
#     filter(depth == 2) |>
#     build_plot()
#   p2 <- stn_data |>
#     filter(depth == 4) |>
#     build_plot()
#
#   subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) |>
#     layout(
#       # margin = list(t = 20, r = 10, b = 10, l = 10),
#       annotations = list(
#         # subplot 1 title
#         list(
#           text = paste0(
#             "<b>2-inch soil temperature</b> - ",
#             killing_hrs$hrs[1],
#             " hrs below 27°F"
#           ),
#           x = 0.5,
#           y = 1.0,
#           xref = "paper",
#           yref = "paper",
#           showarrow = FALSE,
#           font = list(size = 11),
#           xanchor = "center",
#           yanchor = "bottom"
#         ),
#         # subplot 2 title
#         list(
#           text = paste0(
#             "<b>4-inch soil temperature</b> - ",
#             killing_hrs$hrs[2],
#             " hrs below 27°F"
#           ),
#           x = 0.5,
#           y = 0.48,
#           xref = "paper",
#           yref = "paper",
#           showarrow = FALSE,
#           font = list(size = 11),
#           xanchor = "center",
#           yanchor = "bottom"
#         )
#       )
#     )
# }
#
# if (FALSE) {
#   hourly_data |>
#     filter(season == "2025-2026") |>
#     build_plotly("ANGO")
# }
