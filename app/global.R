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

# loader function
load_data <- function(id) {
  path <- index[id, ][["path"]]
  if (!file.exists(path)) {
    warning("File '", path, "' not found.")
    return()
  }
  read_fst(path) |>
    as_tibble() |>
    mutate(dttm_local = with_tz(dttm, "America/Chicago"), .after = dttm)
}
# load_data(1)

# load hourly fst data
hourly_data <- map(seq_along(index$id), load_data) |>
  set_names(index$season)

# set data for UI and set the most recent
season_choices <- names(hourly_data) |> set_names()


# Update from Wisconet ---------------------------------------------------------

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


# Volunteer risk calculation ---------------------------------------------------

#' Potato volunteer survival model
#' Soil temperatures must be below 27F for 120 hours
#' High risk: 2in & 4in < 120 hours
#' Moderate risk: 2in > 120 hours, 4in < 120 hours
#' Low risk 2in & 4in > 120 hours
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
      risk = recode_values(
        risk_score,
        0 ~ "Very high",
        1 ~ "High",
        2 ~ "Moderate",
        3 ~ "Low"
      ) |>
        factor(levels = c("Very high", "High", "Moderate", "Low"))
    ) |>
    arrange(season, station_id)
}

# calculate volunteer risk scores from hourly data
vol_risk <- map(hourly_data, calc_vol_risk)


# Map --------------------------------------------------------------------------

#' @param data:tibble volunteer_risk
build_risk_map <- function(data) {
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

  data |>
    leaflet() |>
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
    addLayersControl(baseGroups = c("OpenStreetMap", "Satellite")) |>
    addEasyButtonBar(btn1, btn2) |>
    addPolygons(
      data = wi_counties,
      color = "black",
      weight = .25,
      fillColor = "white",
      fillOpacity = .0001,
      label = ~ paste(county_name, "County")
    ) |>
    add_risk_markers() |>
    addLegend(
      position = "bottomright",
      pal = colorFactor("viridis", data$risk, reverse = TRUE),
      values = data$risk,
      title = "Volunteer risk",
      opacity = 1
    )
}

add_risk_markers <- function(map, data = getMapData(map)) {
  addCircleMarkers(
    map,
    data = data,
    lat = ~latitude,
    lng = ~longitude,
    layerId = ~station_id,
    label = ~ str_glue("<b>{station_name}</b><br>Risk: {risk}") |>
      lapply(HTML),
    color = "black",
    weight = 1,
    opacity = 1,
    fillColor = ~ colorFactor("viridis", risk, reverse = TRUE)(risk),
    fillOpacity = 1
  )
}

if (FALSE) {
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


# Update from Wisconet ---------------------------------------------------------

# initialize api wrapper
source("wisconet.R")

# try to update stations as necessary
tryCatch(
  {
    wn <- Wisconet$new()
    stns <- wn$stations
    stns |> write_rds("data/stations.rds")

    recent_data <- last(hourly_data)

    # if the latest season doesn't have all the stations use the most recent 2
    if (!all(stns$station_id %in% recent_data$station_id)) {
      recent_data <- bind_rows(tail(hourly_data, n = 2))
    }

    # get most recent data times for each station
    wn_status <- recent_data |>
      summarize(
        last_dttm = max(dttm) |> with_tz("America/Chicago"),
        .by = station_id
      ) |>
      mutate(age = as.numeric(now() - last_dttm, units = "hours")) |>
      arrange(desc(age))

    # find stations with stale data
    stns_to_update <- wn_status |>
      filter(age > 2)

    if (nrow(stns_to_update) == 0) {
      message("Everything up to date.")
      return()
    }

    # update with new data using station-specific times
    wn_new_data <- wn$get_measures_stations(
      stn_ids = as.character(stns_to_update$station_id),
      fields = select_measures$standard_name,
      start_time = set_names(
        stns_to_update$last_dttm + hours(1),
        stns_to_update$station_id
      ),
      end_time = now()
    )

    if (nrow(wn_new_data) == 0) {
      message("Tried to get new data but received none!")
      return()
    }

    # bind new data to existing data
    wn_updated_data <- bind_rows(recent_data, wn_new_data) |>
      arrange(station_id, collection_time, measure_id) |>
      distinct() |>
      filter(measure_value > -50) |>
      build_hourly()

    seasons_updated <- unique(wn_updated_data$season)

    # split into files by season
    lapply(seasons_updated, function(s) {
      df <- wn_updated_data |> filter(season == s)
      hourly_data[[s]] <- df
      fname <- sprintf("data/%s.fst", s)
      write_fst(df, fname, compress = 99)
    })

    # update choices for interface
    season_choices <- names(hourly_data) |> set_names()
  },
  error = function(e) {
    message("Update from Wisconet failed: ", e$message)
    stns <- read_rds("data/stations.rds")
  }
)

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
