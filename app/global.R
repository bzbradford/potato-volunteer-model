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
  library(shiny)
  library(shinyWidgets)
  library(bslib)
  library(plotly)
  library(fst)
})

# shiny::devmode(TRUE)

# Load data ----

wi_counties <- read_sf("data/wi-county-bounds-24k.geojson")
stns <- readRDS("data/stns.rds")
select_measures <- readRDS("data/measures.rds")
hourly_data <- read_fst("data/hourly_data.fst") |> as_tibble()

#' Potato volunteer survival model
#' Soil temperatures must be below 27F for 120 hours
#' High risk: 2in & 4in < 120 hours
#' Moderate risk: 2in > 120 hours, 4in < 120 hours
#' Low risk 2in & 4in > 120 hours

volunteer_risk <- hourly_data %>%
  summarize(
    killing = sum(killing, na.rm = TRUE),
    .by = c(station_id, station_name, latitude, longitude, season, depth)
  ) %>%
  complete(
    nesting(season, station_id, station_name, latitude, longitude),
    depth,
    fill = list(killing = 0)
  ) %>%
  pivot_wider(
    names_from = depth,
    names_glue = "killing{.name}",
    values_from = killing
  ) %>%
  mutate(
    risk_score = (killing2 >= 120) + (killing4 >= 120),
    risk = case_match(
      risk_score,
      0 ~ "High",
      1 ~ "Moderate",
      2 ~ "Low"
    ) %>%
      factor(levels = c("High", "Moderate", "Low"))
  ) %>%
  arrange(season, station_id)


OPTS <- list(
  season_choices = levels(hourly_data$season)
)


# Map --------------------------------------------------------------------------

#' @param data:tibble volunteer_risk
build_risk_map <- function(data) {
  btn1 <- easyButton(
    position = "topleft",
    icon = "fa-location-pin",
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
    add_risk_markers()
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
  volunteer_risk %>%
    filter(season == "2025-2026") %>%
    build_risk_map()

  hourly_data %>%
    filter(station_id == "KNGT")
}


# Plots ------------------------------------------------------------------------

## Single plot ----

expand_range <- function(lo, hi, amt = .05) {
  c(lo - abs(hi - lo) * amt, hi + abs(hi - lo) * amt)
}

# plotly
build_plot <- function(data) {
  require(plotly)

  yrange <- if (nrow(data) == 0) {
    c(0, 100)
  } else {
    expand_range(
      lo = min(data$measure_value, na.rm = TRUE),
      hi = max(data$measure_value, na.rm = TRUE)
    )
  }

  data <- data |>
    arrange(dttm) |>
    mutate(
      rolling_mean = zoo::rollapply(
        measure_value,
        24,
        \(x) mean(x, na.rm = TRUE),
        partial = TRUE
      ),
      .by = c(station_id, standard_name)
    )

  plot_ly(data, x = ~dttm_local) %>%
    add_lines(
      y = ~measure_value,
      line = list(color = "steelblue", width = 1),
      name = "Hourly temp",
      showlegend = FALSE,
      hovertemplate = "%{y:.1f}°F"
    ) %>%
    add_lines(
      y = ~rolling_mean,
      line = list(color = "black", width = 2),
      opacity = 0.5,
      name = "24-hr mean",
      showlegend = FALSE,
      hovertemplate = "%{y:.1f}°F"
    ) %>%
    layout(
      margin = list(t = 10, r = 10, b = 10, l = 10),
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
  hourly_data %>%
    filter(station_id == "HNCK", depth == 2) %>%
    build_plot()

  hourly_data %>%
    filter(station_id == "HNCK", depth == 0) %>%
    build_plot()
}


## Stacked plot ----

build_plotly <- function(data, stn_id) {
  stn <- stns %>% filter(station_id == stn_id)
  risk <- volunteer_risk %>% filter(station_id == stn_id)
  stn_data <- data %>% filter(station_id == stn_id)
  measures <- select_measures

  killing_hrs <- stn_data %>%
    filter(depth > 0) %>%
    summarize(hrs = sum(measure_value < 27, na.rm = TRUE), .by = measure_name)

  p1 <- stn_data |>
    filter(depth == 2) |>
    build_plot()
  p2 <- stn_data |>
    filter(depth == 4) |>
    build_plot()

  subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
    layout(
      margin = list(t = 20, r = 10, b = 10, l = 10),
      annotations = list(
        # subplot 1 title
        list(
          text = paste0(
            "<b>2-inch soil temperature</b> - ",
            killing_hrs$hrs[1],
            " hrs below 27°F"
          ),
          x = 0.5,
          y = 1.0,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 11),
          xanchor = "center",
          yanchor = "bottom"
        ),
        # subplot 2 title
        list(
          text = paste0(
            "<b>4-inch soil temperature</b> - ",
            killing_hrs$hrs[2],
            " hrs below 27°F"
          ),
          x = 0.5,
          y = 0.48,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 11),
          xanchor = "center",
          yanchor = "bottom"
        )
      )
    )
}

if (FALSE) {
  hourly_data %>%
    filter(season == "2025-2026") %>%
    build_plotly("ANGO")
}
