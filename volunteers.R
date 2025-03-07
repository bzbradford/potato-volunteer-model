
library(tidyverse)
library(leaflet)
library(sf)

#' Potato volunteer survival model
#' Soil temperatures must be below 27F for 120 hours
#' High risk: 2in & 4in < 120 hours
#' Moderate risk: 2in > 120 hours, 4in < 120 hours
#' Low risk 2in & 4in > 120 hours


wi_counties <- unzip("data/wi-county-bounds-24k.zip") %>% read_sf()

soil_measures <- tibble(
  measure_name = c("2-inch soil temperature", "4-inch soil temperature"),
  standard_name = c("60min_soil_temp_f_avg@2in", "60min_soil_temp_f_avg@4in"),
  depth = c(2, 4)
)

soiltemp <- read_csv("data/soil_temps_hourly_2024.11.01_2025.03.06.csv.gz") %>%
  mutate(dttm_local = with_tz(dttm, "America/Chicago")) %>%
  mutate(freezing = measure_value < 32, killing = measure_value < 27) %>%
  mutate(rolling_mean = zoo::rollapply(measure_value, 24, mean, partial = TRUE), .by = c(station_id, standard_name)) %>%
  left_join(soil_measures)

volunteer_risk <- soiltemp %>%
  summarize(
    killing = sum(killing),
    .by = c(station_id, station_name, latitude, longitude, depth)
  ) %>%
  pivot_wider(names_from = depth, names_glue = "killing{.name}", values_from = killing) %>%
  mutate(risk = case_match(
    (killing2 >= 120) + (killing4 >= 120),
    0 ~ "High", 1 ~ "Moderate", 2 ~ "Low"
  ) %>% factor(levels = c("High", "Moderate", "Low"))) %>%
  arrange(killing4)

stns <- soiltemp %>%
  summarize(
    hours = n_distinct(collection_time),
    .by = c(station_id, station_name, latitude, longitude)
  )


# Risk maps ---------------------------------------------------------------

## Leaflet ----

volunteer_risk %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addLayersControl(baseGroups = c("OpenStreetMap", "Satellite")) %>%
  addPolygons(
    data = wi_counties,
    color = "black",
    weight = .25,
    fillColor = "white",
    fillOpacity = .0001,
    label = ~paste(county_name, "County")
  ) %>%
  addCircleMarkers(
    lat = ~latitude,
    lng = ~longitude,
    layerId = ~station_id,
    label = ~str_glue("<b>{station_name}</b><br>Risk: {risk}") %>% lapply(HTML),
    color = "black",
    weight = 1,
    opacity = 1,
    fillColor = ~colorFactor("viridis", risk, reverse = TRUE)(risk),
    fillOpacity = 1
  )

## ggplot

risk_pts <- volunteer_risk %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)

ggplot() +
  geom_sf(data = risk_pts) +
  ggspatial::annotation_map_tile("cartolight", zoom = 8) +
  geom_sf(data = wi_counties, lwd = .1, fill = NA) +
  geom_sf(data = risk_pts, aes(fill = risk), shape = 21, size = 5) +
  scale_fill_viridis_d(direction = -1) +
  labs(fill = "Risk") +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.1, .1),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.margin = margin(5, 5, 5, 5)
  )
ggsave("plots/Volunteer survival risk map, 2025.png", h = 8, w = 6)


# Soil temp plots ---------------------------------------------------------

## Hancock ----

soiltemp %>%
  filter(station_name == "Hancock") %>%
  summarize(
    hours = n(),
    freezing = sum(freezing),
    killing = sum(killing)
  )

soiltemp %>%
  filter(station_name == "Hancock") %>%
  ggplot(aes(x = dttm_local, y = measure_value)) +
  geom_line(aes(color = !freezing, group = 1), lwd = .5) +
  geom_line(aes(y = rolling_mean), alpha = .5) +
  geom_hline(yintercept = 32, linetype = "dashed") +
  geom_hline(yintercept = 27) +
  annotate("text", x = as.POSIXct(-Inf, origin = '2024-11-1'), y = 32, label = " Freezing (32F)", hjust = 0, vjust = 1.5, fontface = "italic", size = 3) +
  annotate("text", x = as.POSIXct(-Inf, origin = '2024-11-1'), y = 27, label = " Minimum for volunteer survival (27F)", hjust = 0, vjust = 1.5, fontface = "italic", size = 3) +
  facet_wrap(~measure_name) +
  scale_x_datetime(breaks = "months", minor_breaks = NULL, labels = ~format(.x, "%b '%y")) +
  labs(title = "Hancock soil temperature, winter 2024-2025", x = "Date", y = "Soil temperature (F)") +
  theme(legend.position = "none")


## All stations ----

lapply(1:nrow(stns), function(i) {
  stn <- stns[i,]
  message(stn$station_id)
  temps <- soiltemp %>% filter(station_id == stn$station_id)
  hours <- temps %>%
    summarize(hours = sum(measure_value <= 27), .by = measure_name) %>%
    mutate(label = paste0(" ", hours, " hours < 27°F"))
  risk <- volunteer_risk %>% filter(station_id == stn$station_id)
  zerodate <- as.POSIXct(-Inf, origin = '2024-11-1')

  plt <- temps %>%
    ggplot(aes(x = dttm_local, y = measure_value)) +
    geom_line(aes(color = !freezing, group = 1), lwd = .5) +
    geom_line(aes(y = rolling_mean), alpha = .5) +
    geom_hline(yintercept = 32, linetype = "dashed") +
    geom_hline(yintercept = 27) +
    geom_text(data = hours, aes(label = label), x = zerodate, y = Inf, hjust = 0, vjust = 1.5) +
    annotate("text", x = zerodate, y = 32, label = " Freezing (32°F)", hjust = 0, vjust = 1.5, fontface = "italic", size = 3) +
    annotate("text", x = zerodate, y = 27, label = " Killing (27°F)", hjust = 0, vjust = 1.5, fontface = "italic", size = 3) +
    facet_wrap(~measure_name) +
    scale_x_datetime(breaks = "months", minor_breaks = NULL, labels = ~format(.x, "%b '%y")) +
    labs(
      title = paste(stn$station_name, "soil temperature, winter '24-'25. Potato volunteer risk:", risk$risk),
      x = "Date", y = "Soil temperature (°F)"
    ) +
    theme(legend.position = "none", strip.text = element_text(size = 12, face = "bold"))
  pltname <- paste0("plots/", risk$risk, " - ", stn$station_id, " ", stn$station_name, " soil temp and volunteer risk, winter '24-'25.png")
  ggsave(pltname, plt, h = 5, w = 8, s = 1.25)
  NULL
})

