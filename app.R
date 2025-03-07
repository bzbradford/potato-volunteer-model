
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(bslib)


ui <- page_fillable(
  title = "Potato volunteer risk map",
  padding = 0,
  gap = 0,

  tags$style(".leaflet-control-attribution { display: none; }"),

  tags$header(
    style = "background-color: #c5050c; font-weight: bold; padding: 0.5rem 1rem; font-size: 1.25rem; color: white; display: inline-flex; justify-content: space-between;",
    div("Wisconsin potato volunteer risk map 2024-2025"),
    div(actionLink("info", icon("circle-info"), style = "color: white;"))
  ),

  card_body(
    padding = "1rem",
    layout_columns(
      gap = "1rem",
      card(
        min_height = "600px",
        max_height = "100vw",
        card_header("Risk map"),
        card_body(
          padding = 0,
          leafletOutput("map", height = "100%")
        )
      ),
      card(
        min_height = "600px",
        max_height = "100vw",
        card_header("Soil temperature"),
        card_body(
          padding = 10,
          plotOutput("plot", height = "100%")
        )
      )
    )
  ),

  tags$footer(
    div(
      style = "text-align: right; font-style: italic; font-size: small; padding: 0 1rem 1rem 1rem; color: grey;",
      HTML("Developed by <a href='https://entomology.wisc.edu/directory/ben-bradford/' target='_blank'>Ben Bradford</a>, UW-Madison Entomology<br>"),
      HTML("<a href='https://github.com/bzbradford/potato-volunteer-model' target='_blank'>View source code</a>"),
    )
  )
)

server <- function(input, output) {

  rv <- reactiveValues(
    selected_stn = "HNCK"
  )

  observe({
    mod <- modalDialog(
      includeMarkdown("README.md"),
      footer = modalButton("Close"),
      easyClose = TRUE,
      size = "l"
    )
    showModal(mod)
  }) %>% bindEvent(input$info)

  output$map <- renderLeaflet({
    volunteer_risk %>%
      leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(baseGroups = c("OpenStreetMap", "Satellite")) %>%
      addResetMapButton() %>%
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
  })

  observe({
    rv$selected_stn <- req(input$map_marker_click$id)
  })

  observe({
    id <- req(rv$selected_stn)
    stn <- volunteer_risk %>% filter(station_id == id)
    leafletProxy("map") %>%
      addCircleMarkers(
        data = stn,
        lat = ~latitude,
        lng = ~longitude,
        color = "red",
        weight = 4,
        opacity = 1,
        fill = FALSE,
        layerId = "selected"
      )
  })

  output$plot <- renderPlot({
    id <- req(rv$selected_stn)
    stn <- stns %>% filter(station_id == id)
    temps <- soiltemp %>% filter(station_id == id)
    hours <- temps %>%
      summarize(hours = sum(measure_value <= 27), .by = measure_name) %>%
      mutate(label = paste0(" ", hours, " hours < 27°F"))
    risk <- volunteer_risk %>% filter(station_id == id)
    zerodate <- as.POSIXct(-Inf, origin = min(temps$dttm))

    temps %>%
      ggplot(aes(x = dttm_local, y = measure_value)) +
      geom_line(aes(color = !freezing, group = 1), lwd = .5) +
      geom_line(aes(y = rolling_mean), alpha = .5) +
      geom_hline(yintercept = 32, linetype = "dashed") +
      geom_hline(yintercept = 27) +
      geom_text(data = hours, aes(label = label), x = zerodate, y = Inf, hjust = 0, vjust = 1.5) +
      annotate("text", x = zerodate, y = 32, label = " Freezing (32°F)", hjust = 0, vjust = 1.5, fontface = "italic", size = 3) +
      annotate("text", x = zerodate, y = 27, label = " Killing (27°F)", hjust = 0, vjust = 1.5, fontface = "italic", size = 3) +
      facet_wrap(~measure_name, ncol = 1) +
      scale_x_datetime(breaks = "months", minor_breaks = NULL, labels = ~format(.x, "%b '%y")) +
      labs(
        title = paste(stn$station_name, "soil temperature, winter '24-'25."),
        subtitle = paste("Potato volunteer risk:", risk$risk),
        x = NULL, y = "Soil temperature (°F)"
      ) +
      theme(
        legend.position = "none",
        title = element_text(size = 14),
        strip.text = element_text(size = 12, face = "bold")
      )
  })

}

shinyApp(ui = ui, server = server)
