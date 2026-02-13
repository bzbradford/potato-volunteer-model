server <- function(input, output) {
  # bs_themer()

  # Reactives ----

  rv <- reactiveValues(
    selected_stn = "HNCK"
  )

  season_data <- reactive({
    s <- req(input$season)
    hourly_data |>
      filter(season == s)
  })

  avail_stns <- reactive({
    data <- season_data()
    stns <- unique(data$station_id)
    if (!(rv$selected_stn %in% stns)) {
      rv$selected_stn <- sample(stns, 1)
    }
    stns
  })

  stn_data <- reactive({
    stns <- avail_stns() # just to trigger invalidation on change
    stn <- rv$selected_stn
    season_data() |>
      filter(station_id == rv$selected_stn)
  })

  # for map
  season_risk <- reactive({
    s <- req(input$season)
    volunteer_risk |>
      filter(season == s)
  })

  selected_stn <- reactive({
    season_risk() |>
      filter(station_id == rv$selected_stn)
  })

  output$stn_name <- renderUI({
    stn <- selected_stn()
    div(
      style = "display: inline-flex; gap: 10px; align-items: flex-end;",
      strong(sprintf(
        "%s: %s",
        stn$station_id,
        stn$station_name
      )),
      div(
        style = "font-size: smaller; color: grey;",
        sprintf("(%.6f°N, %.6f°W)", stn$latitude, stn$longitude)
      )
    )
  })

  # Show modal ----

  observe({
    mod <- modalDialog(
      includeMarkdown("README.md"),
      footer = modalButton("Close"),
      easyClose = TRUE,
      size = "l"
    )
    showModal(mod)
  }) |>
    bindEvent(input$info)

  # Map ----

  output$map <- renderLeaflet({
    isolate(season_risk()) |>
      build_risk_map()
  })

  observe({
    risk <- season_risk()

    leafletProxy("map") |>
      clearMarkers() |>
      add_risk_markers(data = risk)
  })

  # handle zoom button
  observe({
    action <- req(input$map_btn)
    switch(
      action,
      "zoom" = {
        stn <- selected_stn()
        leafletProxy("map") |>
          setView(lat = stn$latitude, lng = stn$longitude, zoom = 12)
      },
      "reset" = {
        leafletProxy("map") |>
          fitBounds(
            lat1 = min(stns$latitude),
            lat2 = max(stns$latitude),
            lng1 = min(stns$longitude),
            lng2 = max(stns$longitude)
          )
      }
    )
  })

  # handle marker click
  observe({
    rv$selected_stn <- req(input$map_marker_click$id)
  })

  # show selected site
  observe({
    stn <- selected_stn()
    leafletProxy("map") |>
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

  # render plot
  output$soil_plot <- renderPlotly({
    stn_id <- req(rv$selected_stn)
    build_plotly(stn_data(), stn_id)
  })

  output$air_plot <- renderPlotly({
    stn_id <- req(rv$selected_stn)
    stn_data() |>
      filter(depth == 0) |>
      build_plot()
  })
}
