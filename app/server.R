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

  avail_ids <- reactive({
    data <- season_data()
    avail_ids <- unique(data$station_id)
    cur_id <- rv$selected_stn
    if (!(cur_id %in% avail_ids)) {
      avail_stns <- stns |> filter(station_id %in% avail_ids)
      cur_stn <- stns |> filter(station_id == cur_id)
      closest <- find_closest(cur_stn$latitude, cur_stn$longitude, avail_stns)
      rv$selected_stn <- closest$station_id
    }
    avail_ids
  })

  stn_data <- reactive({
    stns <- avail_ids() # just to trigger invalidation on change
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

  selected_stn_risk <- reactive({
    stn <- season_risk() |>
      filter(station_id == rv$selected_stn)
    req(nrow(stn) > 0)
    stn
  })

  # UI ----
  output$stn_name <- renderUI({
    stn <- selected_stn_risk()
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

  output$stn_risk <- renderUI({
    stn <- selected_stn_risk()
    colorize <- function(val) {
      color <- if (val >= 120) "blue" else "red"
      sprintf('<span style="color: %s">%s</span>', color, val)
    }
    HTML(paste0(
      '<strong>- </strong>',
      '<span style="font-size: smaller">',
      'Killing hours @2in: ',
      colorize(stn$killing2),
      ' - @4in: ',
      colorize(stn$killing4),
      ' - @8in: ',
      colorize(stn$killing8),
      '. Tuber survival likelihood: ',
      stn$risk,
      '</span>'
    ))
  })

  # Show modal ----
  observe({
    mod <- modalDialog(
      includeMarkdown("about.md"),
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
        stn <- selected_stn_risk()
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
    stn <- selected_stn_risk()
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

  # Plots ----
  output$soil_plot <- renderPlotly({
    stn_id <- req(rv$selected_stn)
    # build_plotly(stn_data(), stn_id)
    stn_data() |>
      filter(depth > 0) |>
      build_plot()
  })

  output$air_plot <- renderPlotly({
    stn_id <- req(rv$selected_stn)
    stn_data() |>
      filter(depth == 0) |>
      build_plot()
  })
}
