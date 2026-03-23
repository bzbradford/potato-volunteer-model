server <- function(input, output) {
  # bs_themer()

  # Reactives ------------------------------------------------------------------

  ## rv ----
  rv <- reactiveValues(
    selected_stn = "HNCK"
  )

  ## season_data ----
  # filter hourly data for season
  season_data <- reactive({
    season <- req(input$season)
    df <- hourly_data[[season]]
    if (nrow(df) == 0) {
      warning("No hourly data for season '", season, "'")
      req(FALSE)
    }
    df
  })

  ## season_risk ----
  # filter volunteer risk for season
  season_risk <- reactive({
    season <- req(input$season)
    df <- vol_risk[[season]]
    if (nrow(df) == 0) {
      warning("No volunteer risk data for season '", season, "'")
      req(FALSE)
    }
    df
  })

  ## avail_ids ----
  # station ids with data for selected season
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

  ## stn_data ----
  # data for selected station
  stn_data <- reactive({
    stns <- avail_ids() # just to trigger invalidation on change
    stn <- rv$selected_stn
    season_data() |>
      filter(station_id == rv$selected_stn)
  })

  ## selected_stn_risk ----
  # risk data for selected station to display on the map
  selected_stn_risk <- reactive({
    stn <- season_risk() |>
      filter(station_id == rv$selected_stn)
    req(nrow(stn) == 1)
    stn
  })

  # Rendered UI components -----------------------------------------------------

  ## stn_name ----
  output$stn_name <- renderUI({
    stn <- selected_stn_risk()
    nm <- as.character(stn$station_name)
    stn_link <- sprintf(
      "https://wisconet.wisc.edu/stations/%s",
      str_to_snake(nm)
    )
    div(
      style = "display: inline-flex; gap: 10px; align-items: flex-end;",
      strong(sprintf(
        "%s: %s",
        stn$station_id,
        nm
      )),
      span(
        style = "font-size: smaller; color: grey;",
        sprintf("(%.6f°N, %.6f°W)", stn$latitude, stn$longitude)
      ),
      a(href = stn_link, "View station on Wisconet", target = "_blank")
    )
  })

  ## stn_risk ----
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

  ## modal info handler ----
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

  # Map ------------------------------------------------------------------------

  ## render map ----
  output$map <- renderLeaflet({
    isolate(season_risk()) |>
      build_risk_map()
  })

  ## map marker handler ----
  # refresh map markers on data change
  observe({
    risk <- season_risk()

    leafletProxy("map") |>
      clearMarkers() |>
      add_risk_markers(data = risk)
  })

  ## zoom button handler ----
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

  ## marker click handler ----
  # set selected station on marker click
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

  ## air_plot ----
  # air temperature plot
  output$air_plot <- renderPlotly({
    stn_id <- req(rv$selected_stn)
    stn_data() |>
      filter(depth == 0) |>
      build_plot()
  })

  ## soil_plot ----
  # soil temperature plot
  output$soil_plot <- renderPlotly({
    stn_id <- req(rv$selected_stn)
    # build_plotly(stn_data(), stn_id)
    stn_data() |>
      filter(depth > 0) |>
      build_plot()
  })
}
