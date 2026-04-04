server <- function(input, output, session) {
  # bs_themer()

  # Reactives ------------------------------------------------------------------

  ## rv ----
  rv <- reactiveValues(
    selected_stn = "HNCK",
    hourly_data = list(),
    status = NULL
  )

  ## season_data ----
  # lazy-load data
  observe({
    season <- req(input$season)
    df <- rv$hourly_data[[season]]
    if (is.null(df)) {
      df <- load_data(index$path[index$season == season])
      if (nrow(df) == 0) {
        warning("No hourly data for season '", season, "'")
      } else {
        rv$hourly_data[[season]] <- df
      }
    }
  })

  # filter hourly data for season
  season_data <- reactive({
    season <- req(input$season)
    req(rv$hourly_data[[season]])
  })

  ## season_risk ----
  # compute volunteer risk for the selected season
  season_risk <- reactive({
    season_data() |>
      calc_vol_risk()
  })

  ## avail_ids ----
  # check if selected station is still available, otherwise move to closest
  observe({
    stn_id <- rv$selected_stn
    data <- season_data()
    avail_ids <- unique(data$station_id)
    if (!(stn_id %in% avail_ids)) {
      avail_stns <- stns |> filter(station_id %in% avail_ids)
      cur_stn <- stns |> filter(station_id == stn_id)
      closest <- find_closest(cur_stn$latitude, cur_stn$longitude, avail_stns)
      rv$selected_stn <- closest$station_id
    }
  })

  ## stn_data ----
  # data for selected station
  stn_data <- reactive({
    stn_id <- rv$selected_stn
    df <- season_data() |>
      filter(station_id == stn_id)
    req(nrow(df) > 0)
    df
  })

  ## selected_stn_risk ----
  # station loc and risk data to display on the map
  stn_risk <- reactive({
    stn_id <- rv$selected_stn
    df <- season_risk() |>
      filter(station_id == stn_id)
    req(nrow(df) == 1)
    df
  })

  # Wisconet update ------------------------------------------------------------

  # background task to get new data
  updater_task <- ExtendedTask$new(function() {
    mirai(update_from_wisconet(), .GlobalEnv)
  })

  # invoke on app load
  observeEvent(TRUE, {
    rv$status <- "Updating from Wisconet..."
    updater_task$invoke()
  })

  # handle task resolution by clearing any updated seasons, triggering reload
  observe({
    switch(
      updater_task$status(),
      "success" = {
        # capture updated seasons from task
        res <- updater_task$result()

        # clear those seasons triggering data reload
        lapply(res, function(season) {
          rv$hourly_data[[season]] <- NULL
        })

        # set status
        rv$status <- paste(
          "Data updated:",
          format(now(TZ), "%Y-%m-%d %I:%M %p %Z")
        )
      },
      "error" = {
        tryCatch(
          updater_task$result(),
          error = function(e) {
            rv$status <- paste("Data update failed:", e$message)
          }
        )
      }
    )
  }) |>
    bindEvent(updater_task$status())

  # Modal popup handler --------------------------------------------------------

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

  ## Render map ----
  output$map <- renderLeaflet({
    build_map()
  })

  ## Map marker handler ----
  # refresh map markers on data change
  observe({
    risk <- season_risk()
    leafletProxy("map") |>
      add_risk_markers(data = risk)
  })

  ## Zoom button handler ----
  observe({
    switch(
      req(input$map_btn),
      "zoom" = {
        stn <- stn_risk()
        leafletProxy("map") |>
          setView(lat = stn$latitude, lng = stn$longitude, zoom = 12)
      },
      "reset" = {
        leafletProxy("map") |>
          fit_stns()
      }
    )
  })

  ## Marker click handler ----
  # set selected station on marker click
  observe({
    rv$selected_stn <- req(input$map_marker_click$id)
  })

  ## Render selected site ----
  observe({
    stn <- stn_risk()
    leafletProxy("map") |>
      clearGroup("selected") |>
      addCircleMarkers(
        data = stn,
        lat = ~latitude,
        lng = ~longitude,
        color = "red",
        weight = 3,
        opacity = 1,
        fill = FALSE,
        group = "selected",
        options = pathOptions(pane = "selected")
      )
  })

  # Plots ----------------------------------------------------------------------

  ## stn_name ----
  output$stn_heading <- renderUI({
    stn <- stn_risk()

    nm <- as.character(stn$station_name)
    stn_link <- sprintf(
      "https://wisconet.wisc.edu/stations/%s",
      str_to_snake(nm)
    )

    div(
      style = "width: 100%; display: inline-flex; justify-content: space-between;",
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
        a(
          href = stn_link,
          title = stn_link,
          "View station on Wisconet",
          target = "_blank"
        ),
      ),
      div(
        downloadButton(
          "download",
          label = NULL,
          style = "margin: 0; padding: 0 5px;",
          title = "Download this data"
        )
      )
    )
  }) |>
    bindCache(stn_risk())

  # Download handler ----
  output$download <- downloadHandler(
    filename = function() {
      stn <- stn_risk()
      sprintf("%s %s.csv", stn$station_name, input$season)
    },
    content = function(file) {
      stn_data() |>
        select(c(
          station_id,
          station_name,
          latitude,
          longitude,
          dttm = dttm_local,
          standard_name,
          measure_value
        )) |>
        pivot_wider(names_from = standard_name, values_from = measure_value) |>
        mutate(
          date = format(dttm, "%Y-%m-%d"),
          date_mdy = format(dttm, "%m/%d/%Y"),
          hour = hour(dttm),
          .after = dttm
        ) |>
        mutate(across(dttm, format)) |>
        write_csv(file, na = "", append = TRUE, col_names = TRUE)
    }
  )

  ## stn_risk ----
  output$stn_risk <- renderUI({
    stn <- stn_risk()
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
  }) |>
    bindCache(stn_risk())

  ## air_plot ----
  output$air_plot <- renderPlotly({
    stn_data() |>
      filter(depth == 0) |>
      build_plot()
  }) |>
    bindCache(stn_data())

  ## soil_plot ----
  output$soil_plot <- renderPlotly({
    stn_data() |>
      filter(depth > 0) |>
      build_plot()
  }) |>
    bindCache(stn_data())

  # Footer ---------------------------------------------------------------------

  ## update_status ----
  output$update_status <- renderUI({
    span(
      style = "font-style: italic; color: grey;",
      req(rv$status)
    )
  })
} # end server
