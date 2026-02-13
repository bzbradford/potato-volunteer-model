ui <- page_fillable(
  theme = bs_theme(bootswatch = "flatly"),
  title = "Potato volunteer risk map",
  padding = 0,
  gap = 0,

  tags$style(
    ".leaflet-control-attribution { display: none; } .form-group { margin-bottom: 0; }"
  ),

  tags$header(
    style = "background-color: #c5050c; font-weight: bold; padding: 0.5rem 1rem; font-size: 1.25rem; color: white; display: inline-flex; justify-content: space-between;",
    div("Wisconsin Potato Volunteer Risk Map"),
    div(actionLink(
      inputId = "info",
      label = span(
        style = "display: inline-flex; gap: 10px; align-items: center;",
        icon("circle-info"),
        "More information"
      ),
      style = "color: white; text-decoration: none; font-size: medium;"
    ))
  ),

  card(
    class = "border-0",
    min_height = "600px",
    div(
      style = "display: flex; flex-direction: row; flex-wrap: wrap; gap: 1rem; align-items: end;",
      radioGroupButtons(
        inputId = "season",
        label = HTML(
          "<strong>Weather season:</strong>"
        ),
        choices = OPTS$season_choices,
        selected = last(OPTS$season_choices),
        size = "sm",
        individual = TRUE
      ),
      div(
        style = "font-size: smaller;",
        "Note: Potato volunteer risk applies only to winter seasons. Summer weather provided for informational purposes."
      ),
    ),

    layout_columns(
      col_widths = c(5, 7),
      gap = "1rem",
      # map
      card(
        # min_height = "600px",
        # min_height = "40vw",
        card_header(strong("Risk map")),
        card_body(
          padding = 0,
          leafletOutput("map")
        )
      ),
      # plots
      card(
        # min_height = "600px",
        card_header(
          div(
            style = "min-height: 24px;",
            uiOutput("stn_name")
          )
        ),
        card_body(
          padding = 10,
          strong("Air Temperature"),
          plotlyOutput("air_plot"),
          span(strong("Soil temperature"), uiOutput("stn_risk")),
          plotlyOutput("soil_plot")
        )
      )
    ),

    tags$footer(
      div(
        style = "text-align: right; font-style: italic; font-size: small; color: grey;",
        HTML(
          "Developed by <a href='https://entomology.wisc.edu/directory/ben-bradford/' target='_blank'>Ben Bradford</a>, UW-Madison Entomology. <a href='https://github.com/bzbradford/potato-volunteer-model' target='_blank'>View source code</a>. Weather data sourced from <a href='https://wisconet.wisc.edu' target='_blank'>Wisconet</a>."
        )
      )
    )
  )
)
