ui <- page_fillable(
  theme = bs_theme(bootswatch = "flatly"),
  title = "Potato volunteer risk map",
  padding = 0,
  gap = 0,

  tags$style(".leaflet-control-attribution { display: none; }"),

  tags$header(
    style = "background-color: #c5050c; font-weight: bold; padding: 0.5rem 1rem; font-size: 1.25rem; color: white; display: inline-flex; justify-content: space-between;",
    div("Wisconsin Potato Volunteer Risk Map"),
    div(actionLink("info", icon("circle-info"), style = "color: white;"))
  ),

  card_body(
    padding = "1rem",
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
    span(
      style = "font-size:smaller;",
      "Note: Potato volunteer risk applies only to winter seasons. Summer weather provided for informational purposes."
    ),
    layout_columns(
      gap = "1rem",
      card(
        card_header(strong("Risk map")),
        card_body(
          padding = 0,
          leafletOutput("map", height = "50vh")
        )
      ),
      card(
        card_header(
          div(
            style = "min-height: 24px;",
            uiOutput("stn_name")
          )
        ),
        card_body(
          padding = 10,
          strong("Air Temperature"),
          plotlyOutput("air_plot", height = "400px"),
          strong("Soil temperature"),
          plotlyOutput("soil_plot", height = "800px")
        )
      )
    )
  ),

  tags$footer(
    div(
      style = "text-align: right; font-style: italic; font-size: small; padding: 0 1rem 1rem 1rem; color: grey;",
      HTML(
        "Developed by <a href='https://entomology.wisc.edu/directory/ben-bradford/' target='_blank'>Ben Bradford</a>, UW-Madison Entomology. <a href='https://github.com/bzbradford/potato-volunteer-model' target='_blank'>View source code</a>. Weather data sourced from <a href='https://wisconet.wisc.edu' target='_blank'>WiscoNet</a>."
      )
    )
  )
)
