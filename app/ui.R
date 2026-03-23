ui <- page_fillable(
  theme = bs_theme(bootswatch = "flatly"),
  title = "Potato volunteer risk map",
  padding = 0,
  gap = 0,

  tags$style(
    ".leaflet-control-attribution { display: none; } .form-group { margin-bottom: 0; }"
  ),

  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
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
      style = "font-size: small;",
      HTML(
        "Potato volunteers are tubers left in the field after harvest which may survive the winter and grow during the following season. The tubers themselves may harbor pests and diseases, and the plants that grow from these tubers may increase the risk of serious diseases including late blight (<i>Phytophthora infestans</i>). Using weather data from Wisconet, this app illustrates soil temperatures and the associated risk of potato tuber overwintering. Note: Risk only applies to winter seasons, summer weather included for informational purposes only."
      )
    ),
    div(
      style = "display: flex; flex-direction: row; flex-wrap: wrap; gap: 1rem; align-items: center;",
      tags$label(
        strong("Weather season:"),
        `for` = "season"
      ),
      radioGroupButtons(
        inputId = "season",
        label = NULL,
        choices = season_choices,
        # pick most recent winter season
        selected = season_choices[max(which(str_detect(season_choices, "-")))],
        size = "sm",
        individual = TRUE
      ),
    ),

    layout_columns(
      col_widths = c(5, 7),
      gap = "1rem",
      # map
      card(
        card_header(strong("Risk map")),
        card_body(
          padding = 0,
          leafletOutput("map")
        )
      ),
      # plots
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
