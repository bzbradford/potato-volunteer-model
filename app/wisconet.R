require(tidyverse)
require(httr2)

Wisconet <- R6::R6Class(
  "Wisconet",
  private = list(
    api_url = "https://wisconet.wisc.edu/api/v1",

    # Convert local time to GMT unix timestamp
    time_to_gmt = function(t) {
      as.numeric(with_tz(t, "GMT"))
    },

    # Build an httr2 request for a single station
    build_request = function(stn_id, fields, start_time, end_time) {
      request(str_glue("{private$api_url}/stations/{stn_id}/measures")) |>
        req_url_query(
          start_time = private$time_to_gmt(start_time),
          end_time = private$time_to_gmt(end_time),
          fields = paste(fields, collapse = ",")
        ) |>
        req_throttle(capacity = 60, fill_time_s = 60) |>
        req_retry(max_tries = 5) |>
        req_error(is_error = ~FALSE)
    },

    # Parse the fieldlist metadata from a response
    parse_fieldlist = function(fieldlist) {
      fieldlist |>
        enframe() |>
        select(-name) |>
        unnest_wider(value) |>
        rename(measure_id = id) |>
        select(where(~ !all(is.na(.x) | .x == "")))
    },

    # Parse the data array from a response
    parse_data = function(data) {
      data |>
        enframe() |>
        select(-name) |>
        unnest_wider(value) |>
        mutate(
          dttm = as_datetime(collection_time),
          date = as_date(dttm, tz = self$timezone),
          .after = collection_time
        ) |>
        unnest_longer(measures) |>
        unnest_wider(measures, names_sep = "_") |>
        rename(measure_id = measures_1, measure_value = measures_2)
    },

    # Parse a single response, joining station metadata
    parse_response = function(resp, stn_id) {
      if (resp_is_error(resp)) {
        message("  FAIL [", stn_id, "]: HTTP ", resp_status(resp))
        return(tibble())
      }

      body <- resp_body_json(resp)

      if (length(body[["data"]]) == 0) {
        message("  No data for ", stn_id)
        return(tibble())
      }

      resp_fields <- private$parse_fieldlist(body$fieldlist)
      resp_data <- private$parse_data(body$data)
      resp_joined <- resp_data |> left_join(resp_fields, join_by(measure_id))

      stn <- self$stations |> filter(station_id == stn_id)
      data <- tibble(station_id = stn_id) |> bind_cols(resp_joined)
      stn |> left_join(data, join_by(station_id))
    },

    # Validate inputs, lazily fetching metadata if needed
    validate_inputs = function(stn_ids, fields) {
      if (is.null(self$stations)) {
        self$refresh_stations()
      }
      if (is.null(self$fields)) {
        self$refresh_fields()
      }
      bad_stns <- setdiff(stn_ids, self$stations$station_id)
      if (length(bad_stns) > 0) {
        stop("Unknown station(s): ", paste(bad_stns, collapse = ", "))
      }
      bad_fields <- setdiff(fields, self$fields$standard_name)
      if (length(bad_fields) > 0) {
        stop("Unknown field(s): ", paste(bad_fields, collapse = ", "))
      }
    }
  ),

  public = list(
    stations = NULL,
    fields = NULL,
    timezone = NULL,

    initialize = function(timezone = Sys.timezone(), fetch_on_init = TRUE) {
      self$timezone <- timezone
      if (fetch_on_init) {
        self$refresh_stations()
        self$refresh_fields()
      }
    },

    refresh_stations = function() {
      self$stations <- str_glue("{private$api_url}/stations") |>
        request() |>
        req_perform() |>
        resp_body_json(simplifyVector = TRUE) |>
        as_tibble() |>
        mutate(across(earliest_api_date, mdy)) |>
        select(-c(id, campbell_cloud_id, legacy_id, station_slug)) |>
        filter(!grepl("TEST", station_id))
      invisible(self)
    },

    refresh_fields = function() {
      self$fields <- str_glue("{private$api_url}/fields") |>
        request() |>
        req_perform() |>
        resp_body_json(simplifyVector = TRUE) |>
        as_tibble() |>
        select(where(~ !all(is.na(.x) | .x == ""))) |>
        arrange(measure_type, standard_name)
      invisible(self)
    },

    #' Get measures for a single station
    get_measures = function(stn_id, fields, start_time, end_time = now()) {
      private$validate_inputs(stn_id, fields)
      message("GET ==> ", stn_id, ": ", start_time, " to ", end_time)

      req <- private$build_request(stn_id, fields, start_time, end_time)
      t <- now()
      resp <- req_perform(req)
      result <- private$parse_response(resp, stn_id)

      if (nrow(result) > 0) {
        n_obs <- length(unique(result$collection_time))
        elapsed <- as.numeric(difftime(now(), t, units = "secs"))
        message(sprintf("  Received %s observations in %.3fs", n_obs, elapsed))
      }

      result
    },

    #' Get measures for all active stations in parallel
    get_measures_all = function(
      fields,
      start_time,
      end_time = now()
    ) {
      if (is.null(self$stations)) {
        self$refresh_stations()
      }
      if (is.null(self$fields)) {
        self$refresh_fields()
      }

      active_stns <- self$stations |> filter(earliest_api_date <= end_time)
      stn_ids <- active_stns$station_id

      self$get_measures_stations(stn_ids, fields, start_time, end_time)
    },

    #' Get measures for a specific set of stations in parallel
    get_measures_stations = function(
      stn_ids,
      fields,
      start_time,
      end_time = now()
    ) {
      private$validate_inputs(stn_ids, fields)

      message("Fetching ", length(stn_ids), " stations")

      reqs <- if (length(start_time) > 1) {
        map(
          stn_ids,
          ~ private$build_request(.x, fields, start_time[[.x]], end_time)
        )
      } else {
        map(
          stn_ids,
          ~ private$build_request(.x, fields, start_time, end_time)
        )
      }

      t <- now()
      resps <- req_perform_parallel(
        reqs,
        on_error = "continue",
        progress = "Fetching station data",
        max_active = 10
      )

      results <- map2(
        resps,
        stn_ids,
        ~ private$parse_response(.x, .y),
        .progress = "Parsing responses"
      )
      combined <- bind_rows(results)

      elapsed <- as.numeric(difftime(now(), t, units = "secs"))
      n_stns <- sum(map_int(results, nrow) > 0)
      message(sprintf(
        "  Done: %i/%i stations returned data in %.1fs",
        n_stns,
        length(stn_ids),
        elapsed
      ))

      combined
    },

    #' Display stations on a leaflet map
    map_stations = function() {
      if (is.null(self$stations)) {
        self$refresh_stations()
      }
      leaflet::leaflet(self$stations) |>
        leaflet::addTiles() |>
        leaflet::addMarkers(
          lng = ~longitude,
          lat = ~latitude,
          label = ~ paste0(station_id, ": ", station_name)
        )
    },

    print = function(...) {
      n_stns <- if (!is.null(self$stations)) nrow(self$stations) else "?"
      n_fields <- if (!is.null(self$fields)) nrow(self$fields) else "?"
      cat("<Wisconet API>\n")
      cat("  Timezone:", self$timezone, "\n")
      cat("  Stations:", n_stns, "\n")
      cat("  Fields:", n_fields, "\n")
      invisible(self)
    }
  )
)
