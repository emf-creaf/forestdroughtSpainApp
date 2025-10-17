#' @title mod_mapOutput and mod_map
#'
#' @description Shiny module to generate the map with mabox and mapdeck
#'
#' @param id shiny id
#'
#' @export
mod_mapOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  shiny::tagList(mapdeck::mapdeckOutput(ns("output_map"), height = 600))
}

#' mod_map server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param user_inputs reactiveValues containing the user selected inputs
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_mapOutput
mod_map <- function(
  input, output, session,
  user_inputs,
  lang
) {
  # get the ns
  ns <- session$ns

  # hostess ready
  hostess_map <- waiter::Hostess$new(infinite = TRUE)
  hostess_map$set_loader(waiter::hostess_loader(
    svg = "images/hostess_image.svg",
    progress_type = "fill",
    fill_direction = "ltr"
  ))

  # output base map, later we update it
  output$output_map <- mapdeck::renderMapdeck({
    mapdeck::mapdeck(
      ## debug
      # show_view_state = TRUE,
      # style = mapdeck::mapdeck_style('dark'),
      # style = "mapbox://styles/mapbox/dark-v10",
      style = mapdeck::mapdeck_style("dark"),
      location = c(-3.622, 40.234), zoom = 5.1, pitch = 0, max_pitch = 60,
      # debug info (TRUE)
      show_view_state = FALSE
    )
  })

  # update the data on input change
  bitmap_data <- shiny::reactive({
    # only run when inputs are populated
    shiny::validate(
      # shiny::need(user_inputs$user_var, "Missing meteo variable"),
      shiny::need(user_inputs$user_date, "Missing date")
    )

    # show hostess
    waiter_map <- waiter::Waiter$new(
      id = ns('output_map'),
      html = shiny::tagList(
        hostess_map$get_loader(),
        shiny::br(),
        shiny::p(glue::glue(
          "{translate_app('getting_data_for', lang())} {user_inputs$user_date}"
        )),
        shiny::p(translate_app("please_wait", lang()))
      ),
      color = '#E8EAEB'
    )
    waiter_map$show()
    on.exit(waiter_map$hide(), add = TRUE)
    hostess_map$start()
    on.exit(hostess_map$close(), add = TRUE)

    # needed inputs
    var_sel <- user_inputs$user_var
    date_sel <- user_inputs$user_date |>
      as.character() |>
      stringr::str_remove_all("-")

    # arrow sink
    arrow_sink <- arrow::S3FileSystem$create(
      access_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
      secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      scheme = "https",
      endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"),
      region = ""
    )$cd("forestdrought-spain-app-pngs")

    # arrow::open_dataset(Sys.getenv("PARQUET_BITMAPS")) |>
    arrow::open_dataset(
      arrow_sink,
      factory_options = list(
        selector_ignore_prefixes = c("daily_medfateland_bitmaps.parquet")
      )
    ) |>
      dplyr::filter(date == date_sel) |>
      dplyr::as_tibble()
  }) |>
    shiny::bindCache(
      # user_inputs$user_var,
      user_inputs$user_date,
      cache = "session"
    ) |>
    shiny::bindEvent(
      # user_inputs$user_var,
      user_inputs$user_date
    )

  ts_point_data <- shiny::reactive({
    # validate inputs
    shiny::validate(
      shiny::need(user_inputs$user_latitude, "Missing latitude"),
      shiny::need(user_inputs$user_longitude, "Missing longitude")
    )

    data.frame(
      lat = user_inputs$user_latitude,
      lon = user_inputs$user_longitude,
      fill = "#ED51C1FF",
      id = "selected_coords",
      tooltip_translated = translate_app("map_tooltip", lang())
    )
  })

  # Updating the map
  shiny::observe({
    # get the data
    var_sel <- user_inputs$user_var
    bitmap_sel <- bitmap_data() |>
      dplyr::filter(var == var_sel)
    ts_point_sel <- ts_point_data()

    # validate we have data, send an alert to the user if not
    shiny::validate(
      shiny::need(
        validate_rows_with_alert(bitmap_sel, lang),
        "no data for date and var selected"
      )
    )

    # create the custom legend to show with the bitmap
    legend_js <- mapdeck::legend_element(
      variables = rev(round(seq(
        bitmap_sel[["min_value"]],
        bitmap_sel[["max_value"]],
        length.out = 5
      ), 0)),
      colours = scales::col_numeric(
        hcl.colors(10, "ag_GrnYl", alpha = 0.8),
        c(bitmap_sel[["min_value"]], bitmap_sel[["max_value"]]),
        na.color = "#FFFFFF00", reverse = TRUE, alpha = TRUE
      )(seq(
        bitmap_sel[["min_value"]],
        bitmap_sel[["max_value"]],
        length.out = 5
      )),
      colour_type = "fill", variable_type = "gradient",
      title = glue::glue(
        "{translate_app(user_inputs$user_var, lang())} - {user_inputs$user_date}"
      )
    ) |>
      mapdeck::mapdeck_legend()

    # update the map
    mapdeck::mapdeck_update(map_id = ns("output_map")) |>
      mapdeck::add_bitmap(
        image = bitmap_sel$base64_string, layer_id = "bitmap_sel",
        bounds = c(
          bitmap_sel$left_ext, bitmap_sel$down_ext - 0.085,
          bitmap_sel$right_ext, bitmap_sel$up_ext - 0.085
        ),
        update_view = FALSE, focus_layer = FALSE,
        transparent_colour = "#00000000"
      ) |>
      mapdeck::add_scatterplot(
        data = ts_point_sel,
        lon = "lon", lat = "lat", id = "id",
        fill_colour = "fill", tooltip = "tooltip_translated",
        radius = 500, radius_min_pixels = 5, radius_max_pixels = 10,
        update_view = FALSE, focus_layer = FALSE
      ) |>
      mapdeck::add_legend(legend = legend_js, layer_id = "custom_legend")
  })
}