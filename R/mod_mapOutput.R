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
  shiny::tagList(
    bslib::layout_sidebar(
      mapdeck::mapdeckOutput(ns("output_map"), width = "100%", height = "600px"),
      sidebar = bslib::sidebar(
        shiny::uiOutput(ns('inputs_map')),
        class = "inputs_map",
        open = list(desktop = "open", mobile = "always-above")
      )
    )
  )
}

#' mod_map server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param arrow_sink bucket s3 filesystem
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_mapOutput
mod_map <- function(
  input, output, session,
  arrow_sink,
  lang
) {
  # get the ns
  ns <- session$ns

  # map inputs
  output$inputs_map <- shiny::renderUI({
    # precalculated choices
    user_var_choices <- c(
      "Theta",
      "Psi",
      "REW",
      "ELW",
      "Precipitation",
      "PET",
      "AET",
      "LAI",
      "DDS",
      "LFMC"
    ) |>
      purrr::set_names(translate_app(c(
        "Theta",
        "Psi",
        "REW",
        "ELW",
        "Precipitation",
        "PET",
        "AET",
        "LAI",
        "DDS",
        "LFMC"
      ), lang()))

    user_date_choices <- seq(Sys.Date() - 370, Sys.Date() - 5, by = "day") |>
      as.Date(format = '%j', origin = as.Date('1970-01-01')) |>
      as.character()

    # tagList creating the draggable absolute panel
    shiny::tagList(
      # first row of inputs, variable and dates
      shiny::h4(translate_app("map_controls", lang())),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          # user_var
          shinyWidgets::pickerInput(
            ns("user_var"), label = translate_app("user_var", lang()),
            choices = user_var_choices,
            selected = user_var_choices[1],
            multiple = FALSE,
            options = shinyWidgets::pickerOptions(
              actionsBox = FALSE,
              tickIcon = "glyphicon-ok-sign"
            )
          ),
          shiny::br(),
          # user_date
          shinyWidgets::airDatepickerInput(
            ns("user_date"), label = translate_app("user_date", lang()),
            value = user_date_choices[length(user_date_choices)],
            multiple = FALSE, range = FALSE,
            minDate = user_date_choices[1],
            maxDate = user_date_choices[length(user_date_choices)],
            firstDay = 1, autoClose = TRUE, addon = "none"
          ),
          shiny::br(),
          # aggregation input
          shinyWidgets::materialSwitch(
            ns("user_agg"), label = translate_app("user_agg", lang()),
            value = FALSE, status = "info"
          ),
          shiny::br(), shiny::br(),
          # download
          shiny::wellPanel(
            shiny::h4(translate_app("download_maps_title", lang())),
            shiny::p(translate_app("download_maps_text", lang())),
            shiny::actionButton(
              ns("download_maps_link"), translate_app("download_maps_link", lang()),
              icon = shiny::icon("up-right-from-square"),
              onclick = "window.open('https://data-emf.creaf.cat/public/gpkg/daily_modelled_forests/', '_blank')"
            )
          )
        )
      ) # END of first row of inputs
    ) # end of tagList
  }) # end of map inputs ui

  # hostess ready
  hostess_raster <- waiter::Hostess$new(infinite = TRUE)
  hostess_raster$set_loader(waiter::hostess_loader(
    svg = "images/hostess_image.svg",
    progress_type = "fill",
    fill_direction = "ltr"
  ))
  hostess_polys <- waiter::Hostess$new(infinite = TRUE)
  hostess_polys$set_loader(waiter::hostess_loader(
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

  # update the map png table on input date change
  bitmap_data <- shiny::reactive({
    # only run when inputs are populated
    shiny::validate(
      # shiny::need(input$user_var, "Missing meteo variable"),
      shiny::need(input$user_date, "Missing date")
    )

    # show hostess
    waiter_map <- waiter::Waiter$new(
      id = ns('output_map'),
      html = shiny::tagList(
        hostess_raster$get_loader(),
        shiny::br(),
        shiny::p(glue::glue(
          "{translate_app('getting_data_for', lang())} {input$user_date}"
        )),
        shiny::p(translate_app("please_wait", lang()))
      ),
      color = '#f8f9fa71'
    )
    waiter_map$show()
    on.exit(waiter_map$hide(), add = TRUE)
    hostess_raster$start()
    on.exit(hostess_raster$close(), add = TRUE)

    # needed inputs
    # var_sel <- input$user_var
    date_sel <- input$user_date |>
      as.character() |>
      stringr::str_remove_all("-")

    # arrow data
    arrow::open_dataset(
      arrow_sink,
      factory_options = list(
        selector_ignore_prefixes = c("daily_medfateland_timeseries.parquet")
      )
    ) |>
      dplyr::filter(date == date_sel) |>
      dplyr::as_tibble()
  }) |>
    shiny::bindCache(
      # input$user_var,
      input$user_date,
      cache = "session"
    ) |>
    shiny::bindEvent(
      # input$user_var,
      input$user_date
    )

  # update the polygon data when input date changes
  provinces_data <- shiny::reactive({
    # only run when inputs are populated
    shiny::validate(
      # shiny::need(input$user_var, "Missing meteo variable"),
      shiny::need(input$user_date, "Missing date")
    )

    # show hostess
    waiter_map <- waiter::Waiter$new(
      id = ns('output_map'),
      html = shiny::tagList(
        hostess_polys$get_loader(),
        shiny::br(),
        shiny::p(glue::glue(
          "{translate_app('getting_data_for', lang())} {input$user_date}"
        )),
        shiny::p(translate_app("please_wait", lang()))
      ),
      color = '#f8f9fa71'
    )
    waiter_map$show()
    on.exit(waiter_map$hide(), add = TRUE)
    hostess_polys$start()
    on.exit(hostess_polys$close(), add = TRUE)

    # needed inputs
    # var_sel <- input$user_var
    date_sel <- input$user_date

    # arrow data
    arrow::open_dataset(
      arrow_sink,
      factory_options = list(
        selector_ignore_prefixes = c("daily_medfateland_bitmaps.parquet")
      )
    ) |>
      dplyr::filter(date == date_sel) |>
      dplyr::as_tibble() |>
      sf::st_as_sf(wkt = "geom", crs = 25830) |>
      sf::st_transform(crs = 4326)
  }) |>
    shiny::bindCache(
      # input$user_var,
      input$user_date,
      cache = "session"
    ) |>
    shiny::bindEvent(
      # input$user_var,
      input$user_date
    )

  # Updating the map
  shiny::observe({
    # get the data
    agg_sel <- input$user_agg
    var_sel <- input$user_var
    bitmap_sel <- bitmap_data() |>
      dplyr::filter(var == var_sel)
    # validate we have data, send an alert to the user if not
    shiny::validate(
      shiny::need(
        validate_rows_with_alert(bitmap_sel, lang),
        "no data for date and var selected"
      )
    )
    palette_fun <- function(var_sel) {
      scales::col_numeric(
        hcl.colors(10, "ag_GrnYl", alpha = 1),
        c(min(var_sel, na.rm = TRUE), max(var_sel, na.rm = TRUE)),
        na.color = "#FFFFFF00", reverse = FALSE, alpha = TRUE
      )(var_sel)
    }
    provinces_sel <- provinces_data() |>
      dplyr::select(dplyr::any_of(c(var_sel, "provincia", "geom"))) |>
      dplyr::mutate(
        hex = palette_fun(.data[[var_sel]]),
        tooltip = paste0(
          "<p>", .data[["provincia"]], ": ", round(.data[[var_sel]], 2), "</p>"
        ),
        fake_elevation = 100000 *
          (abs(.data[[var_sel]]) - min(abs(.data[[var_sel]]), na.rm = TRUE)) /
          (max(abs(.data[[var_sel]]), na.rm = TRUE) - min(abs(.data[[var_sel]]), na.rm = TRUE))
      )
    shiny::validate(
      shiny::need(
        validate_rows_with_alert(provinces_sel, lang),
        "no data for date and var selected"
      )
    )

    # branching depending on aggregation selected
    if (isFALSE(agg_sel)) {
      # create the custom legend to show with the bitmap
      legend_js <- mapdeck::legend_element(
        variables = rev(round(seq(
          bitmap_sel[["min_value"]],
          bitmap_sel[["max_value"]],
          length.out = 5
        ), 3)),
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
          "{translate_app(input$user_var, lang())} - {input$user_date}"
        )
      ) |>
        mapdeck::mapdeck_legend()
      # update the map
      mapdeck::mapdeck_update(map_id = ns("output_map")) |>
        mapdeck::clear_polygon(layer_id = "provinces_sel") |>
        mapdeck::add_bitmap(
          image = bitmap_sel$base64_string, layer_id = "bitmap_sel",
          bounds = c(
            bitmap_sel$left_ext, bitmap_sel$down_ext - 0.085,
            bitmap_sel$right_ext, bitmap_sel$up_ext - 0.085
          ),
          update_view = FALSE, focus_layer = FALSE,
          transparent_colour = "#00000000"
        ) |>
        mapdeck::add_legend(legend = legend_js, layer_id = "custom_legend")
    } else {
      # create the custom legend to show with the bitmap
      legend_js <- mapdeck::legend_element(
        variables = rev(round(seq(
          min(provinces_sel[[var_sel]], na.rm = TRUE),
          max(provinces_sel[[var_sel]], na.rm = TRUE),
          length.out = 5
        ), 3)),
        colours = scales::col_numeric(
          hcl.colors(10, "ag_GrnYl", alpha = 0.8),
          c(min(provinces_sel[[var_sel]], na.rm = TRUE), max(provinces_sel[[var_sel]], na.rm = TRUE)),
          na.color = "#FFFFFF00", reverse = TRUE, alpha = TRUE
        )(seq(
          min(provinces_sel[[var_sel]], na.rm = TRUE),
          max(provinces_sel[[var_sel]], na.rm = TRUE),
          length.out = 5
        )),
        colour_type = "fill", variable_type = "gradient",
        title = glue::glue(
          "{translate_app(input$user_var, lang())} - {input$user_date}"
        )
      ) |>
        mapdeck::mapdeck_legend()
      # update the map
      mapdeck::mapdeck_update(map_id = ns("output_map")) |>
        mapdeck::clear_bitmap(layer_id = "bitmap_sel") |>
        mapdeck::add_polygon(
          data = provinces_sel, layer_id = "provinces_sel",
          tooltip = "tooltip",
          id = "provincia",
          fill_colour = "hex",
          fill_opacity = 1,
          auto_highlight = TRUE, highlight_colour = "#FDF5EB80",
          elevation = "fake_elevation", elevation_scale = 1,
          update_view = FALSE, focus_layer = FALSE,
          legend = FALSE
        ) |>
        mapdeck::add_legend(legend = legend_js, layer_id = "custom_legend")
    }
  })
}