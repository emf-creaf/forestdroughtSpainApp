#' @title mod_tsOutput and mod_ts
#'
#' @description Shiny module to show the variables timeseries
#'
#' @param id shiny id
#'
#' @export
mod_tsOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      bslib::layout_column_wrap(
        width = "350px",
        !!!lapply(
          c(
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
          ),
          \(id) {
            bslib::card(
              echarts4r::echarts4rOutput(ns(paste0("ts_", id)), height = "250px")
            )
          }
        )
      ),
      sidebar = bslib::sidebar(
        shiny::uiOutput(ns('inputs_ts')),
        class = "inputs_ts",
        open = list(desktop = "open", mobile = "always-above")
      )
    )
  )
}

#' mod_ts server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param arrow_sink bucket s3 filesystem
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_tsOutput
mod_ts <- function(
  input, output, session,
  arrow_sink,
  lang
) {
  # get the ns
  ns <- session$ns

  # hostess ready
  hostess_ts <- waiter::Hostess$new(infinite = TRUE)
  hostess_ts$set_loader(waiter::hostess_loader(
    svg = "images/hostess_image.svg",
    progress_type = "fill",
    fill_direction = "ltr"
  ))

  # ts inputs
  output$inputs_ts <- shiny::renderUI({
    # tagList creating the draggable absolute panel
    shiny::tagList(
      # first row of inputs, variable and dates
      shiny::h4(translate_app("ts_controls", lang())),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          # aggregation input
          shinyWidgets::materialSwitch(
            ns("user_ts_type"), label = translate_app("user_ts_type", lang()),
            value = FALSE, status = "info"
          ),
          shiny::conditionalPanel(
            condition = "input.user_ts_type == false", ns = ns,
            shiny::br(),
            shinyWidgets::pickerInput(
              ns("user_province"), label = translate_app("user_province", lang()),
              choices = province_names,
              selected = province_names[1],
              multiple = FALSE,
              options = shinyWidgets::pickerOptions(
                actionsBox = FALSE,
                tickIcon = "glyphicon-ok-sign"
              )
            )
          ),
          shiny::conditionalPanel(
            condition = "input.user_ts_type == true", ns = ns,
            shiny::br(),
            # user_longitude
            shiny::numericInput(
              ns("user_longitude"), translate_app("user_longitude", lang()),
              value = -3.034,
              min = -9.500, max = 4, step = 0.100,
              updateOn = "blur"
            ),
            # user_latitude
            shiny::numericInput(
              ns("user_latitude"), translate_app("user_latitude", lang()),
              value = 43.216,
              min = 35.500, max = 44, step = 0.100,
              updateOn = "blur"
            ),
            # user_ts_calculate
            bslib::input_task_button(
              ns("user_ts_update"), translate_app("user_ts_calculate", lang()),
              icon = shiny::icon("rotate"),
              label_busy = translate_app("user_ts_refresh_calculating", lang())
            )
          ),
          shiny::br(), shiny::br(),
          # download
          shiny::wellPanel(
            shiny::h4(translate_app("download_ts_title", lang())),
            shiny::p(translate_app("download_ts_text", lang())),
            shiny::downloadButton(
              ns("download_ts_button"), translate_app("download_ts_button", lang()),
              icon = shiny::icon("up-right-from-square")
            )
          )
        )
      ) # END of first row of inputs
    ) # end of tagList
  }) # end of ts inputs ui

  # province ts data
  province_data <- shiny::reactive({
    # only run when inputs are populated
    shiny::validate(
      shiny::need(input$user_province, "Missing province")
    )

    # show hostess
    waiter_ts <- waiter::Waiter$new(
      id = NULL,
      html = shiny::tagList(
        hostess_ts$get_loader(),
        shiny::br(),
        shiny::p(glue::glue(
          "{translate_app('getting_data_for', lang())} {input$user_province}"
        )),
        shiny::p(translate_app("please_wait", lang()))
      ),
      color = '#f8f9fa71'
    )
    waiter_ts$show()
    on.exit(waiter_ts$hide(), add = TRUE)
    hostess_ts$start()
    on.exit(hostess_ts$close(), add = TRUE)

    province_sel <- input$user_province
    # arrow data
    arrow::open_dataset(
      arrow_sink,
      factory_options = list(
        selector_ignore_prefixes = c("daily_medfateland_bitmaps.parquet")
      )
    ) |>
      dplyr::filter(provincia == province_sel) |>
      dplyr::as_tibble()
  }) |>
    shiny::bindCache(
      # input$user_var,
      input$user_province,
      cache = "session"
    ) |>
    shiny::bindEvent(
      input$user_province
    )

  # extended task for ts_coords_data, to avoid blocking the app while calculating the
  # time series
  ts_coords_data <- shiny::ExtendedTask$new(
    \(...) {
      mirai::mirai_map(
        1L:12L,
        \(month_to_query) {
          # month query
          ts_query <- glue::glue("
            SELECT
              date,
              avg(Theta) FILTER (NOT isnan(Theta)) AS Theta,
              avg(REW) FILTER (NOT isnan(REW)) AS REW,
              avg(Psi) FILTER (NOT isnan(Psi)) AS Psi,
              avg(PET) FILTER (NOT isnan(PET)) AS PET,
              avg(Precipitation) FILTER (NOT isnan(Precipitation)) AS Precipitation,
              avg(AET) FILTER (NOT isnan(AET)) AS AET,
              avg(ELW) FILTER (NOT isnan(ELW)) AS ELW,
              avg(LAI) FILTER (NOT isnan(LAI)) AS LAI,
              avg(DDS) FILTER (NOT isnan(DDS)) AS DDS,
              avg(LFMC) FILTER (NOT isnan(LFMC)) AS LFMC
            FROM read_parquet('s3://forestdrought-spain-app-data/*/*/*/*.parquet')
            WHERE geometry.x > {coords_bbox[['xmin']]} AND
              geometry.x < {coords_bbox[['xmax']]} AND
              geometry.y > {coords_bbox[['ymin']]} AND
              geometry.y < {coords_bbox[['ymax']]} AND
              month = {month_to_query}
            GROUP BY date
            ;
          ")
          DBI::dbGetQuery(duckdb_proxy, ts_query) |>
            dplyr::arrange(date) |>
            dplyr::mutate(
              point_latitude = user_latitude,
              point_longitude = user_longitude
            )
        },
        ...
      )
    }
  ) |>
    bslib::bind_task_button("user_ts_update")
  # 2. create an observer, bind it to the action button and invoke the
  # extended task
  shiny::observe({
    # validate inputs
    shiny::validate(
      shiny::need(input$user_latitude, "Missing latitude"),
      shiny::need(input$user_longitude, "Missing longitude")
    )
    # user points bbox (500^2)
    coords_bbox <- dplyr::tibble(
      x = input$user_longitude, y = input$user_latitude
    ) |>
      sf::st_as_sf(coords = c("x", "y"), crs = 4326) |>
      sf::st_transform(crs = 25830) |>
      sf::st_buffer(250) |>
      sf::st_bbox()
    # invoke extended task
    ts_coords_data$invoke(
      user_longitude = input$user_longitude,
      user_latitude = input$user_latitude,
      coords_bbox = coords_bbox
    )
  }) |>
    shiny::bindEvent(input$user_ts_update)

  # observer for hostess (copied from bslib bind_task_button code)
  was_running <- FALSE
  shiny::observe({
    waiter_ts <- waiter::Waiter$new(
      id = NULL,
      html = shiny::tagList(
        hostess_ts$get_loader(),
        shiny::br(),
        shiny::p(glue::glue(
          "{translate_app('getting_data_for', lang())} {shiny::isolate(input$user_longitude)} - {shiny::isolate(input$user_latitude)}"
        )),
        shiny::p(translate_app("please_wait", lang()))
      ),
      color = '#f8f9fa71'
    )
    running <- ts_coords_data$status() == "running"
    if (running != was_running) {
      was_running <<- running
      if (running) {
        # show hostess
        waiter_ts$show()
        hostess_ts$start()
      } else {
        waiter_ts$hide()
        hostess_ts$close()
      }
    }
  }, priority = 1000)

  #### echarts4r outputs ####
  output$ts_Theta <- echarts4r::renderEcharts4r({
    if (isFALSE(input$user_ts_type)) {
      ts_data <- province_data()
    } else {
      shiny::validate(
        shiny::need(ts_coords_data$result(), "No time series data yet, press the button")
      )
      ts_data <- ts_coords_data$result() |>
        purrr::list_rbind()
    }

    ts_data |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        Theta, symbol = "none",
        name = translate_app("Theta", lang()),
        lineStyle = list(color = "#4ef0ff"),
        itemStyle = list(color = "#4ef0ff"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#4ef0ff"),
              list(offset = 0.25, color = "#00b6de"),
              list(offset = 0.5, color = "#007db4"),
              list(offset = 0.75, color = "#00458b"),
              list(offset = 1, color = "#0e005b")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_Psi <- echarts4r::renderEcharts4r({
    if (isFALSE(input$user_ts_type)) {
      ts_data <- province_data()
    } else {
      shiny::validate(
        shiny::need(ts_coords_data$result(), "No time series data yet, press the button")
      )
      ts_data <- ts_coords_data$result() |>
        purrr::list_rbind()
    }

    ts_data |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        Psi, symbol = "none",
        name = translate_app("Psi", lang()),
        lineStyle = list(color = "#4ef0ff"),
        itemStyle = list(color = "#4ef0ff"),
        areaStyle = list(
          color = list(
            type = "linear", x = 1, y = 1, x2 = 1, y2 = 0,
            colorStops = list(
              list(offset = 0, color = "#0e005b"),
              list(offset = 0.25, color = "#00458b"),
              list(offset = 0.5, color = "#007db4"),
              list(offset = 0.75, color = "#00b6de"),
              list(offset = 1, color = "#4ef0ff")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts4r::e_y_axis(type = "log") |>
      echarts_ts_formatter()
  })
  output$ts_REW <- echarts4r::renderEcharts4r({
    if (isFALSE(input$user_ts_type)) {
      ts_data <- province_data()
    } else {
      shiny::validate(
        shiny::need(ts_coords_data$result(), "No time series data yet, press the button")
      )
      ts_data <- ts_coords_data$result() |>
        purrr::list_rbind()
    }

    ts_data |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        REW, symbol = "none",
        name = translate_app("REW", lang()),
        lineStyle = list(color = "#4ef0ff"),
        itemStyle = list(color = "#4ef0ff"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#4ef0ff"),
              list(offset = 0.25, color = "#00b6de"),
              list(offset = 0.5, color = "#007db4"),
              list(offset = 0.75, color = "#00458b"),
              list(offset = 1, color = "#0e005b")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_ELW <- echarts4r::renderEcharts4r({
    if (isFALSE(input$user_ts_type)) {
      ts_data <- province_data()
    } else {
      shiny::validate(
        shiny::need(ts_coords_data$result(), "No time series data yet, press the button")
      )
      ts_data <- ts_coords_data$result() |>
        purrr::list_rbind()
    }

    ts_data |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        ELW, symbol = "none",
        name = translate_app("ELW", lang()),
        lineStyle = list(color = "#4ef0ff"),
        itemStyle = list(color = "#4ef0ff"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#4ef0ff"),
              list(offset = 0.25, color = "#00b6de"),
              list(offset = 0.5, color = "#007db4"),
              list(offset = 0.75, color = "#00458b"),
              list(offset = 1, color = "#0e005b")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_Precipitation <- echarts4r::renderEcharts4r({
    if (isFALSE(input$user_ts_type)) {
      ts_data <- province_data()
    } else {
      shiny::validate(
        shiny::need(ts_coords_data$result(), "No time series data yet, press the button")
      )
      ts_data <- ts_coords_data$result() |>
        purrr::list_rbind()
    }

    ts_data |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        Precipitation, symbol = "none",
        name = translate_app("Precipitation", lang()),
        lineStyle = list(color = "#4ef0ff"),
        itemStyle = list(color = "#4ef0ff"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#4ef0ff"),
              list(offset = 0.25, color = "#00b6de"),
              list(offset = 0.5, color = "#007db4"),
              list(offset = 0.75, color = "#00458b"),
              list(offset = 1, color = "#0e005b")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_PET <- echarts4r::renderEcharts4r({
    if (isFALSE(input$user_ts_type)) {
      ts_data <- province_data()
    } else {
      shiny::validate(
        shiny::need(ts_coords_data$result(), "No time series data yet, press the button")
      )
      ts_data <- ts_coords_data$result() |>
        purrr::list_rbind()
    }

    ts_data |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        PET, symbol = "none",
        name = translate_app("PET", lang()),
        lineStyle = list(color = "#ffd0fe"),
        itemStyle = list(color = "#ffd0fe"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#ffd0fe"),
              list(offset = 0.25, color = "#f4a7ff"),
              list(offset = 0.5, color = "#e180ff"),
              list(offset = 0.75, color = "#ca55ff"),
              list(offset = 1, color = "#b007ff")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_AET <- echarts4r::renderEcharts4r({
    if (isFALSE(input$user_ts_type)) {
      ts_data <- province_data()
    } else {
      shiny::validate(
        shiny::need(ts_coords_data$result(), "No time series data yet, press the button")
      )
      ts_data <- ts_coords_data$result() |>
        purrr::list_rbind()
    }

    ts_data |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        AET, symbol = "none",
        name = translate_app("AET", lang()),
        lineStyle = list(color = "#ffd0fe"),
        itemStyle = list(color = "#ffd0fe"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#ffd0fe"),
              list(offset = 0.25, color = "#f4a7ff"),
              list(offset = 0.5, color = "#e180ff"),
              list(offset = 0.75, color = "#ca55ff"),
              list(offset = 1, color = "#b007ff")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_LAI <- echarts4r::renderEcharts4r({
    if (isFALSE(input$user_ts_type)) {
      ts_data <- province_data()
    } else {
      shiny::validate(
        shiny::need(ts_coords_data$result(), "No time series data yet, press the button")
      )
      ts_data <- ts_coords_data$result() |>
        purrr::list_rbind()
    }

    ts_data |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        LAI, symbol = "none",
        name = translate_app("LAI", lang()),
        lineStyle = list(color = "#a2ffb6"),
        itemStyle = list(color = "#a2ffb6"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#a2ffb6"),
              list(offset = 0.25, color = "#00e25d"),
              list(offset = 0.5, color = "#00b33b"),
              list(offset = 0.75, color = "#00861b"),
              list(offset = 1, color = "#025b00")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_DDS <- echarts4r::renderEcharts4r({
    if (isFALSE(input$user_ts_type)) {
      ts_data <- province_data()
    } else {
      shiny::validate(
        shiny::need(ts_coords_data$result(), "No time series data yet, press the button")
      )
      ts_data <- ts_coords_data$result() |>
        purrr::list_rbind()
    }

    ts_data |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        DDS, symbol = "none",
        name = translate_app("DDS", lang()),
        lineStyle = list(color = "#ff9b9b"),
        itemStyle = list(color = "#ff9b9b"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#ff9b9b"),
              list(offset = 0.25, color = "#ff4954"),
              list(offset = 0.5, color = "#d40024"),
              list(offset = 0.75, color = "#96000d"),
              list(offset = 1, color = "#5b0000")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_LFMC <- echarts4r::renderEcharts4r({
    if (isFALSE(input$user_ts_type)) {
      ts_data <- province_data()
    } else {
      shiny::validate(
        shiny::need(ts_coords_data$result(), "No time series data yet, press the button")
      )
      ts_data <- ts_coords_data$result() |>
        purrr::list_rbind()
    }

    ts_data |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        LFMC, symbol = "none",
        name = translate_app("LFMC", lang()),
        lineStyle = list(color = "#ff9b9b"),
        itemStyle = list(color = "#ff9b9b"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#ff9b9b"),
              list(offset = 0.25, color = "#ff4954"),
              list(offset = 0.5, color = "#d40024"),
              list(offset = 0.75, color = "#96000d"),
              list(offset = 1, color = "#5b0000")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })

  # download button logic
  output$download_ts_button <- shiny::downloadHandler(
    filename = glue::glue(
      "meteoland_timeseries_{format(Sys.time(), '%Y%m%d%H%M%S')}.csv"
    ),
    content = function(file) {
      if (isFALSE(input$user_ts_type)) {
        shiny::validate(
          shiny::need(province_data(), "no provinces data yet")
        )
        province_data() |>
          write.csv(file)
      } else {
        shiny::validate(
          shiny::need(ts_coords_data$result(), "No time series data yet, press the button")
        )
        ts_coords_data$result() |>
          purrr::list_rbind() |>
          write.csv(file)
      }
    }
  )
}