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
        width = "250px",
        !!!lapply(
          c(
            "Theta",
            "Psi",
            "REW",
            "soilW",
            "soilTemp",
            "Esoil",
            "Eplant",
            "PET",
            "LAI",
            "Precipitation",
            "Interception",
            "Infiltration",
            "Runoff",
            "DeepDrainage",
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
            value = FALSE
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
          # db preparation
          duckdb_proxy <- DBI::dbConnect(duckdb::duckdb())
          withr::defer(DBI::dbDisconnect(duckdb_proxy))
          install_httpfs_statement <- glue::glue_sql(
            .con = duckdb_proxy,
            "INSTALL httpfs;"
          )
          httpfs_statement <- glue::glue_sql(
            .con = duckdb_proxy,
            "LOAD httpfs;"
          )
          install_spatial_statement <- glue::glue_sql(
            .con = duckdb_proxy,
            "INSTALL spatial;"
          )
          spatial_statement <- glue::glue_sql(
            .con = duckdb_proxy,
            "LOAD spatial;"
          )
          credentials_statement <- glue::glue(
            "CREATE OR REPLACE SECRET secret (
              TYPE s3,
              PROVIDER config,
              KEY_ID '{Sys.getenv('AWS_ACCESS_KEY_ID')}',
              SECRET '{Sys.getenv('AWS_SECRET_ACCESS_KEY')}',
              REGION '',
              ENDPOINT '{Sys.getenv('AWS_S3_ENDPOINT')}'
            );"
          )
          DBI::dbExecute(duckdb_proxy, install_httpfs_statement)
          DBI::dbExecute(duckdb_proxy, httpfs_statement)
          DBI::dbExecute(duckdb_proxy, install_spatial_statement)
          DBI::dbExecute(duckdb_proxy, spatial_statement)
          DBI::dbExecute(duckdb_proxy, credentials_statement)

          # month query
          ts_query <- glue::glue("
            SELECT
              date,
              avg(Precipitation) FILTER (NOT isnan(Precipitation)) AS Precipitation,
              avg(PET) FILTER (NOT isnan(PET)) AS PET,
              avg(Interception) FILTER (NOT isnan(Interception)) AS Interception,
              avg(Infiltration) FILTER (NOT isnan(Infiltration)) AS Infiltration,
              avg(Runoff) FILTER (NOT isnan(Runoff)) AS Runoff,
              avg(DeepDrainage) FILTER (NOT isnan(DeepDrainage)) AS DeepDrainage,
              avg(Esoil) FILTER (NOT isnan(Esoil)) AS Esoil,
              avg(Eplant) FILTER (NOT isnan(Eplant)) AS Eplant,
              avg(LAI) FILTER (NOT isnan(LAI)) AS LAI,
              avg(Theta) FILTER (NOT isnan(Theta)) AS Theta,
              avg(REW) FILTER (NOT isnan(REW)) AS REW,
              avg(Psi) FILTER (NOT isnan(Psi)) AS Psi,
              avg(soilW) FILTER (NOT isnan(soilW)) AS soilW,
              avg(soilTemp) FILTER (NOT isnan(soilTemp)) AS soilTemp,
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

  # echarts4r outputs
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
        lineStyle = list(color = "#2575CF"),
        itemStyle = list(color = "#2575CF"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#2575CF"),
              list(offset = 1, color = "#2575CF00")
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
        lineStyle = list(color = "#94C96E"),
        itemStyle = list(color = "#94C96E"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#94C96E"),
              list(offset = 1, color = "#94C96E00")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_Interception <- echarts4r::renderEcharts4r({
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
        Interception, symbol = "none",
        name = translate_app("Interception", lang()),
        lineStyle = list(color = "#2575CF"),
        itemStyle = list(color = "#2575CF"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#2575CF"),
              list(offset = 1, color = "#2575CF00")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_Infiltration <- echarts4r::renderEcharts4r({
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
        Infiltration, symbol = "none",
        name = translate_app("Infiltration", lang()),
        lineStyle = list(color = "#2575CF"),
        itemStyle = list(color = "#2575CF"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#2575CF"),
              list(offset = 1, color = "#2575CF00")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_Runoff <- echarts4r::renderEcharts4r({
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
        Runoff, symbol = "none",
        name = translate_app("Runoff", lang()),
        lineStyle = list(color = "#2575CF"),
        itemStyle = list(color = "#2575CF"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#2575CF"),
              list(offset = 1, color = "#2575CF00")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_DeepDrainage <- echarts4r::renderEcharts4r({
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
        DeepDrainage, symbol = "none",
        name = translate_app("DeepDrainage", lang()),
        lineStyle = list(color = "#2575CF"),
        itemStyle = list(color = "#2575CF"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#2575CF"),
              list(offset = 1, color = "#2575CF00")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_Esoil <- echarts4r::renderEcharts4r({
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
        Esoil, symbol = "none",
        name = translate_app("Esoil", lang()),
        lineStyle = list(color = "#C75C00"),
        itemStyle = list(color = "#C75C00"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#C75C00"),
              list(offset = 1, color = "#C75C0000")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_Eplant <- echarts4r::renderEcharts4r({
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
        Eplant, symbol = "none",
        name = translate_app("Eplant", lang()),
        lineStyle = list(color = "#94C96E"),
        itemStyle = list(color = "#94C96E"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#94C96E"),
              list(offset = 1, color = "#94C96E00")
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
        lineStyle = list(color = "#94C96E"),
        itemStyle = list(color = "#94C96E"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#94C96E"),
              list(offset = 1, color = "#94C96E00")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
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
        lineStyle = list(color = "#00abfa"),
        itemStyle = list(color = "#00abfa"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#00abfa"),
              list(offset = 1, color = "#00abfa00")
            )
          ),
          opacity = 0.7
        )
      ) |>
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
        lineStyle = list(color = "#00abfa"),
        itemStyle = list(color = "#00abfa"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#00abfa"),
              list(offset = 1, color = "#00abfa00")
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
        lineStyle = list(color = "#00abfa"),
        itemStyle = list(color = "#00abfa"),
        areaStyle = list(
          color = list(
            type = "linear", x = 1, y = 1, x2 = 1, y2 = 0,
            colorStops = list(
              list(offset = 0, color = "#00abfa"),
              list(offset = 1, color = "#00abfa00")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_soilW <- echarts4r::renderEcharts4r({
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
        soilW, symbol = "none",
        name = translate_app("soilW", lang()),
        lineStyle = list(color = "#00abfa"),
        itemStyle = list(color = "#00abfa"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#00abfa"),
              list(offset = 1, color = "#00abfa00")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter()
  })
  output$ts_soilTemp <- echarts4r::renderEcharts4r({
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
        soilTemp, symbol = "none",
        name = translate_app("soilTemp", lang()),
        lineStyle = list(color = "#C75C00"),
        itemStyle = list(color = "#C75C00"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#C75C00"),
              list(offset = 1, color = "#C75C0000")
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
        lineStyle = list(color = "#F5191C"),
        itemStyle = list(color = "#F5191C"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#F5191C"),
              list(offset = 1, color = "#F5191C00")
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
        lineStyle = list(color = "#F5191C"),
        itemStyle = list(color = "#F5191C"),
        areaStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
            colorStops = list(
              list(offset = 0, color = "#F5191C"),
              list(offset = 1, color = "#F5191C00")
            )
          ),
          opacity = 0.7
        )
      ) |>
      echarts_ts_formatter(bottom = TRUE)
  })
}