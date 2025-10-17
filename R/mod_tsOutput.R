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
    shiny::div(
      id = ns("ts_hostess"),
      echarts4r::echarts4rOutput(ns("output_ts_soil"), height = 195),
      echarts4r::echarts4rOutput(ns("output_ts_climate"), height = 195),
      echarts4r::echarts4rOutput(ns("output_ts_wb"), height = 195),
      echarts4r::echarts4rOutput(ns("output_ts_stress"), height = 205)
    )
  )
}

#' mod_ts server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param user_inputs reactiveValues containing the user selected inputs
#' @param button_session session object from user inputs module
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_tsOutput
mod_ts <- function(
  input, output, session,
  user_inputs, button_session,
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

  # extended task for ts_data, to avoid blocking the app while calculating the
  # time series
  ts_data <- shiny::ExtendedTask$new(
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
            dplyr::arrange(dates) |>
            dplyr::mutate(
              point_latitude = user_latitude,
              point_longitude = user_longitude
            )
        },
        ...
      )
    }
  ) |>
    bslib::bind_task_button("user_ts_update", session = button_session)
  # 2. create an observer, bind it to the action button and invoke the
  # extended task
  shiny::observe({
    # validate inputs
    shiny::validate(
      shiny::need(user_inputs$user_latitude, "Missing latitude"),
      shiny::need(user_inputs$user_longitude, "Missing longitude")
    )
    # user points bbox (500^2)
    coords_bbox <- dplyr::tibble(
      x = user_inputs$user_longitude, y = user_inputs$user_latitude
    ) |>
      sf::st_as_sf(coords = c("x", "y"), crs = 4326) |>
      sf::st_transform(crs = 25830) |>
      sf::st_buffer(250) |>
      sf::st_bbox()
    # invoke extended task
    ts_data$invoke(
      user_longitude = user_inputs$user_longitude,
      user_latitude = user_inputs$user_latitude,
      coords_bbox = coords_bbox
    )
  }) |>
    shiny::bindEvent(user_inputs$user_ts_update)

  # 3. observer for hostess (copied from bslib bind_task_button code)
  was_running <- FALSE
  shiny::observe({
    waiter_ts <- waiter::Waiter$new(
      id = ns("ts_hostess"),
      html = shiny::tagList(
        hostess_ts$get_loader(),
        shiny::br(),
        shiny::p(glue::glue(
          "{translate_app('getting_data_for', lang())} {shiny::isolate(user_inputs$user_longitude)} - {shiny::isolate(user_inputs$user_latitude)}"
        )),
        shiny::p(translate_app("please_wait", lang()))
      ),
      color = '#E8EAEB'
    )
    running <- ts_data$status() == "running"
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

  # 4. use $result() to get the extended task result when calculated
  # echart outputs (temp, rh and rad-prec-pet (rpp))
  # title only in the first, zoom only in last
  output$output_ts_soil <- echarts4r::renderEcharts4r({
    ts_data$result() |>
      purrr::list_rbind() |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        Theta, symbol = "none",
        name = translate_app("Theta", lang())
      ) |>
      echarts4r::e_line(
        Psi, symbol = "none",
        name = translate_app("Psi", lang())
      ) |>
      echarts4r::e_line(
        REW, symbol = "none",
        name = translate_app("REW", lang())
      ) |>
      echarts4r::e_line(
        soilW, symbol = "none",
        name = translate_app("soilW", lang())
      ) |>
      echarts4r::e_line(
        soilTemp, symbol = "none",
        name = translate_app("soilTemp", lang())
      ) |>
      echarts_ts_formatter()
  })
  output$output_ts_climate <- echarts4r::renderEcharts4r({
    ts_data$result() |>
      purrr::list_rbind() |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        PET, symbol = "none",
        name = translate_app("PET", lang())
      ) |>
      echarts4r::e_line(
        Precipitation, symbol = "none",
        name = translate_app("Precipitation", lang())
      ) |>
      echarts_ts_formatter()
  })
  output$output_ts_wb <- echarts4r::renderEcharts4r({
    ts_data$result() |>
      purrr::list_rbind() |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        Interception, symbol = "none",
        name = translate_app("Interception", lang())
      ) |>
      echarts4r::e_line(
        Runoff, symbol = "none",
        name = translate_app("Runoff", lang())
      ) |>
      echarts4r::e_line(
        DeepDrainage, symbol = "none",
        name = translate_app("DeepDrainage", lang())
      ) |>
      echarts4r::e_line(
        Esoil, symbol = "none",
        name = translate_app("Esoil", lang())
      ) |>
      echarts4r::e_line(
        Eplant, symbol = "none",
        name = translate_app("Eplant", lang())
      ) |>
      echarts4r::e_line(
        LAI, symbol = "none",
        name = translate_app("LAI", lang())
      ) |>
      echarts_ts_formatter(bottom = TRUE)
  })
  output$output_ts_stress <- echarts4r::renderEcharts4r({
    ts_data$result() |>
      purrr::list_rbind() |>
      dplyr::arrange(date) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(
        DDS, symbol = "none",
        name = translate_app("DDS", lang())
      ) |>
      echarts4r::e_line(
        LFMC, symbol = "none",
        name = translate_app("LFMC", lang())
      ) |>
      echarts_ts_formatter()
  })

  # 5. Use $result() also to get the point info and show it in the
  # ui
  # output$output_ts_point <- shiny::renderUI({
  #   shiny::validate(
  #     shiny::need(ts_data$result(), "no ts data yet")
  #   )

  #   point_data <- ts_data$result() |>
  #     purrr::list_rbind()

  #   point_elevation <- point_data |>
  #     dplyr::pull(elevation) |>
  #     dplyr::first() |>
  #     round(2)
  #   point_aspect <- point_data |>
  #     dplyr::pull(aspect) |>
  #     dplyr::first() |>
  #     round(2)
  #   point_slope <- point_data |>
  #     dplyr::pull(slope) |>
  #     dplyr::first() |>
  #     round(2)
  #   point_latitude <- point_data |>
  #     dplyr::pull(point_latitude) |>
  #     dplyr::first()
  #   point_longitude <- point_data |>
  #     dplyr::pull(point_longitude) |>
  #     dplyr::first()

  #   shiny::tagList(
  #     shiny::wellPanel(
  #       shiny::div(
  #         align = "center",
  #         shiny::icon("map-location"),
  #         glue::glue("{point_longitude}, {point_latitude}  |  "),
  #         shiny::icon("mountain"),
  #         glue::glue("{point_elevation}m asl.  |  "),
  #         shiny::icon("hill-rockslide"),
  #         glue::glue("{point_slope}º  |  "),
  #         shiny::icon("compass"),
  #         glue::glue("{point_aspect}º")
  #       )
  #     )
  #   )
  # })

  # Collect reactives to pass to the main app or other modules
  ts_reactives <- shiny::reactiveValues()
  shiny::observe({
    ts_reactives$ts_data <- ts_data
  })
  return(ts_reactives)
}