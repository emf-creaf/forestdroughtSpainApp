#' @title mod_downloadOutput and mod_download
#'
#' @description Shiny module to generate the contents of the download tab
#'
#' @param id shiny id
#'
#' @export
mod_downloadOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  shiny::tagList(shiny::uiOutput(ns('mod_download_container')))
}

#' mod_download server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param user_inputs reactiveValues containing the user selected inputs
#' @param ts_outputs reactiveValues containing the ts calculated ouputs
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_downloadOutput
mod_download <- function(
  input, output, session,
  user_inputs, ts_outputs,
  lang
) {
  output$mod_download_container <- shiny::renderUI({
    # get the ns
    ns <- session$ns
    # output tagList
    shiny::tagList(
      shiny::fluidRow(
        # maps download (gpkg public repository link)
        shiny::column(
          width = 4, offset = 2,
          shiny::wellPanel(
            shiny::h4(translate_app("download_maps_title", lang())),
            shiny::p(translate_app("download_maps_text", lang())),
            shiny::actionButton(
              ns("download_maps_link"), translate_app("download_maps_link", lang()),
              icon = shiny::icon("up-right-from-square"),
              onclick = "window.open('https://data-emf.creaf.cat/public/gpkg/daily_modelled_forests/', '_blank')"
            )
          )
        ), # END of maps download column
        # timeseries download (csv)
        shiny::column(
          width = 4,
          shiny::wellPanel(
            shiny::h4(translate_app("download_ts_title", lang())),
            shiny::p(translate_app("download_ts_text", lang())),
            shinyjs::disabled(
              shiny::downloadButton(
                ns("download_ts_button"),
                translate_app("download_ts_button", lang()),
                icon = shiny::icon("download")
              )
            )
          )
        ) # END of timeseries download column
      ), # END of download row
      # a little space with the footer is needed here
      shiny::br(), shiny::br(), shiny::br(),
    ) # END of ouput tagList
  }) # END of renderUI

  # Observer to deactivate the ts download button if no ts is calculated yet
  shiny::observe({
    if (user_inputs$user_ts_update > 0) {
      shinyjs::enable("download_ts_button")
    }
  }) |>
    shiny::bindEvent(user_inputs$user_ts_update)

  # Dowload button logic
  output$download_ts_button <- shiny::downloadHandler(
    filename = glue::glue(
      "meteoland_timeseries_{format(Sys.time(), '%Y%m%d%H%M%S')}.csv"
    ),
    content = function(file) {
      shiny::validate(
        shiny::need(user_inputs$user_ts_update > 0, "no ts calculated yet")
      )
      ts_outputs$ts_data$result() |>
        purrr::list_rbind() |>
        write.csv(file)
    }
  )
}