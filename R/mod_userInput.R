#' @title mod_userInput and mod_user
#'
#' @description A shiny module to create and populate the user inputs
#' (variable, dates...)
#'
#' @param id shiny id
#'
#' @export
mod_userInput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(shiny::uiOutput(ns('mod_user_container')))
}

#' mod_user server function
#' 
#' @param input internal
#' @param output internal
#' @param session internal
#' @param lang lang reactive
#'
#' @export
mod_user <- function(
  input, output, session,
  lang
) {
  # renderUI for creating the inputs server-side (mainly due to translations)
  output$mod_user_container <- shiny::renderUI({
    # get the ns
    ns <- session$ns

    # precalculated choices
    user_var_choices <- c(
      "Precipitation", "PET", "Interception", "Infiltration", "Runoff",
      "DeepDrainage", "Esoil", "Eplant", "LAI",
      "Theta", "REW", "Psi", "soilW", "soilTemp",
      "DDS", "LFMC"
    ) |>
      purrr::set_names(translate_app(c(
        "Precipitation", "PET", "Interception", "Infiltration", "Runoff",
        "DeepDrainage", "Esoil", "Eplant", "LAI",
        "Theta", "REW", "Psi", "soilW", "soilTemp",
        "DDS", "LFMC"
      ), lang()))

    user_date_choices <- seq(Sys.Date() - 370, Sys.Date() - 5, by = "day") |>
      as.Date(format = '%j', origin = as.Date('1970-01-01')) |>
      as.character()

    # inputs tagList
    shiny::tagList(
      # first row of inputs, variable and dates
      shiny::h4(translate_app("user_var_date_title", lang())),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          # user_date
          shinyWidgets::airDatepickerInput(
            ns("user_date"), label = translate_app("user_date", lang()),
            value = user_date_choices[length(user_date_choices)],
            multiple = FALSE, range = FALSE,
            minDate = user_date_choices[1],
            maxDate = user_date_choices[length(user_date_choices)],
            firstDay = 1
          ),
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
          )
        )
      ), # END of first row of inputs
      # second row of inputs, time series
      shiny::h4(translate_app("user_ts_title", lang())),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          # user_longitude
          shinyWidgets::numericInputIcon(
            ns("user_longitude"), translate_app("user_longitude", lang()),
            value = -3.034,
            min = -9.500, max = 4, step = 0.001,
            icon = shiny::icon("x"),
            help_text = translate_app("user_longitude_help", lang())
          ),
          # user_latitude
          shinyWidgets::numericInputIcon(
            ns("user_latitude"), translate_app("user_latitude", lang()),
            value = 43.216,
            min = 35.500, max = 44, step = 0.001,
            icon = shiny::icon("y"),
            help_text = translate_app("user_latitude_help", lang())
          ),
          # user_ts_calculate
          bslib::input_task_button(
            ns("user_ts_update"), translate_app("user_ts_calculate", lang()),
            icon = shiny::icon("rotate"),
            label_busy = translate_app("user_ts_refresh_calculating", lang())
          )
        )
      ), # END of second row of inputs
      # a little separation
      shiny::br(),
      shiny::br(),
      # info panel row
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h4(shiny::icon("circle-info")),
          shiny::p(translate_app("user_info_p1", lang())),
          shiny::p(translate_app("user_info_p2", lang()))
        )
      ) # END of info panel row
    ) # END of inputs tagList
  }) # END of renderUI

  # Collect reactives to pass to the main app or other modules
  user_reactives <- shiny::reactiveValues()
  shiny::observe({
    user_reactives$user_var <- input$user_var
    user_reactives$user_date <- input$user_date
    user_reactives$user_longitude <- input$user_longitude
    user_reactives$user_latitude <- input$user_latitude
    user_reactives$user_ts_update <- input$user_ts_update
    user_reactives$user_inputs_session <- session
  })

  # return also the session for binding the bslib button to the Extended Task
  # later
  return(list(
    user_reactives = user_reactives,
    user_inputs_session = session
  ))
}