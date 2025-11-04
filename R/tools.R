# Call this function with an input (such as `textInput("text", NULL, "Search")`)
# if you want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  navbar[[4]][[1]]$children[[1]][[1]]$children[[1]][[3]][[2]] <-
    htmltools::tagAppendChild(
      navbar[[4]][[1]]$children[[1]][[1]]$children[[1]][[3]][[2]], form
    )

  return(navbar)
}

#' translate app function
#'
#' translate the app based on the lang selected
translate_app <- function(id, lang, thesaurus = apps_translations) {
  # recursive call for vectors
  if (length(id) > 1) {
    res <- purrr::map_chr(
      id,
      .f = \(.id) {
        translate_app(.id, lang)
      }
    )
    return(res)
  }

  # get id translations
  id_row <- app_translations |>
    dplyr::filter(text_id == id)

  # return raw id if no matching id found
  if (nrow(id_row) < 1) {
    return(id)
  }

  # get the lang translation
  return(dplyr::pull(id_row, glue::glue("translation_{lang}")))
}

#' echarts ts formatter
#'
#' Apply the common format (legend, tooltip, theme...) to timeseries (echarts)
#'
#' @param echart echarts4r object to format
echarts_ts_formatter <- function(echart) {
  echart |>
    echarts4r::e_tooltip(
      trigger = "axis", confine = TRUE,
      backgroundColor = "#4444449a",
      borderColor ="#4444449a",
      textStyle = list(color = "#F8F9FA"),
      formatter = echarts4r::e_tooltip_pointer_formatter(digits = 3)
    ) |>
    echarts4r::e_datazoom(toolbox = FALSE, type = "inside") |>
    echarts4r::e_group("timeseries") |>
    echarts4r::e_connect_group("timeseries") |>
    # echarts4r::e_theme("emf_colors") |>
    echarts4r::e_axis(
      axis = "x",
      axisLine = list(lineStyle = list(color = "#969696", opacity = 0.9)),
      axisLabel = list(color = "#969696"),
      splitLine = list(lineStyle = list(color = "#969696", opacity = 0.9))
    ) |>
    echarts4r::e_axis(
      axis = "y",
      axisLine = list(lineStyle = list(color = "#969696", opacity = 0.9)),
      axisLabel = list(color = "#969696"),
      splitLine = list(lineStyle = list(color = "#969696", opacity = 0.9))
    ) |>
    echarts4r::e_legend(textStyle = list(color = "#969696", opacity = 0.9))
}

validate_rows_with_alert <- function(reactive_data, lang) {
  if (isFALSE(nrow(reactive_data) > 0)) {
    shiny::showModal(
      shiny::modalDialog(
        translate_app("alert_no_data_text", lang()),
        title = translate_app("alert_no_data_title", lang()),
        easyClose = TRUE,
        footer = shiny::modalButton(translate_app("alert_dismiss", lang()))
      )
    )
  }
  # return TRUE or FALSE for shiny::need
  return(nrow(reactive_data) > 0)
}