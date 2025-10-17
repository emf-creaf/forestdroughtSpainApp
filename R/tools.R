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
#' @param bottom Logical. The bottom ts needs to connect the group and also
#'   show the datazoom slider
echarts_ts_formatter <- function(echart, bottom = FALSE) {

  if (isTRUE(bottom)) {
    echart |>
      echarts4r::e_tooltip(trigger = "axis") |>
      echarts4r::e_datazoom(toolbox = FALSE, type = "slider") |>
      echarts4r::e_group("timeseries") |>
      echarts4r::e_connect_group("timeseries") |>
      echarts4r::e_theme("emf_colors") |>
      echarts4r::e_axis(axis = "x", axisLine = list(lineStyle = list(color = "#F8F9FA"))) |>
      echarts4r::e_axis(axis = "y", axisLine = list(lineStyle = list(color = "#F8F9FA"))) |>
      echarts4r::e_legend(textStyle = list(color = "#F8F9FA"))
  } else {
    echart |>
      echarts4r::e_tooltip(trigger = "axis") |>
      echarts4r::e_datazoom(toolbox = FALSE, type = "slider", show = FALSE) |>
      echarts4r::e_group("timeseries") |>
      echarts4r::e_theme("emf_colors") |>
      echarts4r::e_axis(axis = "x", axisLine = list(lineStyle = list(color = "#F8F9FA"))) |>
      echarts4r::e_axis(axis = "y", axisLine = list(lineStyle = list(color = "#F8F9FA"))) |>
      echarts4r::e_legend(textStyle = list(color = "#F8F9FA"))
  }
}

#' echarts cv builder
#'
#' Build the cross validations maps
#'
#' @param echart_data data to build the echart
#' @param stat2plot stat (bias, mae...) to plot
#' @param lang selected lang for translations
echarts_cv_builder <- function(echart_data, stat2plot, lang) {
  echart_data |>
    dplyr::filter(stat == stat2plot) |>
    echarts4r::e_charts(interpolator_id) |>
    echarts4r::e_map_register(
      "interpolator_bboxes", interpolators_geojson
    ) |>
    echarts4r::e_map(
      value, map = "interpolator_bboxes", nameProperty = "i_step"
    ) |>
    echarts4r::e_visual_map(
      value,
      inRange = list(color = c("#14ABCC", "#7CC69A", "#E3DF68")),
      min = 0, max = 100, precision = 3,
      textStyle = list(color = "#F8F9FA"),
      left = "right"
    ) |>
    echarts4r::e_title(
      translate_app(stat2plot, lang()),
      left = "center", textStyle = list(
        fontStyle = "oblique", fontWeight = "bold", fontSize = 14,
        color = "#F8F9FA"
      )
    ) |>
    echarts4r::e_theme("emf_colors")
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