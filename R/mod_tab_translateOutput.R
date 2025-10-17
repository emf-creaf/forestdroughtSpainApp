#' @title mod_tab_translateOutput and mod_tab_translation
#'
#' @description A shiny module to translate tab titles
#'
#' @param id shiny id
#'
#' @export
mod_tab_translateOutput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::uiOutput(ns("tab_title_translated"), inline = TRUE)
}

#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param tab_title character with the tab title to translate
#' @param lang lang reactive
#'
#' @rdname mod_tab_translateOutput
#'
#' @export
mod_tab_translate <- function(
  input, output, session,
  tab_title, lang
) {
  output$tab_title_translated <- shiny::renderUI({
    translated_title <- translate_app(tab_title, lang())
    shiny::tagList(translated_title)
  })
}
