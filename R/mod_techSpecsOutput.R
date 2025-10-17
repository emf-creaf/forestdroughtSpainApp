#' @title mod_techSpecsOutput and mod_techSpecs
#'
#' @description Shiny module to show the technical specifications text
#'
#' @param id shiny id
#'
#' @export
mod_techSpecsOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns('techSpecs'))
  )
}

#' mod_techSpecs server function
#'
#' @details mod_techSpecs generates the crossvalidations table
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_techSpecsOutput
mod_techSpecs <- function(
  input, output, session,
  lang
) {

  output$techSpecs <- shiny::renderUI({

    markdown_translated <- glue::glue(
      "forestdrought_technical_specifications_{lang()}.md"
    )

    shiny::tagList(
      shiny::fluidPage(
        shiny::withMathJax(
          shiny::includeMarkdown(
            system.file('resources', markdown_translated, package = 'forestdroughtSpainApp')
          )
        )
      )
    )


  })

}
