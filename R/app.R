#' App launcher
#'
#' Function to launch the app (in console, in container...)
#'
#' This function builds the UI (calling any modules necessary), the server
#' (calling any modules necessary) and return a `ShinyApp()` object.
#'
#' @export
forestdrought_spain_app <- function() {
  #### Language input ####
  shiny::addResourcePath(
    "images", system.file("resources", "images", package = "forestdroughtSpainApp")
  )
  lang_choices <- c("cat", "spa", "eng")
  lang_flags <- c(
    glue::glue(
      "<img class='flag-image' src='images/cat.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    ),
    glue::glue(
      "<img class='flag-image' src='images/spa.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    ),
    glue::glue(
      "<img class='flag-image' src='images/eng.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    )
  )

  #### Mirai daemons ####
  mirai::daemons(6)
  mirai::everywhere({
    library(DBI)
    library(duckdb)
    # db preparation
    duckdb_proxy <<- DBI::dbConnect(duckdb::duckdb())
    # withr::defer(DBI::dbDisconnect(duckdb_proxy))
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
  })

  shiny::onStop(function() {
    mirai::everywhere({DBI::dbDisconnect(duckdb_proxy)})
    mirai::daemons(0)
  })

  #### JS scripts needed ####

  #### UI ####
  ui <- shiny::tagList(
    # css
    shiny::tags$head(
      # js script,

      # initializations
      waiter::use_waiter(),
      waiter::use_hostess(),
      shinyjs::useShinyjs(),
      # corporative image custom css
      shiny::includeCSS(
        system.file("resources", "css", "corp_image.css", package = "forestdroughtSpainApp")
      )
    ),

    navbarPageWithInputs(
      # opts
      title = "ForestDrought App",
      id = "nav",
      collapsible = TRUE,

      # Lang selector (input for navbaraPageWithInputs)
      inputs = shinyWidgets::pickerInput(
        "lang", NULL,
        choices = lang_choices,
        selected = "eng",
        width = "100px",
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),

      # footer
      footer = shiny::tags$footer(
        shiny::fluidRow(
          shiny::column(
            width = 12, align = "right",
            shiny::HTML(glue::glue(
              '<img src="images/emf_white_logo.svg" width="120px" class="d-inline-block" alt="" loading="lazy">
              <img src="images/creaf_white_logo.svg" width="135px" class="d-inline-block" alt="" loading="lazy">
              <span>({lubridate::year(Sys.Date())})</span>'
            ))
          )
        )
      ),

      # Main (Explore) tab
      shiny::tabPanel(
        title = mod_tab_translateOutput("main_tab_translation"),
        icon = shiny::icon("map"),
        ########################################################### debug ####
        # shiny::absolutePanel(                                              #
        #   id = 'debug', class = 'panel panel-default', fixed = TRUE,       #
        #   draggable = TRUE, width = 640, height = 'auto',                  #
        #   top = 'auto', left = 10, right = 'auto', bottom = 15,            #
        #   shiny::h3("DEBUG"),                                              #
        #   shiny::textOutput('debug1'),                                     #
        #   shiny::textOutput('debug2'),                                     #
        #   shiny::textOutput('debug3')                                      #
        # ),                                                                 #
        ####################################################### end debug ####
        mod_mapOutput("map_output")
      ), # END of main (Explore) tab
      # Time series tab
      shiny::tabPanel(
        title = mod_tab_translateOutput("ts_tab_translation"),
        icon = shiny::icon("chart-line"),
        mod_tsOutput("ts_output")
      ), # END of ts tab
      # Technical specs tab
      shiny::tabPanel(
        title = mod_tab_translateOutput("tech_specs_tab_translation"),
        icon = shiny::icon("cog"),
        mod_techSpecsOutput("tech_specs_output")
      ) # END of tech specs tab
    ) # END of navbarPage
  ) # END of UI tagList

  #### SERVER ####
  server <- function(input, output, session) {
    # lang reactive
    lang <- shiny::reactive({
      input$lang
    })

    # mapbox token
    mapdeck::set_token(Sys.getenv("MAPBOX_TOKEN"))

    # bucket
    forestdrought_bucket <- arrow::s3_bucket(
      "forestdrought-spain-app-pngs",
      access_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
      secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      scheme = "https",
      endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"),
      region = ""
    )

    # modules
    map_reactives <- shiny::callModule(
      mod_map, 'map_output',
      arrow_sink = forestdrought_bucket,
      lang = lang
    )
    ts_reactives <- shiny::callModule(
      mod_ts, 'ts_output',
      arrow_sink = forestdrought_bucket,
      lang = lang
    )
    shiny::callModule(
      mod_techSpecs, "tech_specs_output",
      lang
    )

    # tab translations
    c(
      "main_tab_translation", "ts_tab_translation",
      "tech_specs_tab_translation"
    ) |>
      purrr::walk(
        .f = \(mod_id) {
          shiny::callModule(mod_tab_translate, mod_id, mod_id, lang)
        }
      )

    ########################################################### debug ####
    # output$debug1 <- shiny::renderPrint({
    #   user_reactives$user_reactives$user_ts_update
    # })
    # output$debug2 <- shiny::renderPrint({
    #   user_reactives$user_reactives$user_latitude
    # })
    # output$debug3 <- shiny::renderPrint({
    #   user_reactives$user_reactives$user_longitude
    # })
    ####################################################### end debug ####
  } # END of server function

  #### Wrap the App ####
  app_wrapped <- shiny::shinyApp(
    ui = ui, server = server
  )
  # shiny::runApp(meteoland_app)
  return(app_wrapped)
}