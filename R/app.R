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
  mirai::daemons(12)
  shiny::onStop(function() {mirai::daemons(0)})

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
      # echart themes reg
      echarts4r::e_theme_register(
        '{"color":["#14ABCC","#7CC69A","#E3DF68","#ED51C1"],"backgroundColor":"#191A1A"}',
        name = "emf_colors"
      ),
      echarts4r::e_theme_register(
        '{"color":["#682714","#C75C00","#F2A400","#FAE094","#FEFEE3"],"backgroundColor":"#191A1A"}',
        name = "emf_colors_soil"
      ),
      echarts4r::e_theme_register(
        '{"color":["#006E37","#F9F7EA"],"backgroundColor":"#191A1A"}',
        name = "emf_colors_climate"
      ),
      echarts4r::e_theme_register(
        '{"color":["#2A5676","#387893","#5599AB","#7BB8C1","#A3D6D6","#D2EEEA"],"backgroundColor":"#191A1A"}',
        name = "emf_colors_wb"
      ),
      echarts4r::e_theme_register(
        '{"color":["#EF4868","#ECD999"],"backgroundColor":"#191A1A"}',
        name = "emf_colors_stress"
      ),
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
        icon = shiny::icon("eye"),
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
        shiny::sidebarLayout(
          position = "left", fluid = TRUE,
          sidebarPanel = shiny::sidebarPanel(
            width = 2,
            mod_userInput("user_input")
          ), # END of sidebarPanel
          mainPanel = shiny::mainPanel(
            width = 10,
            shiny::fluidRow(
              shiny::column(
                width = 7,
                mod_mapOutput("map_output")
              ),
              shiny::column(
                width = 5,
                mod_tsOutput("ts_output")
              )
            )
          ) # END of mainPanel
        ) # END of sidebarLayout
      ), # END of main (Explore) tab
      # Download tab
      shiny::tabPanel(
        title = mod_tab_translateOutput("download_tab_translation"),
        icon = shiny::icon("save"),
        mod_downloadOutput("download_output")
      ), # END of donwload tab
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

    # modules
    user_reactives <- shiny::callModule(
      mod_user, 'user_input', lang
    )
    map_reactives <- shiny::callModule(
      mod_map, 'map_output',
      user_reactives$user_reactives,
      lang
    )
    ts_reactives <- shiny::callModule(
      mod_ts, 'ts_output',
      user_reactives$user_reactives,
      user_reactives$user_inputs_session,
      lang
    )
    download_reactives <- shiny::callModule(
      mod_download, "download_output",
      user_reactives$user_reactives, ts_reactives,
      lang
    )
    shiny::callModule(
      mod_techSpecs, "tech_specs_output",
      lang
    )

    # tab translations
    c(
      "main_tab_translation", "download_tab_translation",
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