#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @noRd
#'
#' @import shiny
#' @import plotly
#' @importFrom future plan multisession
#' @importFrom shinyjs hidden
#' @importFrom bslib bs_theme
future::plan(future::multisession)

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    bslib::page_navbar(
      theme = bslib::bs_theme(version = 5, bootswatch = "sandstone"),
      title = tags$div(class = "navbar-brand", href = "#",
                       tags$img(src = "www/comparatlogo.png",
                                alt = "Comparat",
                                height = 50)),
      window_title = "Comparat",
      inverse = FALSE,
      position = "static-top",
      fluid = TRUE,
      collapsible = TRUE,
      lang = "it",

      #### Tab for data loading ----
      tabPanel("Dati",
                 sidebarLayout(
                 sidebarPanel(
                   width = 2,
                     # User inputs
                     ## 1. Load a csv file
                     mod_loadfile_ui("loadfile_1"),

               ## 2. Select the variable of the parameter
               shinyjs::hidden(
                 selectizeInput(
                   "parvar",
                   label = "Analiti",
                   selected = NULL,
                   choices = NULL,
                   multiple = TRUE,
                   options = list(maxItems = 1)
                 )
               ),
               ## 3. Select the variable for response
               shinyjs::hidden(
                 selectizeInput(
                   "response",
                   label = "Risposte",
                   selected = NULL,
                   choices = NULL,
                   multiple = TRUE,
                   options = list(maxItems = 1)
                 )
               ),
               ## 4. Select the grouping variable
               shinyjs::hidden(
                 selectizeInput(
                   "group",
                   label = "Gruppo",
                   selected = NULL,
                   choices = NULL,
                   multiple = TRUE,
                   options = list(maxItems = 1)
                 )
               )
               ),
              # Output: table with input data
                 mainPanel(width = 9, DT::DTOutput("inputdata"))
               )
               ),

      #### Results ----
      tabPanel("Confronti",
                 sidebarLayout(
                   sidebarPanel(
                     width = 2,
                     # User inputs
                     ## 1. Filter by parameter
                     selectizeInput(
                       "parameter",
                       label = "Analita",
                       selected = NULL,
                       choices = NULL,
                       multiple = TRUE,
                       options = list(maxItems = 1)
                     ),
                     # Module with plots and test results
                     mod_compareinput_ui("tests_1"),
                     ## 2. Save the results
                     mod_save_ui("save_1", "Salva", "Cancella")
                   ),
                   mainPanel(width = 10,
                             fluidRow(
                             column(4,
                              mod_comparesummary_ui("tests_1")
                                    ),
                             column(8,
                                  tabsetPanel(id = "testpanel",
                                              type = "tabs",
                                              tabPanel("Normalità", mod_compareshapirotest_ui("tests_1")),
                                              tabPanel("Medie", mod_comparettest_ui("tests_1")),
                                              tabPanel("Varianze", mod_compareftest_ui("tests_1"))
                                              )

                                    )
                             )
                   )
                 )
               ),

      #### Reporting ----
       tabPanel("Report",
                fluidPage(mod_makereport_ui("makereport_1"))),

      # #### Readme ----
      tabPanel("Leggimi",
               fluidPage(includeMarkdown(
                 system.file("rmd",
                             "readme.Rmd",
                             package = "comparat")
               )))

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Comparat",
    ),
    # Add here other external resources
    shinyjs::useShinyjs()
  )
}
