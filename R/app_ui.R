#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    navbarPage(
      theme = bslib::bs_theme(version = 5, bootswatch = "sandstone"),
      title = tags$div(class = "navbar-brand", href = "#",
                       tags$img(src = "www/comparatlogo.png",
                                alt = "Comparat",
                                height = 50)),

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
               ## 3. Select the variable for group 1
               shinyjs::hidden(
                 selectizeInput(
                   "group1",
                   label = "Serie 1",
                   selected = NULL,
                   choices = NULL,
                   multiple = TRUE,
                   options = list(maxItems = 1)
                 )
               ),
               ## 4. Select the variable for group 2
               shinyjs::hidden(
                 selectizeInput(
                   "group2",
                   label = "Serie 2",
                   selected = NULL,
                   choices = NULL,
                   multiple = TRUE,
                   options = list(maxItems = 1)
                 )
               )
               ),
              # Output: table with input data
                 mainPanel(width = 9, DT::dataTableOutput("inputdata"))
               )
               )#,

      #### Results ----
      # tabPanel("Grafici",
      #            sidebarLayout(
      #              sidebarPanel(
      #                width = 2,
      #                # User inputs
      #                ## 1. Filter by parameter
      #                selectizeInput(
      #                  "parameter",
      #                  label = "Analita",
      #                  selected = NULL,
      #                  choices = NULL,
      #                  multiple = TRUE,
      #                  options = list(maxItems = 1)
      #                )#,
      #                ## 2. Save the results
      #                #calibrationsaveUI("save", "Salva", "Cancella")
      #              ),
      #              mainPanel(width = 10, "PROVA")
      #            )
      #          ),
      #
      # #### Reporting ----
      # tabPanel("Report",
      #          fluidPage()),
      #
      # #### Readme ----
      # tabPanel("Leggimi",
      #          fluidPage())

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
