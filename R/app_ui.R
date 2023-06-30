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

      #### Switch for input selection ----
      tabPanel("Scopo",
               mod_aim01_ui("scopo")
               ),

      #### Tab for data loading ----
      tabPanel("Dati",
                mod_loadfile02_ui("dati")
               ),

      #### Results ----
      tabPanel("Confronti",
               mod_compare03_ui("confronto")
               ),

      #### Reporting ----
       tabPanel("Report",
                fluidPage(mod_report04_ui("report"))),

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
