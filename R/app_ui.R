#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom future plan multisession
#' @importFrom bslib bs_theme
future::plan(future::multisession)

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    navbarPage(
      id = "navbar",
      theme = bslib::bs_theme(version = 5, bootswatch = "sandstone"),
      title = tags$div(class = "navbar-brand", href = "#",
                       tags$img(src = "www/comparatlogo.png",
                                alt = "Comparat",
                                height = 50)),
      windowTitle = "Comparat",
      padding = 50,
      inverse = FALSE,
      position = "static-top",
      fluid = FALSE,
      collapsible = TRUE,
      lang = "it",

      # Navbar items ----
      tabPanel("Scopo", value = "aim", mod_aim01_ui("scopo")),
      tabPanel("Dati", value = "data", mod_loadfile02_ui("dati")),
      tabPanel("Confronti", value = "compare", mod_compare03_ui("confronto")),
      tabPanel("Report", value = "report", mod_report04_ui("report")),
      tabPanel("Leggimi", value = "readme",
               includeMarkdown(system.file("rmd", "readme.Rmd",package = "comparat")))

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
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
