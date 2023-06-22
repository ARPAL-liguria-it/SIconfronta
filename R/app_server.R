#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyjs show hide
app_server <- function(input, output, session) {

  r <- reactiveValues()
  mod_aim01_server("scopo", r)
  mod_loadfile02_server("dati", r)
  mod_compare03_server("confronto", r)


}
