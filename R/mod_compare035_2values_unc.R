#' compare UI Function: 2 values with given extended uncertainty option
#'
#' @description A shiny Module for comparing two measurement values with
#'  given extended uncertainy.
#'
#' @details Comparison is performed by \eq{E_n} calculation.
#'  The test result is formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return one UI widget {udm} in which the unit of measurements can be typed.
#'
#' @noRd
#'
#' @import shiny
mod_compare035_2values_unc_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # 1. write the unit of measurment (optional)
    textInput(ns("udm"),
              "Unit\u00E0 di misura",
              "")
  )
}

#' compare UI Function: 2 values with given extended uncertainty option
#'
#' @description A shiny Module for comparing two measurement values with
#'  given extended uncertainy.
#'
#' @details Comparison is performed by \eq{E_n} calculation.
#'  The test result is formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return A two column fluidrow. In the first column a boxplot and a summary table
#' are reported, whereas in the second column test results are reported in two
#' separate tabs.
#'
#' @noRd
#'
#' @import shiny
mod_compare035_2values_unc_output_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    column(4,
           ""),

    column(10,
           tabsetPanel(
             id = ns("tabresults"),
             type = "tabs",

             tabPanel("Confronto tra valori")
             )
           )
    ))
}

#' compare Server Function: 2 values with given extended uncertainty option
#'
#' @description A shiny Module for comparing two measurement values with
#'  given extended uncertainy.
#'
#' @details Comparison is performed by \eq{E_n} calculation.
#'  The test result is formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a {reactiveValues} storing data produced in the other modules.
#' in \code{r$loadfile02$} the following values can be found:
#' \itemize{
#'    \item{data} is the imported data.frame;
#'    \item{parvar} is the data.frame column name in which parameters are stored;
#'    \item{parlist} is the list of parameters provided by the data.frame;
#'    \item{groupvar} is the data.frame column name in which groub labels are stored;
#'    \item{responsevar} is the data.frame column name in which the response numerical values are stored.
#'    }
#' @return A {plotly} interactive boxplot, a {DT} summary table
#'  and \eq{E_n} test in HTML format.
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
#' @importFrom plotly renderPlotly plot_ly add_boxplot add_markers layout config
mod_compare035_2values_unc_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}
