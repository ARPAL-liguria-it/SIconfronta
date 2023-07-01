#' compare UI Function: 1 sample vs known standard deviation option
#'
#' @description A shiny Module for basic one-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality, presence of outliers and variance
#'   comparison by hypothesis tesing.
#'
#' @details Normality is checked by using the Shapiro-Wilk test.
#'   Possible outliers are inspected by generalized extreme studentized deviate test.
#'   Variance values are compared by chi-squared test.
#'
#'   Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return Five UI widgets:
#' \itemize{
#'  \item{udm}{a text input for the unit of measurements.
#'    It is used for axes description and reporting, it is optional and it can be left blank.}
#'  \item{sd}{a numeric input widget for typing the known reference standard deviation value.}
#'  \item{submit}{an action button to submit the sd value to calculations.}
#'  \item{alternative}{a radiobutton widget with alternative test hypothesis.
#'    Choices are "different" or "greater", default is "different".}
#'  \item{significance}{a radiobutton widgted with test confidence levels.
#'    Choices are "90\%", "95\%", "99\%", default is "95\%".}
#' }
#'
#' @noRd
#'
#' @import shiny
mod_compare034_1sample_sigma_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # 1. write the unit of measurment (optional)
    textInput(ns("udm"),
              "Unit\u00E0 di misura",
              ""),

    hr(style = "border-top: 1px solid #000000;"),

    # 2. known reference standard deviation
    numericInput(
      ns("sd"),
      "Deviazione standard di riferimento",
      0,
      min = 0),

    # 3. submit button
    actionButton(
      ns("submit"),
      "Calcola",
      icon = icon("calculator")
      ),

    hr(style = "border-top: 1px solid #000000;"),

      # 4. select the test significant level
      radioButtons(
        ns("significance"),
        "Livello di confidenza",
        choices = c(
          "90%" = 0.90,
          "95%" = 0.95,
          "99%" = 0.99
        ),
        selected = 0.95
      ),

      # 5. select the test alternative hypothesis
      radioButtons(
        ns("alternative"),
        "Ipotesi alternativa",
        choices = c("\u2260" = "different",
                    ">" = "greater"),
        selected = "different"
    )

  )
}

#' compare UI Function: 1 sample vs known mean option
#'
#' @description A shiny Module for basic one-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality, presence of outliers and mean
#'   comparison by hypothesis tesing.
#'
#' @details Normality is checked by using the Shapiro-Wilk test.
#'   Possible outliers are inspected by generalized extreme studentized deviate test.
#'   Mean values are compared by the t-test.
#'
#'   Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return A two column fluidrow. In the first column a boxplot and a summary table
#' are reported, whereas in the second column test results are reported in two
#' separate tabs.
#'
#' @noRd
#'
#' @import shiny
mod_compare034_1sample_sigma_output_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    column(4,
           ""),

    column(10,
           tabsetPanel(
             id = ns("tabresults"),
             type = "tabs",

             tabPanel("Normalit\u00E0"),
             tabPanel("Medie")
             )
           )
    ))
}

#' compare Server Function
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality, presence of outliers and, mean
#'   and variance comparison hypothesis tests are performed.
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
#'  and Shapiro-Wilk test and \eqn{$chi$^2}-test results formatted in HTML.
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
#' @importFrom plotly renderPlotly plot_ly add_boxplot add_markers layout config
mod_compare034_1sample_sigma_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}
