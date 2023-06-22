#' compare UI Function: 2 samples with parameters option
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality, presence of outliers and, mean
#'   and variance comparison hypothesis tests are performed.
#'
#' @details Normality is checked by using the Shapiro-Wilk test.
#'   Possible outliers are inspected by generalized extreme studentized deviate test.
#'   Mean values are compared by the Welch test and Variances are compared by F-test.
#'
#'   Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return Seven UI widgets:
#' \itemize{
#'  \item{label}{a text input for typing the name of the second group of data.}
#'  \item{mean}{a numeric input widget for typing the mean of the second group of data.}
#'  \item{sd}{a numeric input widget for typing the standard deviation of the
#'  second group of data.}
#'  \item{n}{a numeric input widget for typing the sample size of the second group
#'  of data.}
#'  \item{submit}{an action button to submit the mean, sd and n values to calculations.}
#'  \item{alternative}{a radiobutton widget with alternative test hypothesis.
#'    Choices are "different" or "greater", default is "different".}
#'  \item{significance}{a radiobutton widgted with test confidence levels.
#'    Choices are "90\%", "95\%", "99\%", default is "95\%".}
#' }
#'
#' @noRd
#'
#' @import shiny
mod_compare032_2samples_par_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(

    hr(style = "border-top: 1px solid #000000;"),

    textInput(
      ns("label"),
      "Nome del secondo gruppo",
      ""
    ),

    numericInput(
      ns("mean"),
      "Valore medio",
      0,
      min = 0),

    numericInput(
      ns("sd"),
      "Deviazione standard",
      0,
      min = 0),

    numericInput(
      ns("n"),
      "valore medio",
      5,
      min = 5),

    actionButton(
      ns("submit"),
      "Calcola",
      icon = icon("calculator")
      ),

    hr(style = "border-top: 1px solid #000000;"),

      # 1. select the test significant level
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

      # 2. select the test alternative hypothesis
      radioButtons(
        ns("alternative"),
        "Ipotesi alternativa",
        choices = c("\u2260" = "different",
                    ">" = "greater"),
        selected = "different"
    )

  )
}

#' compare UI Function: 2 samples option
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality, presence of outliers and, mean
#'   and variance comparison hypothesis tests are performed.
#'
#' @details Normality is checked by using the Shapiro-Wilk test.
#'   Possible outliers are inspected by generalized extreme studentized deviate test.
#'   Mean values are compared by the Welch test and Variances are compared by F-test.
#'
#'   Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return A two column fluidrow. In the first column a boxplot and a summary table
#' are reported, whereas in the second column test results are reported in three
#' separate tabs.
#'
#' @noRd
#'
#' @import shiny
mod_compare032_2samples_par_output_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    column(4,
           ""),

    column(10,
           tabsetPanel(
             id = ns("testoptions"),
             type = "tabs",

             tabPanel("Normalit\u00E0"),
             tabPanel("Medie"),
             tabPanel("Varianze")
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
#'  and Shapiro-Wilk test, \eqn{t}-test and \eqn{F}-test results formatted in HTML.
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
#' @importFrom plotly renderPlotly plot_ly add_boxplot add_markers layout config
mod_compare032_2samples_par_server <- function(id, data, response, group, analyte) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}
