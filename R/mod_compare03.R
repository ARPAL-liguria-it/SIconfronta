#' compare UI Function:
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality, presence of outliers and, mean
#'   and variance comparison hypothesis tests are performed.
#'
#' @details Tests are performed depending on the data provided and the aim of the
#'   comparison: normality and outliers are checked both for 2 samples and 1 sample
#'   (options "2samples", "2samples_par", "1sample_mu" and "1sample_sigma" of
#'   of the {mod_aim01}); mean and variance comparison is performed for 2 samples
#'   and 1 sample (options "2samples", "2samples_par" and "1sample_mu" and
#'   alternatively "1sample_sigma" for mean and variance comparison,
#'   respectively) whereas for single pair of values, only the comparison is
#'   performed (option "2values_unc").
#'
#'   Normality is checked by using the Shapiro-Wilk test.
#'   Possible outliers are inspected by generalized extreme studentized deviate test.
#'   Mean values are compared by the Welch test for two samples and *t*-test for
#'   one sample option.
#'   Variances are compared by F-test for two samples and $\chi^2$-test for one sample option.
#'
#'   Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return Two UI radiobuttons widget for alternative test hypothesis and
#' confidence level, respectively.
#' \itemize{
#'  \item{alternative}{a radiobutton widget with alternative test hypothesis.
#'    Choices are "different" or "greater", default is "different".}
#'  \item{significance}{a radiobutton widgted with test confidence levels.
#'    Choices are "90\%", "95\%", "99\%", default is "95\%".}
#'  \item{udm}{a text input for the unit of measurements.
#'  It is used for axes description and reporting, it is optional and it can be left blank.}
#' }
#'
#' @noRd
#'
#' @import shiny
mod_compare03_ui <- function(id) {
  ns <- NS(id)
  tagList(sidebarLayout(
    sidebarPanel(
      width = 2,

      # 1. select the parameter
      selectizeInput(
        ns("parameter"),
        label = "Analita",
        selected = NULL,
        choices = NULL,
        multiple = FALSE,
        options = list(maxItems = 1)
      ),

      # 2. write the unit of measurment (optional)
      textInput(ns("udm"),
                "Unit\u00E0 di misura",
                ""),


      # different controls for the different data options ----
      tabsetPanel(
        id = ns("ctrls"),
        type = "hidden",

        tabPanel(
          "2samples",
          mod_compare031_2samples_inputs_ui("2samples")
        ),

        tabPanel(
          "2samples_par",
          mod_compare032_2samples_par_inputs_ui("2samples_par")
        ),

        tabPanel(
          "1sample_mu",
          mod_compare033_1sample_mu_inputs_ui("1sample_mu")
        ),

        tabPanel(
          "1sample_sigma",
          mod_compare034_1sample_sigma_inputs_ui("1sample_sigma")
        ),

        tabPanel("2values_unc",
                 "")

      ),


    # save and delete buttons
    tabsetPanel(
      id = ns("savedel"),
      type = "hidden",

      # show the save button when data is not saved
      tabPanel("save",
               # 5. click on the save button
               actionButton(
                 ns("save"),
                 "Salva",
                 icon = icon("floppy-disk")
               )),

      # show the delete button when data has been saved
      tabPanel("delete",
               # 6. click on the delete buttons (if you spot a mistake)
               actionButton(
                 ns("delete"),
                 "Cancella",
                 icon = icon("eraser")
               ))

      )
    ),


    mainPanel(width = 10,

              fluidRow(
                column(4,
                       ""),

                column(10,

                       # different outputs for the different data options
                       tabsetPanel(
                         id = ns("outputs"),
                         type = "hidden",

                         tabPanel("2samples",
                                  mod_compare031_2samples_output_ui("2samples")
                                  ),

                         tabPanel("2samples_par",
                                  mod_compare032_2samples_par_output_ui("2samples_par")
                                  ),

                         tabPanel("1sample_mu",
                                  mod_compare033_1sample_mu_output_ui("1sample_mu")
                                  ),

                         tabPanel("1sample_sigma",
                                  mod_compare034_1sample_sigma_output_ui("1sample_sigma")
                                  ),

                         tabPanel("2values_unc",
                                  mod_compare035_2values_unc_output_ui("2values_unc")
                                  )

                         )
                       )))
  ))
}

#' compare Server Function
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality by means of Shapiro-Wilk tests,
#'   outliers are inspected by generalized extreme studentized deviate test
#'   and by boxplots, and \eqn{t}-test and \eqn{F}-test for the mean and
#'   variance comparisons, respectively. Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param data a dataset with at least 2 character columns and a numeric columns.
#'  The module has been written to be used after the \code{mod_loadfile} and a filter
#'  on the analyte or parameter factor.
#' @param response a string from \code{columnames(data)} for the numeric variable to
#'   be compared by plotted on y-axes and compared by statistical tests.
#' @param group a string from \code{columnames(data)} for the factor which identifies
#'   the groups to be compared.
#' @param analyte a string from unique values of the analyte factor in \code{data}.
#'   It is used for axes description, only.
#' @return A {plotly} interactive boxplot, a {DT} summary table
#'  and Shapiro-Wilk test, \eqn{t}-test and \eqn{F}-test results formatted in HTML.
#'   \itemize{
#'      \item{boxplot}{A {plotly} boxplot with the response on the y-axis and groups on the x-axis.}
#'      \item{summary}{a {DT} table with \code{max}, \code{mean}, \code{median},
#'        \code{min} and \code{length} values.}
#'      \item{shapirotest}{HTML text with the results for data normality by Shapiro-Wilk test.}
#'      \item{gesdtest}{HTML text with the results for outliers detection by generalized extreme studentized deviate test.}
#'      \item{ttest}{HTML text with the results for mean comparison by \eqn{t}-test.}
#'      \item{ftest}{HTML text with the results for variance comparison by \eqn{F}-test.}
#'   }
#'   In addition, an unnamed list with the following reactive items is returned:
#'    \itemize{
#'      \item{data}{a dataset with a unique identifier (\code{key}),
#'      an outlier logical flag (\code{is_outlier}), the values of the response numerical
#'      variable (\code{response}) and the values of the group factor character variable
#'      (\code{group}) on the columns and the different data values on the rows.}
#'      \item{summarytbl}{a summary table returned by \code{rowsummary}.}
#'      \item{shapirotest}{a HTML formatted string with the results for the normality test.}
#'      \item{gesdtest}{a HTML formatted string with the results for the outliers test.}
#'      \item{ttest}{a HTML formatted string with the results for the t-test.}
#'      \item{ftest}{a HTML formatted string with the results for the F-test.}
#'    }
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
#' @importFrom plotly renderPlotly plot_ly add_boxplot add_markers layout config
mod_compare03_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # updating the UI ----

    # updating the controls in the sidebar
    observeEvent(r$aim01$aim, {
      req(length(r$aim01$aim) == 1)

      updateTabsetPanel(inputId = "ctrls", selected = r$aim01$aim)
    })

    # updating the output in the mainpanel
    observeEvent(r$aim01$aim, {
      req(length(r$aim01$aim) == 1)

     updateTabsetPanel(inputId = "output", selected = r$aim01$aim)
    })

    # updating the list of parameters
    parchoices <- reactive({
      req(levels(r$loadfile02$parlist) != 0)

      c("", levels(r$loadfile02$parlist))
    })

    observeEvent(r$loadfile02$parlist, {
      req(parchoices())

      updateSelectizeInput(session,
                           "parameter",
                           selected = "",
                           choices = parchoices()
                           )

    })

    # storing the selected parameter to the r reactiveValues
    r$compare03 <- reactiveValues()

    observeEvent(input$parameter, {
      req(input$parameter)
      req(input$parameter != "")

      r$compare03$myparameter <- input$parameter

    })

    # storing the udm to the r reactiveValues
    observeEvent(input$udm, {
      req(input$parameter)
      req(input$parameter != "")

      r$compare03$myudm

    })


    # passing the r reactiveValues to different modules depending on the aim option ----
    observeEvent(parchoices(), {

    switch (r$aim01$aim,
      "2samples" = mod_compare031_2samples_server("2samples", r),
      "2samples_par" = mod_compare031_2samples_par_server("2samples_par", r),
      "1sample_mu" = mod_compare031_2samples_par_server("1sample_mu", r),
      "1sample_sigma" = mod_compare031_2samples_par_server("1sample_sigma", r),
      "2values_unc" = mod_compare031_2samples_par_server("2values_unc", r)
    )

    })

  })
}

## To be copied in the UI
# mod_tests_ui("tests_1")

## To be copied in the server
# mod_tests_server("tests_1")
