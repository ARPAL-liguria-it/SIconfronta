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
  tagList(tabsetPanel(
    id = ns("help_results"),
    type = "hidden",

    tabPanel("help",
             includeMarkdown(
               system.file("rmd", "help_compare035_2values_unc.Rmd", package = "comparat")
             )),

    tabPanel("results",

             fluidRow(
               column(
                 5,
                 plotly::plotlyOutput(ns("boxplot"), width = "100%"),
                 DT::DTOutput(ns("summarytable"))
               ),

               column(6,
                      tabsetPanel(
                        id = ns("tabresults"),
                        type = "hidden",

                        tabPanel(
                          h4("Confronto tra valori (E number)"),
                          htmlOutput(ns("entest"))
                        )
                      ))

             ))

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


    r$compare03x <- reactiveValues()

    # storing input values into r$compare03x reactiveValues ----

    ## selected parameter
    observeEvent(r$compare03$myparameter, {
      r$compare03x$parameter <- r$compare03$myparameter

      # updating the tabset switching from help to results tabs
      help_results <- ifelse(r$compare03x$parameter == "", "help", "results")
      updateTabsetPanel(inputId = "help_results", selected = help_results)
    })

    ## unit of measurement
    observeEvent(input$udm, ignoreNULL = FALSE, {
      udmclean <- gsub("[()\\[\\]]", "", input$udm, perl = TRUE)
      r$compare03x$udm <- udmclean
    })

    # preparing the reactive dataset for outputs ----
    mydata <- reactive({
      r$loadfile02$data[get(r$loadfile02$parvar) == r$compare03x$parameter]
    })

    # assembling the dataframe
    input_data <- reactive({

      data.frame(
        response = mydata()[[r$loadfile02$responsevar]],
        uncertainty = mydata()[[r$loadfile02$uncertainty]],
        group = mydata()[[r$loadfile02$groupvar]]
      )

    })

    # scatter plot with error bars ----
    plotlyboxplot <- reactive({
      req(input_data())

      boxplot_2values_unc(
        data = input_data(),
        group = r$loadfile02$groupvar,
        response = r$loadfile02$responsevar,
        uncertainty = r$loadfile02$uncertainty,
        udm = r$compare03x$udm
      )

    })

    output$boxplot <- plotly::renderPlotly({
      plotlyboxplot()
    })


    # summary table ----
    summarytable <- reactive({
      req(input_data())

      rowsummary_1sample_sigma(
        data = input_data(),
        group = "group",
        response = "response",
        uncertainty = "uncertainty",
        udm = r$compare03x$udm
      )

    })


    output$summarytable <- DT::renderDT({

      DT::datatable(
        summarytable(),
        options = list(dom = "t"),
        rownames = FALSE
      )

    })

    #### results for the En-test ----
    entest_list <- reactive({
      req(input_data())

      fct_entest_2values_unc(
        data = input_data(),
        response = "response",
        group = "group",
        uncertainty = "uncertainty"
      )

    })

    entest_text <-
      "<b>H0:</b> %s </br>
<b>H1:</b> %s
<ul>
  <li> Differenza tra i due valori (valore e intervallo di confidenza) = %s %s, %s \u2013 %s %s</li>
  <li> E<sub>n</sub> sperimentale = %s </li>
  <li> E<sub>n</sub> critico = %s </li>
</ul>
\u21e8 %s"

    entest_html <- reactive({

      sprintf(
        entest_text,
        entest_list()$hypotheses[[1]],
        entest_list()$hypotheses[[2]],
        entest_list()$difference[[1]],
        r$compare03x$udm,
        entest_list()$difference[[2]],
        entest_list()$difference[[3]],
        r$compare03x$udm,
        entest_list()$test[[3]],
        entest_list()$test[[2]],
        chitest_list()$result
      )

    })

    output$entest <- renderText({

      entest_html()
    })

  })
}
