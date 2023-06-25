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
#' @return Three UI {shiny} input widgets:
#' \itemize{
#'  \item{udm}{a text input for the unit of measurements.
#'    It is used for axes description and reporting, it is optional and it can be left blank.}
#'  \item{alternative}{a radiobutton widget with alternative test hypothesis.
#'    Choices are "different" or "greater", default is "different".}
#'  \item{significance}{a radiobutton widgted with test confidence levels.
#'    Choices are "90\%", "95\%", "99\%", default is "95\%".}
#' }
#'
#' @noRd
#'
#' @import shiny
mod_compare031_2samples_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(

      # 1. write the unit of measurment (optional)
      textInput(ns("udm"),
                "Unit\u00E0 di misura",
                ""),

      # 2. select the test significant level
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

      # 3. select the test alternative hypothesis
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
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
mod_compare031_2samples_output_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    column(5,
           plotly::plotlyOutput(ns("boxplot"), width = "100%"),
           DT::DTOutput(ns("summarytable"))
           ),

    column(7,
           tabsetPanel(
             id = ns("tabresults"),
             type = "tabs",

             tabPanel("Normalit\u00E0",
                      htmlOutput(ns("shapirotest")),
                      htmlOutput(ns("outliers"))

                      ),
             tabPanel("Medie"),
             tabPanel("Varianze")
             )
           )
    ))
}

#' compare Server Function for two groups of values
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
#' @importFrom plotly renderPlotly
#' @importFrom DT renderDT
mod_compare031_2samples_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r$compare03x <- reactiveValues()

    # storing input values into r$compare03x reactiveValues ----

    ## selected parameter
    observeEvent(r$compare03$myparameter, {

      r$compare03x$parameter <- r$compare03$myparameter

      r$compare03x$data <- r$loadfile02$data[
        get(r$loadfile02$parvar) == r$compare03$myparameter]
    })

    ## unit of measurement
    observeEvent(input$udm, {

      udmclean <- gsub("[()\\[\\]]", "", input$udm, perl = TRUE)
      r$compare03x$udm <- udmclean
    })

    ## alternative hypothesis
    observeEvent(input$alternative, {

      r$compare03x$alternative <- input$alternative
    })

    ## test confidence level
    observeEvent(input$significance, {

      r$compare03x$significance <- input$significance
    })


    # preparing the reactive dataset for outputs ----
    rownumber <- reactive({
      req(r$compare03x$data)

      nrow(r$compare03x$data)
      })

    key <- reactive(seq(from = 1, to = rownumber()))

    # using the row index to identify outliers
    keys <- reactiveVal()

    suppressWarnings(
      observeEvent(plotly::event_data("plotly_click", source = "boxplot"), {

        key_new <- plotly::event_data("plotly_click", source = "boxplot")$key
        key_old <- keys()

        if (key_new %in% key_old) {
          keys(setdiff(key_old, key_new))
        } else {
          keys(c(key_new, key_old))
        }

      }),
      classes = "plotly_unregistered_event_warning")

    # flag per i punti selezionati
    is_outlier <- reactive(key() %in% keys())

    # assembling the dataframe
    input_data <- reactive({
      data.frame(
        key = key(),
        outlier = is_outlier(),
        response = r$compare03x$data[[r$loadfile02$responsevar]],
        group = r$compare03x$data[[r$loadfile02$groupvar]]
      )
    })

    # subset of non outliers
    selected_data <- reactive({
      req(input_data())

      input_data()[input_data()$outlier == FALSE, ]
    })


    # reactive boxplot ----
    plotlyboxplot <- reactive({
      req(input_data())

      boxplot_2samples(
        data = input_data(),
        group = r$loadfile02$groupvar,
        response = r$loadfile02$responsevar,
        udm = r$compare03x$udm
      )
    })

    output$boxplot <- plotly::renderPlotly(plotlyboxplot())


    # reactive summary table ----
    summarytable <- reactive({
      req(selected_data())

      rowsummary_2samples(
        data = selected_data(),
        group = "group",
        response = "response",
        udm = r$compare03x$udm
      )

    })

    output$summarytable <- DT::renderDT(summarytable(),
                                        options = list(dom = "t"),
                                        rownames = FALSE)


    # results of normality check ----
    shapiro_text <- "<b>Gruppo %s:</b> %s (W = %.3f, <i>p</i>-value = %.4f)</br>"

    # levels of the grouping factor
    lvl <- reactive({
      req(selected_data())

      levels(selected_data()$group)
    })

    shapirotest_list <- reactive({
      req(lvl())

      sapply(lvl(), function(x) {
        shapiro_output <- selected_data()[which(selected_data()$group == x),
                                          "response"] |>
          fct_shapiro()

        sprintf(shapiro_text,
                x,
                shapiro_output$result,
                shapiro_output$W,
                shapiro_output$pvalue)
      })
    })

    output$shapirotest <- renderText(
      paste("<h4> Test per la verifica della normalit\u00E0 (Shapiro-Wilk) </h4></br>",
            paste(shapirotest_list(), collapse = ""))
    )


    # results for outliers check ----
    out_text <- "<b>Gruppo %s:</b></br> %s a un livello di confidenza del 95%% </br> %s a un livello di confidenza del 99%% </br></br>"

    outtest_list <- reactive({
      req(selected_data())

      sapply(lvl(), function(x) {
        outtest_output95 <- selected_data()[which(selected_data()$group == x), "response"] |>
          fct_gesd(significance = 0.95)

        outtest_output99 <- selected_data()[which(selected_data()$group == x), "response"] |>
          fct_gesd(significance = 0.99)

        sprintf(out_text,
                x,
                outtest_output95$text,
                outtest_output99$text)
      })
    })

    output$outliers <- renderText(
      paste("<h4> Test per identificare possibili outliers (GESD) </h4></br>",
            paste(outtest_list(), collapse = ""))
    )







  })
}
