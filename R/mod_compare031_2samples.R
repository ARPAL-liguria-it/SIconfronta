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

      # 2. select the test alternative hypothesis
      radioButtons(
        ns("alternative"),
        "Ipotesi alternativa",
        choices = c("\u2260" = "different",
                    ">" = "greater"),
        selected = "different"
      ),

      # 3. select the test significant level
      radioButtons(
        ns("significance"),
        "Livello di confidenza",
        choices = c(
          "90%" = 0.90,
          "95%" = 0.95,
          "99%" = 0.99
        ),
        selected = 0.95
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
  tagList(


    tabsetPanel(
      id = ns("help_results"),
      type = "hidden",

      tabPanel("help",
               includeMarkdown(system.file("rmd", "help_compare031_2samples.Rmd", package = "comparat"))),

      tabPanel("results",

    fluidRow(
    column(5,
           plotly::plotlyOutput(ns("boxplot"), width = "100%"),
           DT::DTOutput(ns("summarytable"))
           ),

    column(7,
           tabsetPanel(
             id = ns("tabresults"),
             type = "tabs",

             tabPanel("Normalit\u00E0",
                      h4("Test per la verifica della normalit\u00E0 (Shapiro-Wilk)"),
                      htmlOutput(ns("shapirotest")),
                      hr(),
                      h4("Test per identificare possibili outliers (GESD)"),
                      htmlOutput(ns("outliers"))
                      ),
             tabPanel("Medie",
                      h4("Test per la differenza tra medie (Welch)"),
                      htmlOutput(ns("ttest"))
                      ),
             tabPanel("Varianze",
                      h4("Test per il rapporto tra varianze (Fisher)"),
                      htmlOutput(ns("ftest")))
             )
           )
    )

    )
    )
  )
}

#' compare Server Function for two groups of values
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality, presence of outliers and, mean
#'   and variance comparison hypothesis tests are performed. This module is
#'   supposed to be used as {mod_compare03} submodule.
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
#'In \code{r$compare03$myparameter} the selected parameter name is stored;
#' @return A {plotly} interactive boxplot, a {DT} summary table
#'  and Shapiro-Wilk test, \eqn{t}-test and \eqn{F}-test results formatted in HTML.
#'  a reactiveValues \code{r$compare03x} with the following items:
#'\itemize{
#'  \item{parameter} the selected parameter;
#'  \item{udm} the unit of measurement;
#'  \item{data} the input data subsetted by the selected parameter.
#'  \item{}
#'
#'}
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

      # updating the tabset switching from help to results tabs
      help_results <- ifelse(r$compare03x$parameter == "", "help", "results")
      updateTabsetPanel(inputId = "help_results", selected = help_results)
    })

    ## unit of measurement
    observeEvent(input$udm, ignoreNULL = FALSE, {
      udmclean <- gsub("[()\\[\\]]", "", input$udm, perl = TRUE)
      r$compare03x$udm <- udmclean
    })

    ## alternative hypothesis
    observeEvent(input$alternative, ignoreNULL = FALSE, {
      r$compare03x$alternative <- input$alternative
    })

    ## test confidence level
    observeEvent(input$significance, ignoreNULL = FALSE, {
      r$compare03x$significance <- input$significance
    })


    # preparing the reactive dataset for outputs ----
    mydata <- reactive({
      r$loadfile02$data[get(r$loadfile02$parvar) == r$compare03x$parameter]
    })

    rownumber <- reactive({
      req(mydata())

      nrow(mydata())
    })

    key <- reactive(seq(from = 1, to = rownumber()))

    # using the row index to identify outliers
    keys <- reactiveVal()

    observeEvent(plotly::event_data("plotly_click", source = "boxplot"), {
      req(plotly::event_data("plotly_click", source = "boxplot")$key)

      key_new <- plotly::event_data("plotly_click", source = "boxplot")$key
      key_old <- keys()

      if (key_new %in% key_old) {
        keys(setdiff(key_old, key_new))
      } else {
        keys(c(key_new, key_old))
      }

    })

    # reset the keys index when changing the parameter
    observeEvent(r$compare03$myparameter, {
      keys(NULL)
    })

    # flag per i punti selezionati
    is_outlier <- reactive(key() %in% keys())

    # assembling the dataframe
    input_data <- reactive({

      data.frame(
        key = key(),
        outlier = is_outlier(),
        response = mydata()[[r$loadfile02$responsevar]],
        group = mydata()[[r$loadfile02$groupvar]]
      )

    })

    # subset of non outliers
    selected_data <- reactive({
      req(input_data())

      input_data()[input_data()$outlier == FALSE,]
    })

    # min number of values for the two groups
    minval <- reactive({
      req(!is.null(selected_data()))

      sapply(levels(selected_data()$group),
                 function(x) {
                   selected_data()[selected_data()$group == x, ] |>
                     nrow()
                   }) |>
        min()
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

    output$boxplot <- plotly::renderPlotly({

      plotlyboxplot()
      })


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
    shapiro_text <-
      "<b>Gruppo %s:</b> %s (W = %.3f, <i>p</i>-value = %.4f)</br>"

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

        sprintf(
          shapiro_text,
          x,
          shapiro_output$result,
          shapiro_output$W,
          shapiro_output$pvalue
        )
      })
    })

    shapiro_html <- reactive(paste(shapirotest_list(), collapse = ""))

    output$shapirotest <- renderText({
      validate(
        need(minval() >= 5,
             message = "Servono almeno 5 valori per poter eseguire i test")
      )

      shapiro_html()
    })


    # results for outliers check ----
    out_text <-
      "<b>Gruppo %s:</b></br> %s a un livello di confidenza del 95%% </br> %s a un livello di confidenza del 99%% </br></br>"

    outtest_list <- reactive({
      req(selected_data())
      req(minval() >= 5)

      sapply(lvl(), function(x) {
        outtest_output95 <-
          selected_data()[which(selected_data()$group == x), "response"] |>
          fct_gesd(significance = 0.95)

        outtest_output99 <-
          selected_data()[which(selected_data()$group == x), "response"] |>
          fct_gesd(significance = 0.99)

        sprintf(out_text,
                x,
                outtest_output95$text,
                outtest_output99$text)
      })

    })

    outliers_html <- reactive(paste(outtest_list(), collapse = ""))

    output$outliers <- renderText({
      validate(
        need(minval() >= 5,
             message = FALSE)
      )

      outliers_html()
    })


    #### results for the t-test ----
    ttest_list <- reactive({
      req(selected_data())
      req(r$compare03x$significance)
      req(r$compare03x$alternative)

      fct_ttest_2samples(
        selected_data(),
        "response",
        "group",
        significance = as.numeric(r$compare03x$significance),
        alternative = r$compare03x$alternative
      )

    })

    ttest_text <-
"<b>H0:</b> %s </br>
<b>H1:</b> %s
<ul>
  <li> Differenza tra le medie (valore e intervallo di confidenza) = %s %s, %s \u2013 %s %s</li>
  <li> <i>t</i> sperimentale = %.3f </li>
  <li> <i>t</i> critico (\u03b1 = %.3f, \u03bd = %.3f) = %.3f </li>
  <li> <i>p</i>-value = %.4f </li>
</ul>
\u21e8 %s"

    ttest_html <- reactive({

      sprintf(
        ttest_text,
        ttest_list()$hypotheses[[1]],
        ttest_list()$hypotheses[[2]],
        ttest_list()$difference[[1]],
        r$compare03x$udm,
        ttest_list()$difference[[2]],
        ttest_list()$difference[[3]],
        r$compare03x$udm,
        ttest_list()$test[[3]],
        ttest_list()$test[[2]],
        ttest_list()$test[[1]],
        ttest_list()$test[[4]],
        ttest_list()$test[[5]],
        ttest_list()$result
      )

    })

    output$ttest <- renderText({
      validate(
        need(minval() >= 5,
             message = "Servono almeno 5 valori per poter eseguire i test")
      )

      ttest_html()
      })


    #### results for the F-test ----
    ftest_list <- reactive({
      req(selected_data())
      req(r$compare03x$significance)
      req(r$compare03x$alternative)

      fct_ftest_2samples(
        selected_data(),
        "response",
        "group",
        significance = as.numeric(r$compare03x$significance),
        alternative = r$compare03x$alternative
      )

    })

    ftest_text <-
"<b>H0:</b> %s </br>
<b>H1:</b> %s
<ul>
  <li> Rapporto tra le varianze (valore e intervallo di confidenza) = %s, %s \u2013 %s</li>
  <li> <i>F</i> sperimentale = %.3f </li>
  <li> <i>F</i> critico (\u03b1 = %.3f, \u03bd = %.0f, %.0f) = %s </li>
  <li> <i>p</i>-value = %.4f </li>
</ul>
\u21e8 %s"

    ftest_html <- reactive({

      sprintf(
        ftest_text,
        ftest_list()$hypotheses[[1]],
        ftest_list()$hypotheses[[2]],
        ftest_list()$ratio[[1]],
        ftest_list()$ratio[[2]],
        ftest_list()$ratio[[3]],
        ftest_list()$test$fsper,
        ftest_list()$test$alpha,
        ftest_list()$test$dof[[1]],
        ftest_list()$test$dof[[2]],
        ftest_list()$test$ftheo,
        ftest_list()$test$pvalue,
        ftest_list()$result
      )

    })

    output$ftest <- renderText({
      validate(
        need(minval() >= 5,
             message = "Servono almeno 5 valori per poter eseguire i test")
      )

      ftest_html()
      })

    # saving the outputs ----
    observeEvent(input_data(), {

      # output dataset
      r$compare03x$data <- mydata()[, !r$loadfile02$parvar, with = FALSE]
      r$compare03x$data[, "rimosso"] <- ifelse(is_outlier() == TRUE, "sì", "no")

      # summary table
      r$compare03x$summary <- summarytable()

      # test results
      r$compare03x$normality <- shapiro_html()
      r$compare03x$outliers <- outliers_html()
      r$compare03x$ttest <- ttest_html()
      r$compare03x$ftest <- ftest_html()

      # the plot is saved only when clicking on the save button
    })

  })
}
