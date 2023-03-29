#' compare UI Function: inputs
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality by means of Shapiro-Wilk tests,
#'   compared by boxplots, and \eqn{t}-test and \eqn{F}-test for the mean and
#'   variance comparisons, respectively. Test results are formatted in HTML.
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
mod_compareinput_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(ns("alternative"),
                 "Ipotesi alternativa",
                 choices = c("\u2260" = "different",
                             ">" = "greater"),
                 selected = "different"),

    radioButtons(ns("significance"),
                 "Livello di confidenza",
                 choices = c("90%" = 0.90,
                             "95%" = 0.95,
                             "99%" = 0.99),
                 selected = 0.95),

    textInput(ns("udm"),
              "Unit\u00E0 di misura",
              ""),

  )
}

#' compare UI Function: generic summary
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality by means of Shapiro-Wilk tests,
#'   compared by boxplots, and \eqn{t}-test and \eqn{F}-test for the mean and
#'   variance comparisons, respectively. Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return A boxplot and a summary table.
#' \itemize{
#'  \item{boxplot}{a {plotly} output comparing the results for the two groups.}
#'  \item{summary}{a {DT} table with \code{max}, \code{mean}, \code{median},
#'    \code{min} and \code{length} values.}
#' }
#'
#' @noRd
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
mod_comparesummary_ui <- function(id){
  ns <- NS(id)
  tagList(

    plotly::plotlyOutput(ns("boxplot"), width = "100%"),
    DT::DTOutput(ns("summary"))

  )
}

#' compare UI Function: Shapiro-Wilk test results
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality by means of Shapiro-Wilk tests,
#'   compared by boxplots, and \eqn{t}-test and \eqn{F}-test for the mean and
#'   variance comparisons, respectively. Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return HTML text with the Shapiro-Wilk test results.
#'
#' @noRd
#'
#' @import shiny
mod_compareshapirotest_ui <- function(id){
  ns <- NS(id)
  tagList(

    htmlOutput(ns("shapirotest"))

  )
}

#' compare UI Function: \eqn{t}-test results
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality by means of Shapiro-Wilk tests,
#'   compared by boxplots, and \eqn{t}-test and \eqn{F}-test for the mean and
#'   variance comparisons, respectively. Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return HTML text with the \eqn{t}-test results.
#'
#' @noRd
#'
#' @import shiny
mod_comparettest_ui <- function(id){
  ns <- NS(id)
  tagList(

    htmlOutput(ns("ttest"))

  )
}

#' compare UI Function: \eqn{F}-test results
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality by means of Shapiro-Wilk tests,
#'   compared by boxplots, and \eqn{t}-test and \eqn{F}-test for the mean and
#'   variance comparisons, respectively. Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return HTML text with the \eqn{F}-test results.
#'
#' @noRd
#'
#' @import shiny
mod_compareftest_ui <- function(id){
  ns <- NS(id)
  tagList(

    htmlOutput(ns("ftest"))

  )
}

#' compare Server Function
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality by means of Shapiro-Wilk tests,
#'   compared by boxplots, and \eqn{t}-test and \eqn{F}-test for the mean and
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
#'      \item{ttest}{a HTML formatted string with the results for the t-test.}
#'      \item{ftest}{a HTML formatted string with the results for the F-test.}
#'    }
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
#' @importFrom plotly renderPlotly plot_ly add_boxplot add_markers layout config
mod_compare_server <- function(id, data, response, group, analyte) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #### Managing selected outliers ----
    # row index
    rownumber <- reactive({
      validate(
        need(data(), message = FALSE),
        need(response(), message = FALSE),
        need(group(), message = FALSE),
        need(analyte(), message = FALSE)
      )

      nrow(data())
    })
    key <- reactive(seq(from = 1, to = rownumber()))

    # using the row index to identify outliers
    keys <- reactiveVal()

    suppressWarnings(
      observeEvent(plotly::event_data("plotly_click", source = "boxplot"), {
      req(selected_data$data)

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

    # preparing the data.frame
    input_data <- reactive({
      data.frame(
        key = key(),
        outlier = is_outlier(),
        response = data()[[response()]],
        group = data()[[group()]]
      )
    })

    # filtering outliers
    selected_data <- reactiveValues(data = NULL)

    observe({
      req(input_data())

      selected_data$data <-
        input_data()[input_data()$outlier == FALSE, ]
    })

    # colors for clicked points
    cols <- reactive(ifelse(input_data()$outlier == TRUE,
                            "#999999",
                            "black"))
    # cleaned udm
    udmclean <- reactive(gsub("[()\\[\\]]", "", input$udm, perl = TRUE))
    # x axis title
    xlabtitle <- reactive(group())
    # y axis title
    ylabtitle <- reactive({
                    paste0(response(),
                    ifelse(udmclean() != "", paste0(" (", udmclean(), ")"), ""))
  })

    #### Render the boxplot ----
    output$boxplot <- plotly::renderPlotly(
      plotly::plot_ly(source = "boxplot") %>%
      plotly::add_boxplot(
        data = selected_data$data,
        y = ~ response,
        x = ~ group,
        name = "boxplot",
        type = "box",
        boxmean = TRUE,
        boxpoints = FALSE,
        color = I("#D55E00"),
        showlegend = FALSE
      ) %>%
        plotly::add_markers(
          data = input_data(),
          y = ~ response,
          x = ~ group,
          name = "valori",
          marker = list(
            color = I(cols()),
            colors = I(cols()),
            size = 10
          ),
          key = ~ key,
          hoverinfo = "y",
          hovertemplate = paste('%{y:.3s}', udmclean())
        ) %>%
        plotly::layout(
          showlegend = FALSE,
          title = NULL,
          xaxis = list(title = xlabtitle()),
          yaxis = list(title = ylabtitle())
        ) %>%
        plotly::config(displayModeBar = FALSE)
    )

    #### Render the summary table ----
    summarytable <- reactive({
      rowsummary(selected_data$data, "response", "group")
    })

    output$summary <- DT::renderDT(
      summarytable(),
      options = list(dom = "t"), rownames = FALSE
      )

    #### Shapiro-Wilk normality test results ----
    shapiro_text <- "<b>Gruppo %s:</b> %s (W = %.3f, <i>p</i>-value = %.4f)</br>"

    # levels of the grouping factor
    lvl <- reactive({
      req(selected_data$data)

      levels(selected_data$data$group)
    })

    shapirotest_list <- reactive({
      req(selected_data$data)

      sapply(lvl(), function(x) {
        shapiro_output <- selected_data$data$response[which(selected_data$data$group == x)] %>%
          fct_shapiro

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

    #### t-test results ----
    ttest_list <- reactive({
      req(selected_data$data)

      fct_ttest(selected_data$data, "response", "group",
                significance = as.numeric(input$significance),
                alternative = input$alternative)
    })

    ttest_text <-
"<h4> Test per differenza tra le medie </h4>
<b>H0:</b> %s </br>
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
        udmclean(),
        ttest_list()$difference[[2]],
        ttest_list()$difference[[3]],
        udmclean(),
        ttest_list()$test[[3]],
        ttest_list()$test[[2]],
        ttest_list()$test[[1]],
        ttest_list()$test[[4]],
        ttest_list()$test[[5]],
        ttest_list()$result
      )
    })

    output$ttest <- renderText(ttest_html())

    #### F-test results ----
    ftest_list <- reactive({
      req(selected_data$data)

      fct_ftest(selected_data$data, "response", "group",
                significance = as.numeric(input$significance),
                alternative = input$alternative)
    })

    ftest_text <-
"<h4> Test per rapporto tra le varianze </h4>
<b>H0:</b> %s </br>
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

    output$ftest <- renderText(ftest_html())

    list(
      data = reactive(input_data()),
      udm = reactive(udmclean()),
      summarytbl = reactive(summarytable()),
      shapirotest = reactive(shapirotest_list()),
      ttest = reactive(ttest_html()),
      ftest = reactive(ftest_html())
    )

  })
}

## To be copied in the UI
# mod_tests_ui("tests_1")

## To be copied in the server
# mod_tests_server("tests_1")
