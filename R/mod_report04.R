#' Report04 UI Function
#'
#' @description A shiny module for simple reporting by {pdf} Rmarkdown.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return a text input (\code{title}), an area text input (\code{description}),
#'  a group of check boxes (\code{content}) and a download button
#'  (\code{makereport}).
#'
#' @noRd
#'
#' @import shiny
mod_report04_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Informazioni aggiuntive"),
    textInput(ns("title"), label = "Titolo del report", width = "80%"),
    textAreaInput(ns("description"), label = "Descrizione dell'esperimento", width = "80%"),
    textAreaInput(ns("discussion"), label = "Interpretazione dei risultati", width = "80%"),

    hr(),

    checkboxGroupInput(ns("content"),
                       label = h4("Test da includere nel report"),
                       width = "80%",
                       choices = "",
                       selected = ""),

    hr(),

    downloadButton(ns("makereport"), label = "Crea il report",
      icon = icon("wand-magic-sparkles"),
      width = '25%')

  )
}

#' makereport Server Functions
#'
#' @description A shiny module for simple reporting by {pdf} Rmarkdown.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param inputreport a list of saved results to be passed to the {rmd}
#'  report template. Additionally, the module uses a report template named
#'  {comparison_report.Rmd} and a pdf logo named {"logoarpal.pdf"} located in
#'  the {"comparat/inst"} folder.
#' @return a {pdf} report compiled following the {comparison_report.Rmd} template.
#'  The report compilation is performed as a \code{future_promise} from package
#'  {promises}.
#'
#' @noRd
#'
#' @import data.table
#' @importFrom devtools session_info
mod_report04_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # dinamically update the checkboxgroupinput ----
    observeEvent(r$compare03$saved_flag, {
      mylist <- reactiveValuesToList(r$compare03)
      mylist <- mylist[names(mylist) %notin% c("myparameter", "saved_flag")]
      # removing not saved results
      mylist <- sapply(mylist, function(x) isTRUE(x$saved)) |>
        (\(x) mylist[x])()

      normality <- sapply(mylist, function(x) ! is.na(x$normality)) |>
        sum() |>
        (\(x) ifelse(x >= 1, "normality", NA))()
      names(normality) <- "Normalit\u00E0 e outliers"

      ttest <- sapply(mylist, function(x) ! is.na(x$ttest)) |>
        sum() |>
        (\(x) ifelse(x >= 1, "ttest", NA))()
      names(ttest) <- ifelse(r$aim01$aim == "2values_unc", "Confronto tra valori", "Confronto tra medie")

      ftest <- sapply(mylist, function(x) ! is.na(x$ftest)) |>
        sum() |>
        (\(x) ifelse(x >= 1, "ftest", NA))()
      names(ftest) <- "Confronto tra varianze"

      mychoices <- c(normality, ttest, ftest)
      mychoices <- mychoices[!is.na(mychoices)]

      freezeReactiveValue(input, "content")
      updateCheckboxGroupInput(session,
                               "content",
                               choices = mychoices,
                               selected = mychoices)
    })

   sysdate <- ifelse(isTRUE(getOption("shiny.testmode")), "testdate",
                     Sys.Date() |> as.character())

    r$report04 <- reactiveValues()

    output$makereport <- downloadHandler(
      filename = function() {
        paste0("comparison_report-", sysdate, ".pdf")
      },
      content = function(file) {
        withProgress(message = "Sto scrivendo il report...", {
        # The report template is copied in a temporary directory to prevent
        # user permission issues
        reportpath <- system.file("rmd", "comparison_report.Rmd",
                                  package = "comparat")
        logopath <- system.file("rmd", "logo.pdf",
                                package = "comparat")

        tempReport <- tempfile(fileext = ".Rmd")
        tempLogo <- tempfile(fileext = ".pdf")
        file.copy(reportpath, tempReport, overwrite = TRUE)
        file.copy(logopath, tempLogo, overwrite = TRUE)

        r$report04$logo <- logopath
        r$report04$info <- devtools::session_info()
        r$report04$title <- input$title
        r$report04$description <- input$description
        r$report04$discussion <- input$discussion
        r$report04$content <- input$content
        r$report04$testmode <- isTRUE(getOption("shiny.testmode"))

        # input parameters for the rmd file ----
        params <- isolate(lapply(r, reactiveValuesToList))

        n_par <- length(params$compare03) - 2
        for (i in n_par) {
          Sys.sleep(1)
          incProgress(1 / n_par)
        }

        # the report is compiled in a separate R environment with a future promise
        render_report(input = tempReport,
                      output = file,
                      params = params)

        })
      }
    )

  })
}
