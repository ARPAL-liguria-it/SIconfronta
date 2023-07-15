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

      normality <-
        sapply(mylist, function(x) ! is.na(x$normality)) |>
        sum() |>
        (\(x) ifelse(x >= 1, "normality", NA))()
      names(normality) <- "Normalit\u00E0 e outliers"

      ttest <- sapply(mylist, function(x) ! is.na(x$ttest)) |>
        sum() |>
        (\(x) ifelse(x >= 1, "ttest", NA))()
      names(ttest) <- "Confronto tra medie"

      ftest <- sapply(mylist, function(x) ! is.na(x$ftest)) |>
        sum() |>
        (\(x) ifelse(x >= 1, "ftest", NA))()
      names(ftest) <- "Confronto tra varianze"

      mychoices <- c(normality, ttest, ftest)
      mychoices <- mychoices[!is.na(mychoices)]

      updateCheckboxGroupInput(session,
                               "content",
                               choices = mychoices,
                               selected = mychoices)
    })


    r$report04 <- reactiveValues()

    output$makereport <- downloadHandler(
      filename = function() {
        paste0("comparison_report-", Sys.Date(), ".pdf")
      },
      content = function(file) {
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

        # input parameters for the rmd file ----
        params <- isolate(lapply(r, reactiveValuesToList))

        id <- showNotification(
        	"Preparazione del report...",
                duration = NULL,
                closeButton = FALSE
                            )
        on.exit(removeNotification(id), add = TRUE)

        # the report is compiled in a separate R environment with a future promise
        render_report(input = tempReport,
                      output = file,
                      params = params)

      }
    )

  })
}

## To be copied in the UI
# mod_makereport_ui("makereport_1")

## To be copied in the server
# mod_makereport_server("makereport_1")
