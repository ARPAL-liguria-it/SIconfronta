#' makereport UI Function
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
mod_makereport_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h2("Un po' di contesto"),
    textInput(ns("title"), label = "Titolo del report"),
    textAreaInput(ns("description"), label = "Descrizione dell'esperimento"),

    tags$hr(),

    tags$h2("Cosa includere nel report"),
    checkboxGroupInput(ns("content"), label = "Test",
                       choices = c("Normalit\u00E0 e outliers" = "shapirotest",
                                   "Confronto tra medie" = "ttest",
                                   "Confronto tra varianze" = "ftest"),
                       selected = c("shapirotest",
                                    "ttest",
                                    "ftest")),

    tags$hr(),

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
mod_makereport_server <- function(id, inputreport){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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

        # input parameters for the rmd file
        params <- list(
          logo = logopath,
          title = input$title,
          description = input$description,
          content = input$content,
          data = inputreport(),
          info = devtools::session_info()
        )

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
