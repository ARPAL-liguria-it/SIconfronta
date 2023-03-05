#' loadfile UI Function
#'
#' @description A shiny Module for loading csv dataset.
#'   The dataset is provided by clicking on the corresponding UI folder icon and
#'   by browsing the the desired file location.
#'   The file to be imported need to have:
#'   \itemize{
#'    \item a .csv extension;
#'    \item at least 2 \code{character} column;
#'    \item at least 1 \code{numeric} variable columns.
#'   }
#'   Column headers are optional while dot "." as decimal separator is mandatory.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param label Short text, default is "CSV file".
#' @return A UI widget with a button folder icon and a text label.
#'
#' @noRd
#'
#' @import shiny
mod_loadfile_ui <- function(id, label = "CSV file") {
  ns <- NS(id)

  tagList(
    fileInput(
      ns("file"),
      label,
      accept = ".csv",
      multiple = FALSE,
      buttonLabel = icon("folder"),
      placeholder = "vuoto"
    )
  )
}

#' loadfile Server Functions
#'
#' @description A shiny Module for loading csv dataset.
#'   The dataset is provided by clicking on the corresponding UI folder icon and
#'   by browsing the the desired file location.
#'   \itemize{
#'    \item a .csv extension;
#'    \item at least 2 \code{character} column;
#'    \item at least 1 \code{numeric} variable columns.
#'   }
#'   Column headers are optional while dot "." as decimal separator is mandatory.
#'   When \code{stringAsFactors = TRUE} character columns are converted to \code{factor}.
#'
#' @details Data are imported by \code{data.table::fread} which takes care of guessing
#'  the field separator and handles the text delimiter, such as "".
#'  Decimal separator is the dot "." and numeric values with comma "," as decimal
#'  separator will be treated as characters.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param stringAsFactors Conversion of strings to factor (\code{TRUE} or \code{FALSE}).
#'  Default is \code{TRUE}.
#' @return a data.frame with values from the input csv file.
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
mod_loadfile_server <- function(id, stringsAsFactors) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    userFile <- reactive({
      # nel caso non venga caricato un file, non viene eseguito nulla
      validate(
        need(
          input$file$name,
          message =
"In attesa di un file .csv con almeno tre colonne contenenti:
1. il nome degli analiti che si vogliono esaminare;
2. una variabile testuale che distingua i due gruppi da cofrontare;
3. i valori misurati."
        ),
        need(input$file$datapath, message = FALSE)
      )

      input$file
    })

    # il nome del file caricato viene scritto nella console di R
    observe({
      msg <- sprintf("File %s was uploaded", userFile()$name)
      cat(msg, "\n")
    })

    # i dati vengono trasportati dal file a un data.frame
    dataframe <- reactive({
      data.table::fread(userFile()$datapath,
                        header = "auto",
                        stringsAsFactors = stringsAsFactors)
    })

  })
}

## To be copied in the UI
# mod_loadfile_ui("loadfile_1")

## To be copied in the server
# mod_loadfile_server("loadfile_1")
