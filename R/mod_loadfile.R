#' loadfile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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
#' @noRd
mod_loadfile_server <- function(id, stringsAsFactors) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    userFile <- reactive({
      # nel caso non venga caricato un file, non viene eseguito nulla
      validate(
        need(
          input$file$name,
          message =
            "In attesa di un file .csv con almeno tre colonne:
                          1. il nome degli analiti che si vogliono esaminare;
                          2. i valori per il primo gruppo;
                          3. i valori per il secondo gruppo."
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
