#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # get the content of loaded file
  loaededfile <-
    mod_loadfile_server("loadfile_1", stringsAsFactors = TRUE)

  output$inputdata <- DT::renderDataTable(loaededfile(),
                                          rownames = FALSE,
                                          style = "bootstrap5",
                                          options = list(
                                            language = list(
                                              search = "Cerca",
                                              info = "Valori da _START_ a _END_ su _TOTAL_",
                                              lengthMenu = "Mostra _MENU_ valori",
                                              paginate = list(
                                                first = "Primo",
                                                last = "Ultimo",
                                                previous = "Precedente",
                                                `next` = "Successivo"
                                              )
                                            )
                                          ))
}
