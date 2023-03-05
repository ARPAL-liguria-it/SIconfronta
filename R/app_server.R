#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyjs show hide
app_server <- function(input, output, session) {
  #### tabs are blocked until the requested information are provided ----
  # comparison and result tabs are blocked
  shinyjs::disable(selector = '.navbar-nav a[data-value="Confronti"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Report"')

  # the comparison tab is activated when the required data is loaded
  # and the variables selected
  observeEvent(c(input$parvar, input$group, input$response), {
    req(input$parvar %in% charcol())
    req(input$group %in% charcol())
    req(input$response %in% numcol())

    shinyjs::enable(selector = '.navbar-nav a[data-value="Confronti"')
  })

  # the comparison table is disabled when new not-compliant data is loaded
  observeEvent(c(input$parvar, input$group, input$response), {
    req(
      input$parvar %notin% charcol() |
        input$group %notin% charcol() |
        input$response %notin% numcol() |
        is.null(input$parvar) |
        is.null(input$group) |
        is.null(input$response)
    )

    shinyjs::disable(selector = '.navbar-nav a[data-value="Confronti"')
  })

  # the report tab is enabled only when some results have been saved
  observeEvent(saved_data(), {
    if (length(saved_data()) == 0) {
      shinyjs::disable(selector = '.navbar-nav a[data-value="Report"')
    } else {
      shinyjs::enable(selector = '.navbar-nav a[data-value="Report"')
    }
  })

  #### get the content of loaded file ----
  loadedfile <- mod_loadfile_server("loadfile_1", stringsAsFactors = TRUE)

  # get the variable from file column names
  ## numeric variables
  numcol <- reactive({
    colnames(loadedfile())[sapply(loadedfile(), is.numeric)]
  })
  ## string variables
  charcol <- reactive({
    colnames(loadedfile())[sapply(loadedfile(), is.factor)]
  })
  # number of numeric variables
  sumnum <- reactive(length(numcol()))
  # number of string variables
  sumchar <- reactive(length(charcol()))

  #### update input for variable selection ----
  observeEvent(charcol(), {
    shinyjs::show("parvar")

    updateSelectizeInput(session,
                         "parvar",
                         selected = charcol()[1],
                         choices = charcol())
  })

  #### update input for group variable ----
  observeEvent(numcol(), {
    shinyjs::show("group")

    updateSelectizeInput(session,
                         "group",
                         selected = charcol()[2],
                         choices = charcol())
  })

  #### update input for response variable ----
  observeEvent(numcol(), {
    shinyjs::show("response")

    updateSelectizeInput(session,
                         "response",
                         selected = numcol()[1],
                         choices = numcol())
  })

  #### update input with the analyte list ----
  parlist <- reactive({
    req(input$parvar)

    loadedfile()[[input$parvar]] %>% unique
  })

  observeEvent(parlist(), {
    updateSelectizeInput(session,
                         "parameter",
                         selected = parlist()[1],
                         choices = parlist())
  })

  #### Table with the loaded data ----
  output$inputdata <- DT::renderDT(
    loadedfile(),
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
     )
  )


  #### Filtering data by parameter ----
  filtered_data <- reactive({
    req(input$parameter %in% parlist())
    req(input$parvar %in% charcol())

    loadedfile()[get(input$parvar) == input$parameter]
    })


  #### Visual inspection for outliers and data summary ----
  clean_data <- mod_compare_server(
                  "tests_1",
                  data = reactive(filtered_data()),
                  response = reactive(input$response),
                  group = reactive(input$group),
                  analyte = reactive(input$parameter)
  )

  #### Saving results to a reactive list ----
  saved_data <- mod_save_server(
                  "save_1",
                  group = reactive(input$group),
                  response = reactive(input$response),
                  selected_parameter = reactive(input$parameter),
                  inputlist = reactive(clean_data)
  )

  # reordering data by parameters and remove null elements
  saved_ordered <- reactive({
                      ord <- saved_data()[parlist()]
                      ord[!sapply(ord,is.null)]
                      })

  mod_makereport_server("makereport_1",
                        inputreport = reactive(saved_ordered()))

}
