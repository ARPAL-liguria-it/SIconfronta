#' 01_aim UI Function
#'
#' @description A shiny Module for selecting which elaboration will be
#' performed by the {Comparat} shinyAPP.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a {reactiveValues} list.
#' @return a list of radio buttons with the elaborations and an action button
#' and an action button for saving the results.
#'
#' @noRd
#'
#' @import shiny
#' @import markdown
mod_aim01_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(


    column(7,

      # selecting the aim of the comparison ----
      radioButtons(
        ns("aim"),
        label = h4("Cosa vuoi fare"),
        choices = c(
          "Confrontare due serie di dati complete" = "2samples",
          "Confrontare due serie di dati di cui una completa e l'altra riassunta nei suoi parametri" = "2samples_par",
          "Cofrontare una serie di dati con un valore medio noto esattamente" = "1sample_mu",
          "Confrontare una serie di dati con un valore di deviazione standard noto esattamente" = "1sample_sigma",
          "Confrontare due valori dotati di incertezza estesa" = "2values_unc"
        ),
        width = "100%"
      ),

      actionButton(
        ns("nextbtn"),
        label = "Avanti",
        icon = icon("circle-right")
      ),
    ),


    column(5,
      # conditional tabset with examples and instructions for the different options ----
      tabsetPanel(
        id = ns("example"),
        type = "hidden",

        tabPanel("2samples",
                 includeMarkdown(
                   system.file("rmd", "help_aim01_2samples.Rmd", package = "comparat")
                 )),

        tabPanel("2samples_par",
                 includeMarkdown(
                   system.file("rmd", "help_aim01_2samples_par.Rmd", package = "comparat")
                 )),

        tabPanel("1sample_mu",
                 withMathJax(includeMarkdown(
                   system.file("rmd", "help_aim01_1sample_mu.Rmd", package = "comparat")
                 ))),

        tabPanel("1sample_sigma",
                 withMathJax(includeMarkdown(
                   system.file("rmd", "help_aim01_1sample_sigma.Rmd", package = "comparat")
                 ))),

        tabPanel("2values_unc",
                 withMathJax(includeMarkdown(
                   system.file("rmd", "help_aim01_2values_unc.Rmd", package = "comparat")
                 )))
      )


    )


  ))
}

#' 01_aim Server Functions
#'
#' @description A shiny Module for selecting which elaboration will be
#' performed by the {Comparat} shinyAPP.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return a reactiveValues with the option chosen in UI stored into \code{r$aim01$aim}.
#'  Possible values are \code{"2samples"}, \code{"2samples_par"}, \code{"1sample_mu"},
#'  \code{"1sample_sigma"} and \code{"2values_unc"}.
#'
#' @noRd
#'
#' @import shiny
mod_aim01_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # updating examples and instructions depending on the option selected ----
    observeEvent(input$aim, {
      updateTabsetPanel(inputId = "example", selected = input$aim)
    })


    # adding the selected option to the r list ----
    r$aim01 <- reactiveValues()

    observeEvent(input$nextbtn, {
      showModal(
        modalDialog(
          title = "Non potrai tornare indietro",
          "Confermi la tua scelta?",
          easyClose = TRUE,
          footer = tagList(
            actionButton(ns("yesbtn"), "S\u00EC"),
            modalButton("No")
          )
        )
      )
    })

    observeEvent(input$yesbtn, {
      removeModal()

      r$aim01$aim <- input$aim
    })

  })
}

## To be copied in the UI
# mod_aim01_ui("aim01_1")

## To be copied in the server
# mod_aim01_server("aim01_1")
