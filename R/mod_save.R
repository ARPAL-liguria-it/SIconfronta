#' save UI Function
#'
#' @description A shiny Module for saving output to a named list.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return {save} and {delete} action buttons.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter
#' @importFrom ggplot2 scale_color_manual labs theme theme_bw
#' @importFrom shinyjs show hide
mod_save_ui <- function(id,
                        label1 = "save",
                        label2 = "delete") {
  ns <- NS(id)
  tagList(actionButton(ns("save"),
                       label1,
                       icon = icon("floppy-disk")),

          actionButton(ns("delete"),
                       label2,
                       icon = icon("eraser")))
}


#' @description A shiny Module for saving output to a named list.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return a list with the following items
#'  \itemize{
#'    \item{state}{Either 0 or 1. If 0 the other elements of the list are \code{NULL},
#'    else the expected results are stored.}
#'    \item{data}{a dataset with the values of the group factor character
#'      variable (with the actual group variable name) the values of the response
#'      numerical variable (with the actual response variable name) and an
#'      outlier logical flag (\code{rimosso}).}
#'    \item{summarytbl}{a summary table returned by \code{rowsummary}.}
#'    \item{boxplot}{a {ggplot2} boxplot.}
#'    \item{shapirotest}{a RMarkdown formatted string with the results for the normality test.}
#'    \item{ttest}{a RMarkdown formatted string with the results for the t-test.}
#'    \item{ftest}{a RMarkdown formatted string with the results for the F-test.}
#'  }
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyjs show hide
mod_save_server <- function(id,
                            group,
                            response,
                            selected_parameter,
                            inputlist){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    savedoutput <- reactiveValues(list = list())

    # the click on the save button triggers the saving of data into a list
    observeEvent(input$save, {
      req(inputlist())

      shapirotest_markdown <- htmltormarkdown(inputlist()$shapirotest()) %>%
                                paste(collapse = "")
      ttest_markdown <- htmltormarkdown(inputlist()$ttest()) %>%
                                paste(collapse = "")
      ftest_markdown <- htmltormarkdown(inputlist()$ftest()) %>%
                                paste(collapse = "")

      ## ggplot boxplot
      outcols <- c("TRUE" = "#999999", "FALSE" = "black")
      udmclean <- ifelse(inputlist()$udm() != "",
                         paste0(" (", inputlist()$udm(), ")"),
                         "")
      xlabtitle <- group()
      ylabtitle <- paste0(response(), udmclean)

      boxplot <- ggplot2::ggplot() +

                  ggplot2::geom_boxplot(data = inputlist()$data()[which(inputlist()$data()$outlier == FALSE),],
                                        ggplot2::aes(x = group,
                                                     y = response),
                                                     fill = "white",
                                                     col = "black",
                                                     outlier.shape = NA) +
                  ggplot2::geom_jitter(data = inputlist()$data(),
                                       ggplot2::aes(x = group,
                                                    y = response,
                                                    col = outlier),
                                       width = 0.2) +
                  ggplot2::labs(x = xlabtitle,
                                y = ylabtitle) +
                  ggplot2::scale_color_manual(values = outcols,
                                              breaks = c("TRUE", "FALSE"),
                                              labels = c("rimosso", "non rimosso"),
                                              name = ggplot2::element_blank()) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(legend.position = "top")

      # raw data table properly formatted
      dataset <- inputlist()$data()[, c("group", "response", "outlier")]
      colnames(dataset) <- c(group(), ylabtitle, "rimosso")
      dataset$rimosso <- gsub("TRUE", "s\u00EC", dataset$rimosso)
      dataset$rimosso <- gsub("FALSE", "no", dataset$rimosso)

      # summary data table properly formatted
      summarytbl <- inputlist()$summarytbl()
      summarytbl$statistica <- gsub("max", paste0("massimo", udmclean), summarytbl$statistica)
      summarytbl$statistica <- gsub("min", paste0("minimo", udmclean), summarytbl$statistica)
      summarytbl$statistica <- gsub("mean", paste0("media", udmclean), summarytbl$statistica)
      summarytbl$statistica <- gsub("median", paste0("mediana", udmclean), summarytbl$statistica)
      summarytbl$statistica <- gsub("sd", paste0("dev. std.", udmclean), summarytbl$statistica)

      ## saving the results into a list
      savedoutput$list[[selected_parameter()]]$state <- 1
      savedoutput$list[[selected_parameter()]]$parameter <- selected_parameter()
      savedoutput$list[[selected_parameter()]]$data <- dataset
      savedoutput$list[[selected_parameter()]]$summarytbl <- summarytbl
      savedoutput$list[[selected_parameter()]]$boxplot <- boxplot
      savedoutput$list[[selected_parameter()]]$shapirotest <- shapirotest_markdown
      savedoutput$list[[selected_parameter()]]$ttest <- ttest_markdown
      savedoutput$list[[selected_parameter()]]$ftest <- ftest_markdown

    })

    # the click on the delete button triggers the deletion of stored data
    observeEvent(input$delete, {
      savedoutput$list[[selected_parameter()]] <- NULL
      print(savedoutput$list)
    })

    # display save OR delete action button
    observeEvent(c(input$save, input$delete, selected_parameter()), {
       req(savedoutput$list)
       req(selected_parameter())

       if (is.null(savedoutput$list[[selected_parameter()]])) {
         shinyjs::show("save")
         shinyjs::hide("delete")
       } else {
         shinyjs::hide("save")
         shinyjs::show("delete")
       }
     })

    reactive(savedoutput$list)

  })
}

## To be copied in the UI
# mod_save_ui("save_1")

## To be copied in the server
# mod_save_server("save_1")
