#' Number of decimals for given significant figures
#'
#' @description The function returns the number of decimals required for given
#'  significant figures.
#'
#' @param value a number for which the number of decimals required to get the
#'    desired number of significant figures should be calculated
#' @param signif the number of desired significant figures
#'
#' @return The number of decimals required for the \code{signif} significant figures.
#'
#' @noRd
signiftodigits <- function(value,
                           signif = 4L) {

  stopifnot(is.numeric(value),
            all.equal(signif, as.integer(signif)))

  if (is.infinite(value)){
    0
  } else {

  # the number is converted to text with the desired significant figures
  sprintf_txt <- paste0("%#.", signif, "g")
  value_text <- sprintf(sprintf_txt, value)
  # splitting integers from decimals
  value_digits <- strsplit(value_text, "\\.")
  # counting the integers
  value_integers <- nchar(value_digits[[1]][[1]])
  # checking if the number is scientific notation
  is_scientific <- "e" %in% strsplit(value_digits[[1]][[2]], "")[[1]]
  # if the integers are not enough for the requested significant figures,
  # or the data has been expressed in scientific notation, the number of decimals
  # is 0, otherwise, the decimals are counted.
  ndigits <- ifelse(value_integers >= signif | is_scientific,
                    0,
                    nchar(value_digits[[1]][[2]]))
  ndigits

  }
}

#' Formatting a number with a given number of significant figures
#'
#' @description The function returns a character values with a number formatted
#' with the desired significant figures
#'
#' @param number the input number to be formatted
#' @param sigfig an integer with the number of desired significant figures
#'
#' @return a character value with the number formatted with the desired
#' significant figures
#'
#' @noRd
format_sigfig <- function(number, sigfig = 4L){
  stopifnot(
    is.numeric(number),
    is.integer(sigfig)
  )
sprintf("%.*f", signiftodigits(number, sigfig), number)

}

#' Conversion of an HTML formatted string to a RMarkdown string
#'
#' @description The function substitutes some common HTML tag to their
#'  RMarkdown counterparts. Handled HTML tags are \code{<h4></h4>},
#'  \code{<h5></h5>}, \code{<ul></ul>}, \code{<li></li>}, \code{<b></b>},
#'  \code{<i></i>} and \code{</br>}.
#'
#' @param htmlstring the HTML string to be converted.
#'
#' @return a string with the HTML tags replaced by their RMarkdown counterparts.
#'
#' @noRd
htmltormarkdown <- function(htmlstring){

  stopifnot(
    !is.null(htmlstring)
  )

  if (is.na(htmlstring)) {

    NA

  } else {

  htmlstring |>
    (\(x) gsub("<h4>", "\n###", x) )() |>
    (\(x) gsub(" </h4>", "  \n" , x) )() |>
    (\(x) gsub("<h5>", "\n####", x) )() |>
    (\(x) gsub("</h5>", "  \n", x) )() |>
    (\(x) gsub("<ul>", "\n", x) )() |>
    (\(x) gsub("</ul>", "  \n", x) )() |>
    (\(x) gsub("<li>", "\n  *", x) )() |>
    (\(x) gsub("</li>", "", x) )() |>
    (\(x) gsub("\u03b1", "$\\\\alpha$", x) )() |>
    (\(x) gsub("\u03bd", "$\\\\nu$", x) )() |>
    (\(x) gsub("\u2013", "\\\\textendash\\\\", x) )() |>
    (\(x) gsub("\u21e8", "\n $\\\\Rightarrow$", x) )() |>
    (\(x) gsub("\u00b1", "$\\\\pm$", x) )() |>
    (\(x) gsub("R\u00B2", "$\\\\mathrm{R}^2$", x) )() |>
    (\(x) gsub("\u2264", "$\\\\leq$", x) )() |>
    (\(x) gsub("\u2260", "$\\\\neq$", x) )() |>
    (\(x) gsub("<b>", "**", x) )() |>
    (\(x) gsub("</b>", "**", x) )() |>
    (\(x) gsub("<i>", "_", x) )() |>
    (\(x) gsub("</i>", "_", x) )() |>
    (\(x) gsub("</br>", "  \n ", x) )() |>
    (\(x) gsub("\u03C7<sup>2</sup>", "$\\\\chi^2$", x) )() |>
    (\(x) gsub("\u03C7\u00B2", "$\\\\chi^2$", x) )()
  }
}

#' Rendering of an RMarkdown report as future promise
#'
#' @description The function passes some parameters to a RMarkdown file for
#'  automatic reporting. The report is processed as a \code{future_promise}
#'  from the {promises} package.
#'
#' @param input the {rmd} report template.
#' @param output a temporary file for writing the content of the new report.
#' @param params the parameters to be passed to the remport template.
#'
#' @return the function passes the parameters to a {Rmd} report template as
#'  {future_promise}
#'
#' @noRd
#' @importFrom rmarkdown render
#' @importFrom promises future_promise
render_report <- function(input, output, params) {
  promises::future_promise({
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
                    )
  }, seed = TRUE)
}

#' {\%notin\%} operator
#'
#' @description The \code{\%notin\%} operator returns the opposite result of the
#'  \code{\%in\%} operator.
#'
#' @noRd
`%notin%` = Negate(`%in%`)
