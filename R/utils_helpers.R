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

#' Summary arranged on rows for two or more groups
#'
#' @description The function returns a table with max, mean, median, min, sd and n
#'  values arranged on rows while groups are on columns. Numbers are formatted as
#'  text in order to provide the desired significant figures.
#'
#' @param data the \code{data.frame} or \code{data.table} to be summarised.
#' @param response a string with the name of the variable to summarise.
#' @param group a string with the name of the grouping variable.
#' @param signif a integer with the number of desired significant figures.
#'
#' @return a \code{data.table} with 6 rows and \eqn{n + 1} columns for \eqn{n}
#'   levels of the group variable.
#'
#' @export
rowsummary <- function(data,
                       response,
                       group,
                       signif = 3L){

  stopifnot(is.data.frame(data),
            is.character(response),
            is.character(group),
            all.equal(signif, as.integer(signif)),
            response %in% colnames(data),
            group %in% colnames(data))

  mydata <- data.table(data)
  lvl <- levels(as.factor(mydata[[group]]))
  roworder <- c("n", "max", "mean", "median", "min", "sd")
  fm <- as.formula(paste("statistica", '~', group))

  mydata[, lapply(.SD, function(x) {
            c(
              n = .N,
              max = max(x, na.rm = TRUE) %>% sprintf('%#.*g', signif, .),
              mean = mean(x, na.rm = TRUE) %>% sprintf('%#.*g', signif, .),
              median = median(x, na.rm = TRUE) %>% sprintf('%#.*g', signif, .),
              min = min(x, na.rm = TRUE) %>% sprintf('%#.*g', signif, .),
              sd = sd(x, na.rm = TRUE) %>% sprintf('%#.*g', signif, .)
            )
              }),
         by = group,
         .SDcols = response][,
          "statistica" := rep(roworder, length(lvl))] %>%
   dcast(eval(fm), value.var = response) %>% .[roworder]
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
  htmlstring %>%
    { gsub("<h4>", "\n###", .) } %>%
    { gsub(" </h4>", "  \n" , .) } %>%
    { gsub("<h5>", "\n", .) } %>%
    { gsub("</h5>", "", .) } %>%
    { gsub("<ul>", "\n", .) } %>%
    { gsub("</ul>", "  \n", .) } %>%
    { gsub("<li>", "\n  *", .) } %>%
    { gsub("</li>", "", .) } %>%
    { gsub("\u03b1", "$\\\\alpha$", .) } %>%
    { gsub("\u03bd", "$\\\\nu$", .) } %>%
    { gsub("\u2013", "\\\\textendash\\\\", .) } %>%
    { gsub("\u21e8", "\n $\\\\Rightarrow$", .) } %>%
    { gsub("\u00b1", "$\\\\pm$", .) } %>%
    { gsub("R\u00B2", "$\\\\mathrm{R}^2$", .) } %>%
    { gsub("≠", "$\\\\neq$", .) } %>%
    { gsub("≤", "$\\\\leq$", .) } %>%
    { gsub("\u2264", "$\\\\leq$", .) } %>%
    { gsub("\u2260", "$\\\\neq$", .) } %>%
    { gsub("<b>", "**", .) } %>%
    { gsub("</b>", "**", .) } %>%
    { gsub("<i>", "_", .) } %>%
    { gsub("</i>", "_", .) } %>%
    { gsub("</br>", "  \n ", .) }
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
