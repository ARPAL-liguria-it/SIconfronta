#' Displays the results of a \eqn{t}-test for two groups of values summarised
#' by mean, standard deviation and number of values
#'
#' @description The function displays the results of a \eqn{t}-test performed
#'  on two groups of values summarised by mean, standard deviation and number
#'  of values.
#'  The returned text is suitable for the {comparat} {shiny} app.
#'
#' @param group1 a character value with the name of the first group.
#' @param mean1 a numeric value with the mean for the first group.
#' @param sd1 a numeric value with the standard deviation for the first group.
#' @param n1 a numeric value with the number of values for the first group.
#' @param group2 a character value with the name of the second group.
#' @param mean2 a numeric value with the mean for the second group.
#' @param sd2 a numeric value with the standard deviation for the second group.
#' @param n2 a numeric value with the number of values for the second group.
#' @param significance a number, typically either 0.90, 0.95 (default) or 0.99
#'   indicating the confidence level for the test.
#' @param alternative a character string specifying the alternative hypotesis,
#'   must be one of \code{"different"} or \code{"greater"}.
#'
#' @details \eqn{t}-test is calculated using the group with the numerically
#'  larger mean as first argument.
#'  As a consequence, for the means of two series of values \eqn{A} and \eqn{B},
#'  being the first one numerically greater than the second one, the alternative
#'  hypotheses tested can only be \eqn{\bar{A} \neq \bar{B}}
#'  (\code{alternative = "different"}) or \eqn{\bar{A} > \bar{B}}
#'  (\code{alternative = "greater"}).
#' @return A list with the following items:
#'  \describe{
#'    \item{hypotheses}{a named vector of strings, being \code{h0} and \code{h1}
#'    the null and alternative hypothesis, respectively.}
#'    \item{difference}{a named vector of numbers, being \code{mean},
#'    \code{lwrci} and \code{uprci} the difference in means of the two groups and
#'    the lower and upper ends of the confidence interval, respectively.
#'    The confidence interval is calculated considering both the \code{significance}
#'    and \code{alternative} arguments. For \code{alternative = "greater"} only the
#'    lower end of the confidence interval will be calculated.}
#'    \item{test}{a named vector of numbers, being \code{dof}, \code{tsper},
#'    \code{ttheo} and \code{pvalue} the degrees of freedom, the calculated value
#'    of the \eqn{t} statistic, the tabulated value of the \eqn{t} statistic and
#'    the \eqn{p}-value associated to the test. As in the original \code{t.test}
#'    function in base R, the statistic is calculated by performing a Welch test
#'    and approximating the actual number of degrees of freedom.}
#'    \item{result}{a string indicating whether H0 should be rejected or not.}
#'  }
#'
#' @export
#'
#' @importFrom stats sd qt pt
fct_ttest_2samples_par <- function(group1,
                                   mean1,
                                   sd1,
                                   n1,
                                   group2,
                                   mean2,
                                   sd2,
                                   n2,
                                   significance = 0.95,
                                   alternative = "different") {
  stopifnot(
    is.numeric(mean1),
    is.numeric(mean2),
    is.numeric(sd1),
    is.numeric(sd2),
    is.numeric(n1),
    is.numeric(n2),
    is.character(group1),
    is.character(group2),
    alternative %in% c("different", "greater")
  )

  # recoding the alternative hypothesis
  h1 <- switch (alternative,
    "different" = "two.sided",
    "greater" = "greater"
  )

  # recoding the significance level based on alternative hypothesis
  alpha <- switch (alternative,
                    "different" = significance + (1 - significance)/2,
                    "greater" = significance
  )


  mysummary <- data.frame(group = c(group1, group2),
                          n = c(n1, n2),
                          mean = c(mean1, mean2),
                          sd = c(sd1, sd2))
  colnames(mysummary) <- c("group", "n", "mean", "sd")

  # get the group with higher mean
  max_mean_gr <- mysummary$group[which.max(mysummary$mean)]
  min_mean_gr <- mysummary$group[which.min(mysummary$mean)]

  # get the mean, sd and n associated with the max and min means values
  max_mean_vl <- max(mysummary$mean)
  min_mean_vl <- min(mysummary$mean)
  max_mean_sd <- mysummary$sd[which.max(mysummary$mean)]
  min_mean_sd <- mysummary$sd[which.min(mysummary$mean)]
  max_mean_n <- mysummary$n[which.max(mysummary$mean)]
  min_mean_n <- mysummary$n[which.min(mysummary$mean)]

  # get the standard errors for the group with max and min means
  max_mean_stderr <- max_mean_sd/sqrt(max_mean_n)
  min_mean_stderr <- min_mean_sd/sqrt(min_mean_n)

  # t-test numerator
  diff_mean <- max_mean_vl - min_mean_vl
  # t-test denominator
  diff_stderr <- sqrt(max_mean_stderr^2 + min_mean_stderr^2)
  # t value
  tvalue <- diff_mean/diff_stderr

  # degree of freedom
  dof <- (max_mean_sd^2/max_mean_n + min_mean_sd^2/min_mean_n)^2/
    ( (max_mean_sd^4/(max_mean_n^2 * (max_mean_n - 1) )) + (min_mean_sd^4/(min_mean_n^2 * (min_mean_n - 1))) )

  # t critical and p-value
  tcritical <- stats::qt(alpha, dof)
  pvalue <- ifelse(h1 == "two.sided", 2 * stats::pt(tvalue, dof, lower.tail = FALSE), stats::pt(tvalue, dof, lower.tail = FALSE))

  # difference confidence interval
  ci <- diff_mean * c(-1, 1) * tcritical * diff_stderr

  # Being clear with some text
  h0_text <- switch (alternative,
    "different" = sprintf("media di %s = media di %s", max_mean_gr, min_mean_gr),
    "greater" = sprintf("media di %s \u2264 media di %s", max_mean_gr, min_mean_gr),
  )

  h1_text <- switch (alternative,
    "different" = sprintf("media di %s \u2260 media di %s", max_mean_gr, min_mean_gr),
    "greater" = sprintf("media di %s > media di %s", max_mean_gr, min_mean_gr)
  )

  positive <- switch (alternative,
                      "different" = sprintf("la media di %s e la media di %s sono statisticamente differenti", max_mean_gr, min_mean_gr),
                      "greater" = sprintf("la media di %s \u00E8 statisticamente maggiore della media di %s", max_mean_gr, min_mean_gr)
  )

  negative <- switch (alternative,
                      "different" = sprintf("la media di %s e la media di %s non sono statisticamente differenti", max_mean_gr, min_mean_gr),
                      "greater" = sprintf("la media di %s non \u00E8 statisticamente maggiore della media di %s", max_mean_gr, min_mean_gr)
  )

  result <- ifelse(tvalue < tcritical, negative, positive)

  list(hypotheses = c("h0" = h0_text,
                      "h1" = h1_text),
       difference = c("mean" = diff_mean |> format_sigfig(),
                      "lwrci" = ci[1] |> format_sigfig(),
                      "uprci" = ifelse(h1 == "two.sided", ci[2] |> format_sigfig(), Inf)),
       test = c("dof" = dof |> round(4),
                "alpha" = alpha,
                "tsper" = tvalue |> round(4),
                "ttheo" = tcritical |> round(3),
                "pvalue" = pvalue |> round(4)),
       result = result)

}

#' Displays the results of a \eqn{F}-test for two groups of values
#'
#' @description The function displays the results of a \eqn{F}-test performed
#'  on two groups of values.
#'  The returned text is suitable for the {comparat} {shiny} app.
#'
#' @param data a \code{data.frame} or \code{data.table} with the results
#'   relevant for testing. At least a two-levels grouping \code{factor} variable
#'   and a \code{numeric} vector with the measurements should be included.
#' @param response the name of a numeric vector in \code{data}.
#'   Quotation (" ") is not required.
#' @param group the name of a two-level factor variable that identifies the groups
#'   in \code{data}. Quotation (" ") is not required.
#' @param significance a number, typically either 0.90, 0.95 (default) or 0.99
#'   indicating the confidence level for the test.
#' @param alternative a character string specifying the alternative hypotesis,
#'   must be one of \code{"different"} or \code{"greater"}.
#'
#' @details \eqn{F}-test is calculated using the base-R function \code{var.test},
#'  with the group with the numerically larger variance as first argument.
#'  As a consequence, for the variances of two series of values \eqn{A} and \eqn{B},
#'  being the first one numerically greater than the second one, the alternative
#'  hypotheses tested can only be \eqn{\mathrm{Var}(A) \neq \mathrm{Var}(B)}
#'  (\code{alternative = "different"}) or \eqn{\mathrm{Var}(A) > \mathrm{Var}(B)}
#'  (\code{alternative = "greater"}).
#' @return A list with the following items:
#'  \describe{
#'    \item{hypotheses}{a named vector of strings, being \code{h0} and \code{h1}
#'    the null and alternative hypothesis, respectively.}
#'    \item{difference}{a named vector of numbers, being \code{ratio},
#'    \code{lwrci} and \code{uprci} the ratio of the variances of the two groups and
#'    the lower and upper ends of the confidence interval, respectively.
#'    The confidence interval is calculated considering both the \code{significance}
#'    and \code{alternative} arguments. For \code{alternative = "greater"} only the
#'    lower end of the confidence interval will be calculated.}
#'    \item{test}{a named vector of numbers, being \code{dof}, \code{fsper},
#'    \code{ftheo} and \code{pvalue} the degrees of freedom, the calculated value
#'    of the \eqn{F} statistic, the tabulated value of the \eqn{F} statistic and
#'    the \eqn{p}-value associated to the test.}
#'    \item{result}{a string indicating whether H0 should be rejected or not.}
#'  }
#'
#' @export
#'
#' @importFrom stats aggregate var.test sd qf
fct_ftest_2samples <- function(data,
                      response,
                      group,
                      significance = 0.95,
                      alternative = "different") {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
    response %in% colnames(data),
    group %in% colnames(data),
    alternative %in% c("different", "greater")
  )

  # recoding the alternative hypothesis
  h1 <- switch (alternative,
                "different" = "two.sided",
                "greater" = "greater"
  )

  # recoding the significance level based on alternative hypothesis
  alpha <- switch (alternative,
                   "different" = significance + (1 - significance)/2,
                   "greater" = significance
  )

  # defining the formula for groups
  myformula <- as.formula(paste(response, "~", group, sep = " "))
  # defining a function for a short summary
  summary_function <- function(x) c(n = length(x),
                                    mean = mean(x, na.rm = TRUE),
                                    sd = stats::sd(x, na.rm = TRUE))
  # get the summary
  mysummary <- do.call(data.frame, stats::aggregate(myformula, data, summary_function))
  colnames(mysummary) <- c(group, "n", "mean", "sd")
  # get the group with higher sd
  max_sd <- mysummary[[group]][which.max(mysummary$sd)]
  min_sd <- mysummary[[group]][which.min(mysummary$sd)]
  higher_sd <- data[which(data[[group]] == max_sd),][[response]]
  lower_sd <- data[which(data[[group]] == min_sd),][[response]]
  # # F-test results
  ftest <- stats::var.test(x = higher_sd, y = lower_sd,
                  alternative = h1, conf.level = significance)
  ratio <- (stats::sd(higher_sd)^2 / stats::sd(lower_sd)^2) |> format_sigfig()
  ratioconfint <- c(NA, NA)
  ratioconfint[1] <- ftest$conf.int[1] |> format_sigfig()
  ratioconfint[2] <- ftest$conf.int[2] |> format_sigfig()
  fvalue <- ftest$statistic |> round(4)
  dof <- ftest$parameter |> unname() # numerator and denominator
  fcritical <- c(stats::qf(1-alpha, dof[[1]], dof[[2]]),
                 stats::qf(alpha, dof[[1]], dof[[2]])) |> round(4)
  pvalue <- ftest$p.value |> round(4)

  # Being clear with some text
  h0_text <- switch (alternative,
                     "different" = sprintf("varianza di %s = varianza di %s", max_sd, min_sd),
                     "greater" = sprintf("varianza di %s \u2264 varianza di %s", max_sd, min_sd),
  )

  h1_text <- switch (alternative,
                     "different" = sprintf("varianza di %s \u2260 varianza di %s", max_sd, min_sd),
                     "greater" = sprintf("varianza di %s > varianza di %s", max_sd, min_sd)
  )

  positive <- switch (alternative,
                      "different" = sprintf("la varianza di %s e la varianza di %s sono statisticamente differenti", max_sd, min_sd),
                      "greater" = sprintf("la varianza di %s \u00E8 statisticamente maggiore della varianza di %s", max_sd, min_sd)
  )

  negative <- switch (alternative,
                      "different" = sprintf("la varianza di %s e la varianza di %s non sono statisticamente differenti", max_sd, min_sd),
                      "greater" = sprintf("la varianza di %s non \u00E8 statisticamente maggiore della varianza di %s", max_sd, min_sd)
  )

  result <- switch (alternative,
                    "different" = ifelse(fvalue > fcritical[1] & fvalue < fcritical[2],
                                         negative,
                                         positive),
                    "greater" = ifelse(fvalue < fcritical[2],
                                       negative,
                                       positive)
  )

  ftheo <- switch (alternative,
                    "different" = paste0(fcritical[1], ", ", fcritical[2]),
                    "greater" = paste0(fcritical[2])
  )



  list(hypotheses = c("h0" = h0_text,
                      "h1" = h1_text),
       ratio = c("mean" = ratio,
                      "lwrci" = ratioconfint[1],
                      "uprci" = ratioconfint[2]),
       test = list("dof" = c("numeratore" = dof[[1]],
                             "denominatore" = dof[[2]]),
                "alpha" = alpha,
                "fsper" = unname(fvalue),
                "ftheo" = ftheo,
                "pvalue" = pvalue),
       result = unname(result))

}

#' Plotly boxplots for comparing two groups of values
#'
#' @description The function provides a simple {plotly} boxplot for comparing
#' two groups of values
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' a column named *group* with a two level factor label for the two groups
#' to be compared, a column named *response* with the numeric values for
#' the two groups and a column named *outlier* with a logical vector.
#' @param group a character string for the label of the grouping variable.
#' @param response a character string with the label for the response numeric variable.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {plotly} boxplot for comparing two group of values. Raw data values
#' are overlayed on top of the boxes.
#'
#' @export
#'
#' @importFrom plotly plot_ly add_boxplot add_markers layout config
boxplot_2samples <- function(data,
                             group,
                             response,
                             udm) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
    is.character(udm),
    colnames(data) %in% c("key", "outlier", "response", "group"),
    dim(data)[2] == 4
  )

  quo_group <- enquote(group)
  quo_response <- enquote(response)
  quo_udm <- enquote(udm)

  cols <- ifelse(data$outlier == TRUE,
                          "#999999",
                          "black")

  ylabtitle <- paste0(response,
           ifelse(udm != "", paste0(" (", udm, ")"), ""))


  plotly::plot_ly(source = "boxplot") |>
    plotly::add_boxplot(
      data = data[data$outlier == FALSE, ],
      y = ~ response,
      x = ~ group,
      name = "boxplot",
      type = "box",
      boxmean = TRUE,
      boxpoints = FALSE,
      color = I("#D55E00"),
      showlegend = FALSE,
      key = NULL
    ) |>
    plotly::add_markers(
      data = data,
      y = ~ response,
      x = ~ group,
      name = "valori",
      marker = list(
        color = I(cols),
        colors = I(cols),
        size = 10
      ),
      key = ~ key,
      hoverinfo = "y",
      hovertemplate = paste('%{y:.3s}', udm)
    ) |>
    plotly::layout(
      showlegend = FALSE,
      title = NULL,
      xaxis = list(title = group),
      yaxis = list(title = ylabtitle,
                   hoverformat = ".3s")
    ) |>
    plotly::config(displayModeBar = FALSE,
                   locale = "it")

}

#' GGplot2 boxplots for comparing two groups of values
#'
#' @description The function provides a simple {ggplot2} boxplot for comparing
#' two groups of values
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' a column with a two level factor label for the two groups
#' to be compared, a column with the numeric values for the two groups and a
#' column named *rimosso* with "sì" or "no" values.
#' @param group a character string for the label of the grouping variable.
#' @param response a character string with the label for the response numeric variable.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {ggplot2} boxplot for comparing two group of values. Raw data values
#' are overlayed on top of the boxes.
#'
#' @export
#'
#' @rawNamespace import(ggplot2, except = last_plot)
ggboxplot_2samples <- function(data,
                             group,
                             response,
                             udm) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
    is.character(udm)
  )

  rimosso <- NULL
  cols <- c("s\u00EC" = "#999999", "no" = "black")

  xlabtitle <- group
  ylabtitle <- paste0(response, ifelse(udm != "", paste0(" (", udm, ")"), ""))

  quo_group <- ggplot2::ensym(group)
  quo_response <- ggplot2::ensym(response)


  ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = data[which(data$rimosso == "no"),],
                          ggplot2::aes(x = !!quo_group,
                                       y = !!quo_response),
                          fill = "white",
                          col = "black",
                          outlier.shape = NA) +
    ggplot2::geom_jitter(data = data,
                         ggplot2::aes(x = !!quo_group,
                                      y = !!quo_response,
                                      col = rimosso),
                         width = 0.2) +
    ggplot2::labs(x = xlabtitle,
                  y = ylabtitle) +
    ggplot2::scale_color_manual(values = cols,
                                breaks = c("s\u00EC", "no"),
                                labels = c("rimosso", "non rimosso"),
                                name = ggplot2::element_blank()) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

}

#' Summary arranged on rows for two groups
#'
#' @description The function returns a table with max, mean, median, min, sd and n
#'  values arranged on rows while groups are on columns. Numbers are formatted as
#'  text in order to provide the desired significant figures.
#'
#' @param data the \code{data.frame} or \code{data.table} to be summarised.
#' @param response a string with the name of the variable to summarise.
#' @param group a string with the name of the grouping variable.
#' @param udm a string with the unit of measurement.
#' @param signif a integer with the number of desired significant figures.
#'
#' @return a \code{data.table} with 6 rows and \eqn{n + 1} columns for \eqn{n}
#'   levels of the group variable.
#'
#' @export
#'
#' @import data.table
#' @importFrom stats sd median
rowsummary_2samples <- function(data,
                                response,
                                group,
                                udm = "",
                                signif = 3L) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
    is.character(udm),
    all.equal(signif, as.integer(signif)),
    response %in% colnames(data),
    group %in% colnames(data)
  )

  statistica <- NULL
  mydata <- data.table(data)
  lvl <- levels(as.factor(mydata[[group]]))
  roworder <- c("n", "massimo", "media", "mediana", "minimo", "deviazione standard")
  fm <- as.formula(paste("statistica", '~', group))

  # calculate the summary
  mysummary <- mydata[, lapply(.SD, function(x) {
    c(
      n = .N,
      massimo = max(x, na.rm = TRUE) |> format_sigfig(signif),
      media = mean(x, na.rm = TRUE) |> format_sigfig(signif),
      mediana = stats::median(x, na.rm = TRUE) |> format_sigfig(signif),
      minimo = min(x, na.rm = TRUE) |> format_sigfig(signif),
      `deviazione standard` = stats::sd(x, na.rm = TRUE) |> format_sigfig(signif)
    )
  }),
  by = group,
  .SDcols = response][,
    # table with three columns
    "statistica" := rep(roworder, length(lvl))] |>
    data.table::dcast(eval(fm), value.var = response) |>
    # reordering rows
    (\(x) x[roworder])()

  # adding unit of measurement
    mysummary[, "statistica" := lapply(statistica, (\(x) {
      ifelse(x != "n" & udm != "", paste0(x, " (", udm, ")"), x)
      }))]
}

