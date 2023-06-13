#' Displays the results of a Shapiro-Wilk normality test
#'
#' @description The function displays the results of a Shapiro-Wilk test for
#'  normality.
#'  The returned text is suitable for the {comparat} {shiny} app.
#'
#' @param values a \code{vector} with the values relevant for testing.
#'
#' @details the Shapiro-Wilk test is calculated using the base-R function
#'  \code{shapiro.test}.
#' @return A list with the following items:
#'  \describe{
#'    \item{W}{a numeric value with the test statistic.}
#'    \item{pvalue}{a numeric value with the p-value of the test.}
#'    \item{result}{A string with the result of the test.}
#'  }
#'
#' @export

fct_shapiro <- function(values) {

  stopifnot(
    is.vector(values),
    is.numeric(values)
  )

  shapiro_output <- shapiro.test(values)
  result <- ifelse(shapiro_output$p.value <= 0.05,
                   "I valori non sono compatibili con una distribuzione normale",
                   "I valori sono compatibili con una distribuzione normale")


  list(W = shapiro_output$statistic[[1]],
       pvalue = shapiro_output$p.value[[1]],
       result = result)

}

#' Displays the results of generelised extreme strudentized deviate (GESD) test
#'
#' @description The function displays the results of generelised
#'  extreme strudentized deviate (GESD) test for outlier detection.
#'  The returned text is suitable for the {comparat} {shiny} app.
#'
#' @param values a \code{vector} with the values relevant for testing.
#' @param significance a number, typically either 0.90, 0.95 (default) or 0.99
#'   indicating the confidence level for the test.
#' @param m maximum number of possible outliers to be tested.
#'   Default is one third of the number of values.
#'
#' @details the GESD test is performed according to secton 4.3.2 of
#'  UNI ISO 16269-4:2019 - Statistical interpretation of data - Part 4: Detection
#'  and treatment of outliers. The software implementation in performed in
#'  accordance to Annex A of the same document.
#' @return A dataframe  with the following columns:
#' \describe{
#'    \item{I}{Numeric values inspected by the test.}
#'    \item{R}{Numeric values for the extreme studentized deviate.}
#'    \item{lambda}{numberic values with the critical values of the test statistics.}
#'    \item{outliers}{a logical vector with the result of the test.}
#'  }
#'
#' @source UNI ISO 16269-4:2019 - Statistical interpretation of data -
#'  Part 4: Detection and treatment of outliers. Section 4.3.2 and Annex A.
#'  \url{https://store.uni.com/uni-iso-16269-4-2019}
#' @export

fct_gesd <- function(values,
                     significance = 0.95,
                     m = round(length(values)/3, 0)) {

  stopifnot(
    is.vector(values),
    length(values) >= 5,
    is.numeric(significance),
    significance >= 0.90 & significance < 1,
    is.numeric(m),
    m <= length(values)
  )

  # function for calculating the critical lambda value
  lamba_l <- function(n_values,
                      l_removed,
                      signif) {

    alfa <- 1 - signif
    n_l <- n_values - l_removed
    p <- (1 - alfa/2)^(1/(n_l))
    tp <- qt(p, n_l - 2)

    ((n_l - 1) * tp) / sqrt((n_l - 2 + tp^2) * (n_l))

  }

  n <- length(values)
  l <- 0
  df <- data.frame(I = values)
  df_result <- data.frame()

  while (l <= m) {
    # mean and std.deviation
    x_mean <- mean(df$I)
    x_sd <- sd(df$I)

    # deviates from the mean
    df$deviate <- abs(df$I - x_mean)

    # maximum studentized deviate
    df$R <- max(df$deviate)/x_sd

    # attach the maximum stuntized deviate to the final results dataset
    df_result <- rbind(df_result, df[which.max(df$deviate),])

    # remove the value with the maximum deviate from the dataset
    df <- df[-which.max(df$deviate),]

    l <- l + 1
  }

 df_result$l <- 0:m
 df_result$lambda <- lamba_l(n, df_result$l, signif = significance)
 df_result$outlier <- ifelse(df_result$R > df_result$lambda, TRUE, FALSE)
 df_result <- df_result[, c("I", "R", "lambda", "outlier")]
 df_result

}

#' Displays the results of a \eqn{t}-test
#'
#' @description The function displays the results of a \eqn{t}-test.
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
#' @details \eqn{t}-test is calculated using the base-R function \code{t.test},
#'  with the group with the numerically larger mean as first argument.
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

fct_ttest <- function(data,
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
                                    sd = sd(x, na.rm = TRUE))
  # get the summary
  mysummary <- do.call(data.frame, aggregate(myformula, data, summary_function))
  colnames(mysummary) <- c(group, "n", "mean", "sd")
  # get the group with higher mean
  max_mean <- mysummary[[group]][which.max(mysummary$mean)]
  min_mean <- mysummary[[group]][which.min(mysummary$mean)]
  higher_mean <- data[which(data[[group]] == max_mean),][[response]]
  lower_mean <- data[which(data[[group]] == min_mean),][[response]]
  # # t-test results
  ttest <- t.test(x = higher_mean, y = lower_mean,
                  alternative = h1, conf.level = significance)
  difference <- (mean(higher_mean) - mean(lower_mean)) %>% sprintf("%.*f", signiftodigits(., 4L), .)
  diffconfint <- c(NA, NA)
  diffconfint[1] <- ttest$conf.int[1] %>% sprintf("%.*f", signiftodigits(., 4L), .)
  diffconfint[2] <- ttest$conf.int[2] %>% sprintf("%.*f", signiftodigits(., 4L), .)
  tvalue <- ttest$statistic %>% round(4)
  dof <- ttest$parameter %>% round(4)
  tcritical <- qt(alpha, dof) %>% round(3)
  pvalue <- ttest$p.value %>% round(4)

  # Being clear with some text
  h0_text <- switch (alternative,
    "different" = sprintf("media di %s = media di %s", max_mean, min_mean),
    "greater" = sprintf("media di %s \u2264 media di %s", max_mean, min_mean),
  )

  h1_text <- switch (alternative,
    "different" = sprintf("media di %s \u2260 media di %s", max_mean, min_mean),
    "greater" = sprintf("media di %s > media di %s", max_mean, min_mean)
  )

  positive <- switch (alternative,
                      "different" = sprintf("la media di %s e la media di %s sono statisticamente differenti", max_mean, min_mean),
                      "greater" = sprintf("la media di %s \u00E8 statisticamente maggiore della media di %s", max_mean, min_mean)
  )

  negative <- switch (alternative,
                      "different" = sprintf("la media di %s e la media di %s non sono statisticamente differenti", max_mean, min_mean),
                      "greater" = sprintf("la media di %s non \u00E8 statisticamente maggiore della media di %s", max_mean, min_mean)
  )

  result <- ifelse(tvalue < tcritical, negative, positive)

  list(hypotheses = c("h0" = h0_text,
                      "h1" = h1_text),
       difference = c("mean" = difference,
                      "lwrci" = diffconfint[1],
                      "uprci" = diffconfint[2]),
       test = c("dof" = unname(dof),
                "alpha" = alpha,
                "tsper" = unname(tvalue),
                "ttheo" = tcritical,
                "pvalue" = pvalue),
       result = unname(result))

}

#' Displays the results of a \eqn{F}-test
#'
#' @description The function displays the results of a \eqn{F}-test.
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

fct_ftest <- function(data,
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
                                    sd = sd(x, na.rm = TRUE))
  # get the summary
  mysummary <- do.call(data.frame, aggregate(myformula, data, summary_function))
  colnames(mysummary) <- c(group, "n", "mean", "sd")
  # get the group with higher sd
  max_sd <- mysummary[[group]][which.max(mysummary$sd)]
  min_sd <- mysummary[[group]][which.min(mysummary$sd)]
  higher_sd <- data[which(data[[group]] == max_sd),][[response]]
  lower_sd <- data[which(data[[group]] == min_sd),][[response]]
  # # F-test results
  ftest <- var.test(x = higher_sd, y = lower_sd,
                  alternative = h1, conf.level = significance)
  ratio <- (sd(higher_sd)^2 / sd(lower_sd)^2) %>% sprintf("%.*f", signiftodigits(., 4L), .)
  ratioconfint <- c(NA, NA)
  ratioconfint[1] <- ftest$conf.int[1] %>% sprintf("%.*f", signiftodigits(., 4L), .)
  ratioconfint[2] <- ftest$conf.int[2] %>% sprintf("%.*f", signiftodigits(., 4L), .)
  fvalue <- ftest$statistic %>% round(4)
  dof <- ftest$parameter %>% unname # numerator and denominator
  fcritical <- c(qf(1-alpha, dof[[1]], dof[[2]]),
                 qf(alpha, dof[[1]], dof[[2]])) %>% round(4)
  pvalue <- ftest$p.value %>% round(4)

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
