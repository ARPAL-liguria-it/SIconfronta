#' Tomato yields for two fertilizer mixtures
#'
#' A dataset containing the results expressed in pounds for tomato yields obtained
#'  by a randomized experiment with two fertilizer mixtures. The variable are as
#'  follows:
#'
#' @format a dataframe with 6 rows and 3 columns:
#' \describe{
#'    \item{parameter}{the parameter observed in the experiment, the yield}
#'    \item{fertilizer}{the variable for the useed fertilizer mixture: \code{a} or \code{b}}
#'    \item{pounds}{the measured yields, in pounds}
#' }
#'
#' @name tomato_yields
#' @docType data
#' @author George E. P. Box
#' @author J. Stuart Hunter
#' @author William G. Hunter
#' @source Statistics for Experimenters, Design, Innovation and Discovery.
#'  Wiley, Second Edition, 2005. ISBN: 978-0-471-71813-0.
#'  Section 3.1, pag. 78, table 3.3
#' @keywords data
"tomato_yields"

#' F.Test data
#'
#' A dataset with 10 values divided in two groups 5 values. The dataset is
#' provided for testing the \code{fct_test} results.
#'
#' @format a dataframe with 5 rows and 2 columns:
#' \describe{
#'    \item{group}{the grouping variable, either \code{a} or \code{b}}
#'    \item{value}{the measured value for the variable of interest}
#' }
#'
#' @name ftest_reference
#' @docType data
#' @source \url{https://support.microsoft.com/en-us/office/f-test-function-100a59e7-4108-46f8-8443-78ffacb6c0a7}.
#' @keywords data
"ftest_reference"


#' Shapiro-Wilk test data
#'
#' A dataset with 11 values of men weights expressed in pounds.
#' The dataset is provided for testing the results of \code{fct_shapiro}.
#'
#' @format a vector with 11 numerical elements.
#'
#' @name shapiro_reference
#' @docType data
#' @author S. S. Shapiro
#' @author M. B. Wilk
#' @source An analysis of variance test for normality (complete samples),
#'  Biometrika (1965), 52, 3 and 2, p. 591.
#'  Section 4 - Examples, pag. 606, Example 1.
#'  \url{http://links.jstor.org/sici?sici=0006-3444\%28196512\%2952\%3A3\%2F4\%3C591\%3AAAOVTF\%3E2.0.CO\%3B2-B}.
#' @keywords data
"shapirotest_reference"
