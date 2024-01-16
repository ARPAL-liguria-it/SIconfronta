test_that("Supported CSV files are correctly imported for two samples option", {
  # expetected result for 2samples option
  expected_classes <- c(analita = "factor",
                        gruppo = "factor",
                        valore = "numeric")

  # semicolon as field separator and comma as decimal separator
  sep_comma_decimal_dot <- system.file("extdata",
                                             "raw_2samples_commafield_dotdecimal.csv",
                                             package = "SIconfronta") |>
    csvimport() |>
    sapply("class")

  # semicolon as field separator and dot as decimal separator
  sep_semicolon_decimal_dot <- system.file("extdata",
                                           "raw_2samples.csv",
                                           package = "SIconfronta") |>
    csvimport() |>
    sapply("class")

  # semicolon as field separator and dot as decimal separator
  sep_semicolon_decimal_comma <- system.file("extdata",
                                             "raw_2samples_semicolonfield_commadecimal.csv",
                                             package = "SIconfronta") |>
    csvimport() |>
    sapply("class")


  expect_identical(sep_semicolon_decimal_comma, expected_classes)
  expect_identical(sep_semicolon_decimal_dot, expected_classes)
  expect_identical(sep_comma_decimal_dot, expected_classes)
})

test_that("Supported CSV files are correctly imported for two values option", {
  # expetected result for 2samples option
  expected_classes <- c(analita = "factor",
                        gruppo = "factor",
                        valore = "numeric",
                        unc = "numeric")

  # semicolon as field separator and comma as decimal separator
  sep_comma_decimal_dot <- system.file("extdata",
                                       "raw_2values_unc_commafield_dotdecimal.csv",
                                       package = "SIconfronta") |>
    csvimport() |>
    sapply("class")

  # semicolon as field separator and dot as decimal separator
  sep_semicolon_decimal_dot <- system.file("extdata",
                                           "raw_2values_unc.csv",
                                           package = "SIconfronta") |>
    csvimport() |>
    sapply("class")

  # semicolon as field separator and dot as decimal separator
  sep_semicolon_decimal_comma <- system.file("extdata",
                                             "raw_2values_unc_semicolonfield_commadecimal.csv",
                                             package = "SIconfronta") |>
    csvimport() |>
    sapply("class")


  expect_identical(sep_semicolon_decimal_comma, expected_classes)
  expect_identical(sep_semicolon_decimal_dot, expected_classes)
  expect_identical(sep_comma_decimal_dot, expected_classes)
})
