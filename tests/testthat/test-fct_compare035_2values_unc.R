test_that("Errors are correctly handled for En-test on two values with
          extended uncertainty", {
  faildf <- data.frame(a = "a",
                       b = "b",
                       c = 3)
  expect_error(fct_entest_2values_unc(faildf, "c", "d"), "")
  expect_error(fct_entest_2values_unc(faildf, "c", "b", "c"), "")
})


noeffectdata <- data.frame(mygroup = letters[1:2],
                           myvalue = c(2.15, 4.84),
                           myuncertainty = c(1.02, 2.50))

noeffect_result <- fct_entest_2values_unc(noeffectdata, "myvalue", "myuncertainty", "mygroup")

# comparison vs EnvStats::varTest(tomato_yields_a$pounds, sigma.squared = ref_sd^2)
test_that("Calculations are correct for En-test when no differences are expected", {
  expect_equal(noeffect_result$hypotheses[[1]], "b = a")
  expect_equal(noeffect_result$hypotheses[[2]], "b ≠ a")
  expect_equal(noeffect_result$difference[[1]], "2.690") # 2.69
  expect_equal(noeffect_result$difference[[2]], "-0.01007") # -0.01007407
  expect_equal(noeffect_result$difference[[3]], "5.390") # 5.390074
  expect_equal(noeffect_result$test[[1]], "0.9963") # 0.996269
  expect_equal(noeffect_result$test[[2]], "1.000") # 1
})


yeseffectdata <- data.frame(mygroup = letters[1:2],
                            myvalue = c(4.84, 2.15),
                            myuncertainty = c(2.30, 1.02))

yeseffect_result <- fct_entest_2values_unc(yeseffectdata, "myvalue", "myuncertainty", "mygroup")

test_that("Calculations are correct for En-test when a difference is expected", {
  expect_equal(yeseffect_result$hypotheses[[1]], "a = b")
  expect_equal(yeseffect_result$hypotheses[[2]], "a ≠ b")
  expect_equal(yeseffect_result$difference[[1]], "2.690") # 2.69
  expect_equal(yeseffect_result$difference[[2]], "0.1740") # 0.1739714
  expect_equal(yeseffect_result$difference[[3]], "5.206") # 5.206029
  expect_equal(yeseffect_result$test[[1]], "1.069") #  1.069145
  expect_equal(yeseffect_result$test[[2]], "1.000") # 1
})


test_that("ggboxplot_2values_unc", {
  expect_true(ggboxplot_2values_unc(yeseffectdata, "mygroup", "myvalue", "myuncertainty", "ug/L") |>
                ggplot2::is.ggplot())
  expect_equal(ggboxplot_2values_unc(yeseffectdata, "mygroup", "myvalue", "myuncertainty", "ug/L")$labels$x, "mygroup")
  expect_equal(ggboxplot_2values_unc(yeseffectdata, "mygroup", "myvalue", "myuncertainty", "ug/L")$labels$y, "myvalue (ug/L)")
})

test_that("rowsummary_2values_unc works well", {
  mytbl <- rowsummary_2values_unc(yeseffectdata, "myvalue", "myuncertainty", "mygroup", "kg")

  expect_equal(mytbl$statistica |> unlist(), c("valore (kg)", "incertezza estesa (kg)"))
  expect_equal(colnames(mytbl), c("statistica", "a", "b"))
  expect_equal(mytbl[which(mytbl$statistica == "valore (kg)"), "a"], "4.840")
  expect_equal(mytbl[which(mytbl$statistica == "valore (kg)"), "b"], "2.150")
  expect_equal(mytbl[which(mytbl$statistica == "incertezza estesa (kg)"), "a"], "2.300")
  expect_equal(mytbl[which(mytbl$statistica == "incertezza estesa (kg)"), "b"], "1.020")
})
