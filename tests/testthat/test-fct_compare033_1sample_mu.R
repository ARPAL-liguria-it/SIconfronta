test_that("Errors are correctly handled for t-test on 1 group of data
          with vs a reference value", {
  faildf <- data.frame(a = "a",
                       b = "b",
                       c = 3)
  expect_error(fct_ttest_1sample_mu(faildf, "c", "d"), "")
  expect_error(fct_ttest_1sample_mu(faildf, "c", "b", alternative = "less"), "")
})

tomato_yields_a <- tomato_yields[fertilizer == "a"]
first_group <- "a"
first_n <- tomato_yields[fertilizer == "a", .N]
first_mean <- tomato_yields[fertilizer == "a", mean(pounds)]
first_sd <- tomato_yields[fertilizer == "a", sd(pounds)]
second_group <- "b"
second_n <- tomato_yields[fertilizer == "b", .N]
second_mean <- tomato_yields[fertilizer == "b", mean(pounds)]
second_sd <- tomato_yields[fertilizer == "b", sd(pounds)]

ttest_result1 <- fct_ttest_1sample_mu(tomato_yields_a, "pounds", "fertilizer", "law", 30)


test_that("Calculations are correct for t-test on one sample and alternative = different", {
  expect_equal(ttest_result1$hypotheses[[1]], "valore di riferimento law = media di a")
  expect_equal(ttest_result1$hypotheses[[2]], "valore di riferimento law ≠ media di a")
  expect_equal(ttest_result1$mean[[1]], "20.84")
  expect_equal(ttest_result1$mean[[2]], "11.84")
  expect_equal(ttest_result1$mean[[3]], "29.84")
  expect_equal(ttest_result1$test[[1]], 4)
  expect_equal(ttest_result1$test[[2]], 0.975)
  expect_equal(ttest_result1$test[[3]], 2.8269)
  expect_equal(ttest_result1$test[[4]], 2.7760)
  expect_equal(ttest_result1$test[[5]], 0.0475)
})

ttest_result2 <- fct_ttest_1sample_mu(tomato_yields_a, "pounds", "fertilizer", "law", 30,
                                      alternative = "greater")

test_that("Calculations are correct for t-test on one sample and alternative = greater", {
  expect_equal(ttest_result2$hypotheses[[1]], "valore di riferimento law ≤ media di a")
  expect_equal(ttest_result2$hypotheses[[2]], "valore di riferimento law > media di a")
  expect_equal(ttest_result2$mean[[1]], "20.84")
  expect_equal(ttest_result2$mean[[2]], "-Inf")
  expect_equal(ttest_result2$mean[[3]], "27.75")
  expect_equal(ttest_result2$test[[1]], 4)
  expect_equal(ttest_result2$test[[2]], 0.9500)
  expect_equal(ttest_result2$test[[3]], 2.8269)
  expect_equal(ttest_result2$test[[4]], 2.1320)
  expect_equal(ttest_result2$test[[5]], 0.0237)
})

ttest_result3 <- fct_ttest_1sample_mu(tomato_yields_a, "pounds", "fertilizer", "law", 30,
                                      significance = 0.99)

test_that("Calculations are correct for t-test on one sample and confidence = 0.99", {
  expect_equal(ttest_result3$hypotheses[[1]], "valore di riferimento law = media di a")
  expect_equal(ttest_result3$hypotheses[[2]], "valore di riferimento law ≠ media di a")
  expect_equal(ttest_result3$mean[[1]], "20.84")
  expect_equal(ttest_result3$mean[[2]], "5.921")
  expect_equal(ttest_result3$mean[[3]], "35.76")
  expect_equal(ttest_result3$test[[1]], 4)
  expect_equal(ttest_result3$test[[2]], 0.9950)
  expect_equal(ttest_result3$test[[3]], 2.8269)
  expect_equal(ttest_result3$test[[4]], 4.6040)
  expect_equal(ttest_result3$test[[5]], 0.0475)
})


test_that("ggboxplot_1sample_mu works well", {
  testdata <- tomato_yields[fertilizer == "a"]
  testdata$rimosso <- c(rep("no", 3), "sì", "no")

  expect_true(ggboxplot_1sample_mu(testdata, "fertilizer", "pounds", "law", 30, "ug/L") |>
                ggplot2::is.ggplot())
  expect_equal(ggboxplot_1sample_mu(testdata, "fertilizer", "pounds", "law", 30, "ug/L")$labels$x, "fertilizer")
  expect_equal(ggboxplot_1sample_mu(testdata, "fertilizer", "pounds", "law", 30, "ug/L")$labels$y, "pounds (ug/L)")
  print(ggboxplot_1sample_mu(testdata, "fertilizer", "pounds", "law", 30, "ug/L"))
})

test_that("rowsummary_1sample_mu works well", {
  testdata <- tomato_yields[fertilizer == "a"]
  testdata$rimosso <- c(rep("no", 3), "sì", "no")

  res <- rowsummary_1sample_mu(testdata, "pounds", "fertilizer",
                                 "law", 30, udm = "kg")

  expect_equal(res$statistica |> unlist(),
               c("n", "massimo (kg)", "media (kg)", "mediana (kg)", "minimo (kg)",
                 "deviazione standard (kg)"))
  expect_equal(colnames(res), c("statistica", "a", "law"))
  expect_equal(res[statistica == "media (kg)", a],
               sprintf("%.3g", tomato_yields[fertilizer == "a", mean(pounds)]))
  expect_equal(res[statistica == "media (kg)", law], "30")
  expect_equal(res[statistica == "massimo (kg)", law],
               "-")
  expect_equal(res[statistica == "mediana (kg)", law],
               "-")
  expect_equal(res[statistica == "n", a],
               tomato_yields[fertilizer == "a", .N] %>% as.character)
  expect_equal(res[statistica == "deviazione standard (kg)", law],
               "-")
})
