test_that("signiftodigits works well", {
  expect_equal(sprintf("%.*f", signiftodigits(5, 4L), 5), "5.000")
  expect_equal(sprintf("%.*f", signiftodigits(0.5, 4L), 0.5), "0.5000")
  expect_equal(sprintf("%.*f", signiftodigits(0.501, 4L), 0.501), "0.5010")
  expect_equal(sprintf("%.*f", signiftodigits(0.00501, 4L), 0.00501), "0.005010")
  expect_equal(sprintf("%.*f", signiftodigits(0.00501, 2L), 0.00501), "0.0050")
  expect_equal(sprintf("%.*f", signiftodigits(Inf, 2L), Inf), "Inf")
})

test_that("rowsummary works well", {
  expect_equal(rowsummary(tomato_yields, "pounds", "fertilizer")$statistica,
               c("n", "max", "mean", "median", "min", "sd"))
  expect_equal(colnames(rowsummary(tomato_yields, "pounds", "fertilizer")),
               c("statistica", "a", "b"))
  expect_equal(rowsummary(tomato_yields, "pounds", "fertilizer")[statistica == "mean", a],
               sprintf("%.3g", tomato_yields[fertilizer == "a", mean(pounds)]))
  expect_equal(rowsummary(tomato_yields, "pounds", "fertilizer")[statistica == "max", b],
               sprintf("%.3g", tomato_yields[fertilizer == "b", max(pounds)]))
  expect_equal(rowsummary(tomato_yields, "pounds", "fertilizer")[statistica == "median", b],
               "24.0")
  expect_equal(rowsummary(tomato_yields, "pounds", "fertilizer")[statistica == "n", a],
               tomato_yields[fertilizer == "a", .N] %>% as.character)
  expect_equal(rowsummary(tomato_yields, "pounds", "fertilizer")[statistica == "sd", b],
               "5.43")
})

test_that("htmltormarkdown works well", {
  expect_equal(htmltormarkdown("<h4> Test per la verifica della normalità (Shapiro-Wilk) </h4></br>"),
               "\n### Test per la verifica della normalità (Shapiro-Wilk)  \n  \n ")
})
