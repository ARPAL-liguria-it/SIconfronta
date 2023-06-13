testServer(
  mod_compare_server,
  # Add here your module params
  args = list(data = reactive(tomato_yields),
              response = reactive("pounds"),
              group = reactive("fertilizer"),
              analyte = reactive("try"))
  , {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )

    # testing the inputs
    session$setInputs(alternative = "different",
                      significance = 0.95)
    expect_true(input$alternative == "different")
    expect_true(input$significance == 0.95)

    # testing the intermediate dataset
    expect_equal(rownumber(), 11)
    expect_equal(dim(selected_data$data), c(11, 4))
    expect_equal(colnames(selected_data$data), c("key", "outlier", "response", "group"))
    expect_equal(selected_data$data$key, seq(1, 11))
    expect_equal(selected_data$data$outlier, rep(FALSE, 11))
    expect_equal(selected_data$data$response, tomato_yields$pounds)
    expect_equal(selected_data$data$group, tomato_yields$fertilizer)

    # testing data points removal
    ## removal of the 5th data point
    outlierflag <- rep(FALSE, 11)
    keys(5)
    outlierflag5 <- outlierflag
    outlierflag5[5] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag5)
    expect_equal(selected_data$data$key, c(1:4, 6:11))

    ## removal of 5th and 7th points
    keys(c(5, 7))
    outlierflag57 <- outlierflag5
    outlierflag57[7] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag57)
    expect_equal(selected_data$data$key, c(1:4, 6, 8:11))

    ## re-adding the 5th point
    keys(7)
    outlierflag7 <- outlierflag
    outlierflag7[7] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag7)
    expect_equal(selected_data$data$key, c(1:6, 8:11))

    ## re-adding all the points
    keys(NA)
    session$flushReact()

    expect_equal(is_outlier(), rep(FALSE, 11))

    # testing shapiro-wilk test intermediate results
    expect_equal(lvl(), c("a", "b"))
    expect_equal(shapirotest_list()[[1]],
      "<b>Gruppo a:</b> I valori sono compatibili con una distribuzione normale (W = 0.990, <i>p</i>-value = 0.9803)</br>")
    expect_equal(shapirotest_list()[[2]],
      "<b>Gruppo b:</b> I valori sono compatibili con una distribuzione normale (W = 0.926, <i>p</i>-value = 0.5512)</br>")

    # testing grubbs test intermediate results
    expect_equal(gesdtest_list()[[1]],
      "<b>Gruppo a:</b></br> nessun valore anomalo a un livello di confidenza del 95% </br> nessun valore anomalo a un livello di confidenza del 99% </br></br>")
    expect_equal(gesdtest_list()[[2]],
      "<b>Gruppo b:</b></br> nessun valore anomalo a un livello di confidenza del 95% </br> nessun valore anomalo a un livello di confidenza del 99% </br></br>")

    # Testing the outputs
    ## Testing the boxplot output
    expect_true(inherits(output$boxplot, "json"))
    ## Testing the summary output
    expect_true(inherits(output$summary, "json"))
    ## Testing the Shapiro-Wilk test output
    expect_true(inherits(output$shapirotest, "character"))
    ## Testing the GESD test output
    expect_true(inherits(output$gesdtest, "character"))
    ## Testing the t-test output
    expect_true(inherits(output$ttest, "character"))
    ## Testing the F-test output
    expect_true(inherits(output$ftest, "character"))
    ## Testing the reactive list as output
    expect_true(inherits(session$getReturned(), "list"))
    expect_equal(names(session$getReturned()),
                 c("data", "udm",
                   "summarytbl", "shapirotest", "gesdtest",
                   "ttest", "ftest"))

    # - If ever your input updates a reactiveValues)
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
})

test_that("module compareinput ui works", {
  ui <- mod_compareinput_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_compareinput_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

test_that("module comparesummary ui works", {
  ui <- mod_comparesummary_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_comparesummary_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

test_that("module compareshapirogrubbstest ui works", {
  ui <- mod_compareshapirogesdtest_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_compareshapirogesdtest_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

test_that("module comparettest ui works", {
  ui <- mod_comparettest_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_comparettest_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

test_that("module compareftest ui works", {
  ui <- mod_compareftest_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_compareftest_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

