r <- reactiveValues(aim01 = reactiveValues(),
                    loadfile02 = reactiveValues(),
                    compare03x = reactiveValues())

testServer(
  mod_compare03_server,
  # Add here your module params
  args = list(r), {

    r$aim01$aim <- "2samples"
    r$loadfile02$data <- tomato_yields
    r$loadfile02$parvar <- "parameter"
    r$loadfile02$responsevar <- "pounds"
    r$loadfile02$groupvar <- "fertilizer"
    r$loadfile02$parlist <- "yield"

    session$setInputs(parameter = "yield")
    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    # testing the inputs
    expect_true(r$compare03$myparameter == "yield")

    r$compare03x$alternative <- "different"
    r$compare03x$significance <- "0.95"
    r$compare03x$udm <- "ug/L"
    session$flushReact()

    session$setInputs(save = 1)

    expect_true(input$save == 1)


    # testing the outputs
    expect_equal(names(r$compare03$yield),
                 c("saved", "boxplot", "significance", "alternative",
                   "outliers", "normality", "summary", "data", "udm",
                   "parameter"))


    # - If ever your input updates a reactiveValues)
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
})

test_that("module compareinput ui works", {
  ui <- mod_compare03_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_compare03_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

