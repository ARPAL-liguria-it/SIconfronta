testServer(
  mod_makereport_server,
  # Add here your module params
  args = list(inputreport = "My settings")
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

    # Testing the setting of inputs
    session$setInputs(title = "My nice title")
    expect_true(input$title == "My nice title")

    session$setInputs(description = "My very long description")
    expect_true(input$description == "My very long description")

    session$setInputs(content = c("shapirotest", "ftest"))
    expect_equal(input$content, c("shapirotest", "ftest"))

    expect_true(inherits(session$getReturned(), "shiny.render.function"))
})

test_that("module ui works", {
  ui <- mod_makereport_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_makereport_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

