testServer(
  mod_save_server,
  # Add here your module params
  args = list(group = reactive("factor"),
              response = reactive("variable"),
              selected_parameter = reactive("myparameter"),
              inputlist = reactive(NA))
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

    ## Testing the save button
    session$setInputs(save = 1)
    expect_true(input$save == 1)

    ## Testing the delete button
    session$setInputs(delete = 1)
    expect_true(input$save == 1)
    expect_true(input$delete == 1)

    ## Testing the reactive list as output
    expect_true(inherits(session$getReturned(), "reactive"))
})

test_that("module ui works", {
  ui <- mod_save_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_save_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

