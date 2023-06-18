testServer(
  mod_aim01_server,
  # Add here your module params
  args = list(r = reactiveValues())
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
    session$setInputs(aim = "2samples",
                      aimbtn = TRUE)
    expect_true(r$aim == "2samples")
    session$setInputs(aim = "2samples_par",
                      aimbtn = TRUE)
    expect_true(r$aim == "2samples_par")
    session$setInputs(aim = "1sample_mu",
                      aimbtn = TRUE)
    expect_true(r$aim == "1sample_mu")
    session$setInputs(aim = "1sample_sigma",
                      aimbtn = TRUE)
    expect_true(r$aim == "1sample_sigma")
    session$setInputs(aim = "2values_unc",
                      aimbtn = TRUE)
    expect_true(r$aim == "2values_unc")
})

test_that("module aim01 ui works", {
  ui <- mod_aim01_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_aim01_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

