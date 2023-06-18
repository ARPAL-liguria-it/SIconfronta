testServer(mod_loadfile_server,
           # Module params
           args = list(stringsAsFactors = TRUE), {
             ns <- session$ns
             expect_true(inherits(ns, "function"))
             expect_true(grepl(id, ns("")))
             expect_true(grepl("test", ns("test")))
             # Check the input file name and path
             session$setInputs(file = list(
               datapath = system.file("testdata", "tomato_yields.csv", package = "comparat"),
               name = "tomato_yields.csv"
             ))
             # number of numeric columns
             numcol <- sapply(dataframe(), is.numeric) %>% sum
             # number of factor columns
             fctcol <- sapply(dataframe(), is.factor) %>% sum
             # check the filename
             expect_equal(input$file$name, "tomato_yields.csv")
             # check the datapath
             expect_equal(
               input$file$datapath,
               system.file("testdata", "tomato_yields.csv", package = "comparat")
             )
             # check the column names
             expect_equal(colnames(dataframe()),
                          c("parameter", "fertilizer", "pounds"))
             # chech the dimensions of the dataset
             expect_equal(dim(dataframe()), c(11, 3))
             # check the number of numeric columns
             expect_equal(numcol, 1)
             # check the number of factor columns
             expect_equal(fctcol, 2)
             # check that the first column is factor
             expect_equal(class(dataframe()[["parameter"]]), "factor")
             # check that the second column is numeric
             expect_equal(class(dataframe()[["fertilizer"]]), "factor")
             # check that the third column is numeric
             expect_equal(class(dataframe()[["pounds"]]), "numeric")
             # check the number of values for the two groups
             expect_equal(dataframe()[, .N, by = "fertilizer"][, N], c(5, 6))
           })

test_that("module ui works", {
  ui <- mod_loadfile_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_loadfile_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})

