context("test-BuildTraining")

test_that("BuildTraining work as expected", {
  #data(package = "SentiAnalyzer", testing1)
  
  simple_df <- data.frame(
    X = 1:10, 
    yay = c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
    boo = c(0, 0, 0, 1, 0, 0, 1, 1, 1, 1),
    meh = c(0, 0, 1, 1, 1, 1, 1, 1, 0, 0),
    sad = c(0, 0, 0, 0, 1, 0, 1, 0, 1, 1),
    awesome = c(1, 0, 1, 1, 1, 0, 0, 0, 0, 0),
    turtle = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 1))
  
  # col are numeric or integer
  sapply(simple_df[-ncol(simple_df)], function(x) expect_true(class(x) %in% c("integer", "numeric")))
  
  # output
  expect_warning(df <- BuildTraining(x = simple_df))
  expect_type(df, "list")
  # show_failure(expect_s3_class(output, "list"))
})