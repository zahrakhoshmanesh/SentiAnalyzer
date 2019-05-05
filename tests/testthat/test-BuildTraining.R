context("test-BuildTraining")

test_that("BuildTraining work as expected", {
  
  
  data(package = "SentiAnalyzer", testing1)
  #input is dataframe
  expect_s3_class(testing1, "data.frame")
  #col are numeric or integer
  expect_equal(unique(sapply(testing1[-ncol(testing1)], class)),"integer")
  #output
  output <- BuildTraining(x = testing1)
  expect_type(output, "list")
  #show_failure(expect_s3_class(output, "list"))
  
})
