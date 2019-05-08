context("test-BuildPrediction")

test_that("BuildPrediction work as expected", {
  data(package = "SentiAnalyzer", trained_models)
  
  # input is dataframe
  # expect_s3_class(testing1, "data.frame") # This doesn't do anything - it's not testing the function. Try passing in something else that isn't a data frame and see if it gives you a useful error. That's a decent test.
  
  # testing output
  outi <- BuildPrediction(x = trained_models)
  # output is a list
  expect_s3_class(outi, "data.frame")
  # output has one col
  expect_equivalent(dim(outi), c(5, 1))
  # col are numeric or integer
 # expect_equal(unique(sapply(testing1[-ncol(testing1)], class)), "integer") #
})