context("test-comparison")
# - Function specification:
# - name: 'Training'
# - input should be a dataframe of numeric variables
# - function should error if input is not dataframe or coloums are not numeric
# - function should return a list
test_that("comparison work as expected", {
  data(package = "SentiAnalyzer", trained_models)
  preds <- BuildPrediction(trained_models)
  
  # input is dataframe
  #expect_s3_class(testing1, "data.frame")
  
  output <- comparison(x = preds)
  # expect_type(output, "list")
  # show_failure(expect_s3_class(output, "list"))
  
  expect_equal(class(output), "data.frame")
  # output has one col
  expect_equivalent(dim(output), c(5, 5))
  # expect_equal(sapply(-ncol(testing),class), c("integer"|"numeric"))#
})
