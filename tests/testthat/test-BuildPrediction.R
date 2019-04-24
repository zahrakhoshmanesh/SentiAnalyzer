context("test-BuildPrediction")
# - Function specification:
# - name: 'Training'
# - input should be a dataframe of numeric variables
# - function should error if input is not dataframe or coloums are not numeric
# - function should return a list
test_that("BuildPrediction work as expected", {
  
  data(package = "SentiAnalyzer", testing1)
  # c<-read.csv(valid_path)
  #input is dataframe
  expect_s3_class(testing1, "data.frame")
  #col are numeric or integer
  expect_equal(unique(sapply(testing1[-ncol(testing1)], class)),"integer")#
  #output
  test_val <- BuildPrediction(x = testing1)
  #output is a list

  expect_equal(class(test_val), "data.frame")
  #output has one col
  expect_equivalent(dim(test_val), c(4,1))
  #expect_equal(sapply(-ncol(testing),class), c("integer"|"numeric"))#
})
