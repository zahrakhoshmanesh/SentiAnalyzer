context("test-BuildPrediction")
# - Function specification:
# - name: 'Training'
# - input should be a dataframe of numeric variables
# - function should error if input is not dataframe or coloums are not numeric
# - function should return a list
test_that("BuildPrediction work as expected", {
  
  valid_path <- system.file("data", "testing.csv", package = "SentiAnalyzer")
  c<-read.csv(valid_path)
  #input is dataframe
  expect_s3_class(c, "data.frame")
  #col are numeric or integer
  expect_equal(unique(sapply(c[-ncol(c)], class)),"integer")#
  #output
  test_val <- BuildPrediction(x = c)
  #output is a list

  expect_equal(class(test_val), "data.frame")
  #output has one col
  expect_equivalent(dim(test_val), c(4,1))
  #expect_equal(sapply(-ncol(testing),class), c("integer"|"numeric"))#
})
