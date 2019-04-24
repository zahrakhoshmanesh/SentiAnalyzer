context("test-BuildTraining")
# - Function specification:
# - name: 'Training'
# - input should be a dataframe of numeric variables
# - function should error if input is not dataframe or coloums are not numeric
# - function should return a list
test_that("BuildTraining work as expected", {
  
  
  valid_path <- system.file("data", "testing1.csv", package = "SentiAnalyzer")
  c<-read.csv(valid_path)
  #input is dataframe
  expect_s3_class(c, "data.frame")
  #col are numeric or integer
  expect_equal(unique(sapply(c[-ncol(c)], class)),"integer")#
  #output
  test_val <- BuildTraining(x = c)
  #output is a list
  
  expect_equal(class(test_val), "list")
  #output has one col
  expect_equivalent(length(test_val), 5)
  #expect_equal(sapply(-ncol(testing),class), c("integer"|"numeric"))#
})
