context("test-BuildTraining")
# - Function specification:
# - name: 'Training'
# - input should be a dataframe of numeric variables
# - function should error if input is not dataframe or coloums are not numeric
# - function should return a list
test_that("BuildTraining work as expected", {
  
  
  data(package = "SentiAnalyzer", testing1)
  #input is dataframe
  expect_s3_class(testing1, "data.frame")
  #col are numeric or integer
  expect_equal(unique(sapply(testing1[-ncol(testing1)], class)),"integer")#
  #output
  test_val <- BuildTraining(x = testing1)
  #output is a list
  
  expect_equal(class(test_val), "list")
  #output has one col
  expect_equivalent(length(test_val), 6)
  #expect_equal(sapply(-ncol(testing),class), c("integer"|"numeric"))#
})
