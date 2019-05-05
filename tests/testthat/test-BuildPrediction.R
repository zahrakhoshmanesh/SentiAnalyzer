context("test-BuildPrediction")

test_that("BuildPrediction work as expected", {
  
  data(package = "SentiAnalyzer", testing1)
 
  
  #input is dataframe
  expect_s3_class(testing1, "data.frame")

  #testing output
  outi <- BuildPrediction(x = testing1)
  #output is a list
  expect_s3_class(outi, "data.frame")
  #output has one col
  expect_equivalent(dim(outi), c(5,1))
  #col are numeric or integer
  expect_equal(unique(sapply(testing1[-ncol(testing1)], class)),"integer")#
  if (checkmate::testList( x= testing1)) {
    # Add more checks here to make sure the list is properly formatted!!
    testthat::expect_equivalent(length(x), 5)}

})
