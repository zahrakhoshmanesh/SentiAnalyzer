context("test-BuildPrediction")

test_that("BuildPrediction work as expected", {
  
  data(package = "SentiAnalyzer", testing1)
  #csv_data <- read.csv(system.file(package = "SentiAnalyzer", "extdata/testing.csv"))
  
  #input is dataframe
  expect_s3_class(testing1, "data.frame")

  #testing output
  #prediction-output <- BuildPrediction(x = csv_data)
  #output is a list
  #expect_s3_class(prediction-output, "data.frame")
 # expect_type(prediction-output, "data.frame")
  #show_failure(expect_s3_class(output, "integer"))

})
