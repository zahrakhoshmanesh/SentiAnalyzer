context("test-CleanText")
test_that("test for function CleanText :Clean dataset and build term matrix for  Machine Learning algorithm", {
  expect_error(CleanText())
  data(package = "SentiAnalyzer", original_dataset)
  #expect_error(CleanText(original_dataset,'' ,'' ))
  expect_s3_class(original_dataset, "data.frame")
  #output
  cleanText_output1 <- CleanText(original_dataset,dtm_method=1,reductionrate=0.99)
  #output is a data frame
  expect_s3_class(cleanText_output1, "data.frame")
  cleanText_output2 <- CleanText(original_dataset,dtm_method=2,reductionrate=0.99)
  #output is a data frame
  expect_s3_class(cleanText_output2, "data.frame")
  #cleanText_output3 <- CleanText(original_dataset,dtm_method=3,reductionrate=0.999)
  #output is a data frame
  #expect_s3_class(cleanText_output3, "data.frame")

})
