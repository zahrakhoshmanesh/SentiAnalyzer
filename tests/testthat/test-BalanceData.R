context("test-BalanceData")

test_that("test for function BalanceData :checking dataset is balanced and fix it to balanced dataset", {
  expect_error(BalanceData())
  data(package = "SentiAnalyzer", imbalance_data)
  expect_s3_class(imbalance_data, "data.frame")
  #output
  balance_output <- BalanceData(dataset = imbalance_data)
  #output is a data frame
  expect_s3_class(balance_output, "data.frame")
 
 })

