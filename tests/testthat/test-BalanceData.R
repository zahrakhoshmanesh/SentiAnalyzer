context("test-BalanceData")

test_that("BalanceData function work as expected", {
  expect_error(BalanceData())
  data(package = "SentiAnalyzer", imbalance_data)
  expect_s3_class(imbalance_data, "data.frame")
  #output
 balance_output <- BalanceData(dataset = imbalance_data)
 expect_s3_class(balance_output, "data.frame")
 
 })

