context("test-VisualizeData")

test_that("test for function VisualizeData :Visualize the dataset and plot high term frequency", {
  expect_error(VisualizeData())
  data(package = "SentiAnalyzer", original_dataset)
  expect_s3_class(original_dataset, "data.frame")

})
