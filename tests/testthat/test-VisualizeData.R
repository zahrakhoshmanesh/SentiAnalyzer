context("test-VisualizeData")

test_that("test for function VisualizeData :Visualize the dataset and plot high term frequency", {
  expect_error(VisualizeData())
  expect_error(VisualizeData("m"))
})
