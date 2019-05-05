context("test-VisualizeData")

test_that("test for function VisualizeData :Visualize the dataset and plot high term frequency", {
  
  ##########test input#####################
  
  expect_error(VisualizeData())
  data(package = "SentiAnalyzer", original_dataset)
  expect_s3_class(original_dataset, "data.frame")
  
  ########### test output##################
  
  vis_out <- VisualizeData(dataset=original_dataset,termcount=15)
  #test output is a ggplot object
  expect_s3_class(vis_out, "ggplot")
  expect_true(is.ggplot(vis_out))
  

})
