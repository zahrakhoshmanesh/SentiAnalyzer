context("test-team_6")

test_that("test for function BalanceData :checking dataset is balanced and fix it to balanced dataset", {
  
  #test returning warning if file path not provided by user and set to default path
  expect_warning(BalanceData(),"file path does not provided by user, set to default file path, /data/Imbalance_Restaurant_Reviews.tsv")
  
  #test returning warning if file path does not exist and set to default path
  expect_warning(BalanceData(file="s"),"file does not exist, set to default file, Australia map")
  
  #test the result is data frame
  #expect_warning(is.data.frame(BalanceData(tolerance = 0.1)),"file path does not provided by user, set to default file path, gadm36_AUS_shp/gadm36_AUS_1.shp")
  
  #expect_named(team_6(tolerance = 0.1),c("group","long","lat","temporary.group","order","GID_0","NAME_0","GID_1","NAME_1","TYPE_1","ENGTYPE_1","CC_1","HASC_1") )
  
})

