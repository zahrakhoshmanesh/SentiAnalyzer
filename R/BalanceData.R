
#' Clean text and buil term matrix for bag of words model or TF DFI.
#'
#' @param x unbalanced dataset.
#' @return balanced dataframe
#' @author Zahra Khoshmanesh
#' @export
#' @import ROSE
#' @import assertthat
#' @import testthat

BalanceData<-function(x){

  library(ROSE)
  data(hacide)
  df<-x

  #over sampling
  data_balanced_over <- ovun.sample(cls ~ ., data = hacide.train, method = "over",N = 1960)$data
  table(data_balanced_over$cls)

  data_balanced_under <- ovun.sample(cls ~ ., data = hacide.train, method = "under", N = 40, seed = 1)$data
  table(data_balanced_under$cls)

  data_balanced_both <- ovun.sample(cls ~ ., data = hacide.train, method = "both", p=0.5,N=1000, seed = 1)$data
  table(data_balanced_both$cls)

  data.rose <- ROSE(cls ~ ., data = hacide.train, seed = 1)$data
  table(data.rose$cls)

  #check table
  table(df$Liked)
  data_balanced_over <- ovun.sample(Liked ~ ., data = df, method = "over",N = 1000)$data
  table(data_balanced_over$Liked)

  data_balanced_under <- ovun.sample(Liked ~ ., data = df, method = "under", N = 500, seed = 1)$data
  table(data_balanced_under$Liked)

  data_balanced_both <- ovun.sample(Liked ~ ., data = df, method = "both", p=0.5,N=500, seed = 1)$data
  table(data_balanced_both$Liked)

  data.rose <- ROSE(Liked ~ ., data = df, seed = 1)$data
  table(data.rose$Liked)
  return(balansed_df)
}
