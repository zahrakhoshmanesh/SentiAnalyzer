#' Testing
#'
#' A reduced restaurant review dataset containing 98 terms of 1000 observation 
#'
#' @format A data frame with 1000 rows and 98 variables:
#' \describe{
#'   ...
#' }
"testing"

#' Testing1
#'
#' A further reduced restaurant review dataset containing 8 terms of 1000 observation
#'
#' @format Reduced dataframe for testing purpose with 1000 rows and 8 variables:
#' \describe{
#'   ...
#' }
"testing1"

#' imbalance_data
#'
#' A dataset containing restaurant review data, intentionally made unbalanced
#'
#' @format A data frame with 903 rows and 2 variables:
#' \describe{
#'   \item{Liked}{class variable}
#'   \item{Review}{text reviews}
#'   ...
#' }
#' @source \url{https://www.kaggle.com/c/restaurant-reviews}
"imbalance_data"

#' original_dataset
#'
#' A dataset containing restaurant review data
#'
#' @format A data frame with 1000 rows and 2 variables:
#' \describe{
#'   \item{Liked}{class variable}
#'   \item{Review}{text reviews}
#'   ...
#' }
#' @source \url{https://www.kaggle.com/c/restaurant-reviews}
"original_dataset"

#' clean_dataset
#'
#' output of CleanText, the corpus that is cleaned up and stemmed for base words
#'
#' @format A data frame with 1000 rows and 97 variables:
#' \describe{ cleaned dataset term matrix
#'   ...
#' }
"clean_dataset"

#' trained_models
#'
#' output of BuildTraining, lists
#'
#' @format A list with  5 trained algorithms resampling summaries: accuracy, kappa
#' \describe{ list, in sequence: GBM, KNN, NB, RF,SVM_Poly 
#'   ...
#' }
"trained_models"