% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BalanceData.R
\name{BalanceData}
\alias{BalanceData}
\title{Clean text and build term matrix for bag of words model or TF DFI.}
\usage{
BalanceData(dataset)
}
\arguments{
\item{dataset}{unbalanced dataset, a dataframe : two column: first text reviews and second binary class, label: negative =0 and positive=1.}
}
\value{
balanced_dataframe balanced dataframe containing two columns: review texts and binary class , label: negative =0 and positive=1.

A balanced dataframe
}
\description{
Clean text and build term matrix for bag of words model or TF DFI.
}
\examples{
\dontrun{
library("SentiAnalyzer")
direction <- system.file(package = "SentiAnalyzer", "extdata/Imbalance_Restaurant_Reviews.tsv")
imbalance_data<- read.delim(direction,quote='',stringsAsFactors = FALSE)
BalanceData(imbalance_data)}
}
\author{
Zahra Khoshmanesh
}
