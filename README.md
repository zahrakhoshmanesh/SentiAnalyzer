

<!-- badges: start -->
[![travis](https://travis-ci.org/zahrakhoshmanesh/SentiAnalyzer.svg?branch=master)](https://travis-ci.org/zahrakhoshmanesh/SentiAnalyzer)
[![Coverage status](https://codecov.io/gh/zahrakhoshmanesh/SentiAnalyzer/branch/master/graph/badge.svg)](https://codecov.io/github/zahrakhoshmanesh/SentiAnalyzer?branch=master)


<!-- badges: end -->

# SentiAnalyzer
<img src="inst/extdata/SAhexlogo.png" align="right" width="120"/>



### Sentiment analysis for consumer review

SentiAnalyzer is a straight forward solution for analysis of of consumer reviews which includes natural language processing (NLP) to preprocess the reviews and prepare the term matrix of reviews data in order to be consistent with Machine Learning algorithms. The dataset that SentiAnalyzer can work with is the review text and a binary class indicating the customer sentiment, e.g. whether consumer feeling is possitive or negative. 

##  Overview of the SentiAnalyzer package 

1. Balancing the dataset : using function BalanceData

2. Cleaning, Tokenizing and Building the term matrix of reviews : using function CleanText

3. Visualize the clean term matrix to give the user the insight inside the data : using function VisualzieData and shiny app

4. Train different classification algorithms(e.g., SVM,NB,RF,KNN,GBM) and choose best parameters for each one to get the highest possible classification accuracy for an specefic dataset : using function BuildTraining

5. Choose the best trained classification algorithm for the specific dataset according to different measures (e.g., FScore, Recall, Precision, Accuracy) : using function Comparison

6. Predict on the new review data : using function BuildPrediction

7. Visualize the output of the confusion matrix, that is, the accuracy, precision, recall and f1-score of the training model in predicting the sentiment of the consumer review : Using function Comparison and Shiny app



## Installation
`install.packages("SentiAnalyzer")`

## How to use the package

The package website: [SentiAnalyzer](https://zahrakhoshmanesh.github.io/SentiAnalyzer/)

Tutorial : [Workflow of Functions](https://zahrakhoshmanesh.github.io/SentiAnalyzer/articles/workflow.html)

Shiny versions:  

1. [What is incide the data: Interactive Visualization](https://joeybudi.shinyapps.io/zahra/)

2. [Training Algorithms: Interactive version](https://joeybudi.shinyapps.io/atousa/)







