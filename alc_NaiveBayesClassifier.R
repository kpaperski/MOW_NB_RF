library(e1071)
source("clas_stats.R")

# Trains naiveBayes model
#
# @param train - training data
# @param test - test data
# @param alc_colname - name of column in which the class labels are located
#
# returns the confusion matrix of classifier predictions for test data
alc_NaiveBayesClassifier <- function(train, test, alc_colname) {
  prop.table(table(train[[alc_colname]]))
  prop.table(table(test[[alc_colname]]))

  training_data <- train %>%
    select(-alc_colname)

  model <- naiveBayes(x = training_data, y = train[[alc_colname]], laplace = 1)
  pred <- predict(model, test, type = "class")
  prob <- predict(model, test, type = "raw")

  return(clas_stats(data = test, alc_colname = alc_colname, pred = pred, prob = prob, plot_title = "Naive Bayes"))
}