library(randomForest)
source("clas_stats.R")

# Trains random forest model
#
# @param train - training data
# @param test - test data
# @param alc_colname - name of column in which the class labels are located
# @param ntree - ntree for randomForest model
# @param mtry - mtry for randomForest model
#
# returns the confusion matrix of classifier predictions for test data
alc_randomForestClassifier <- function(train, test, alc_colname, ntree, mtry) {
  training_data <- train %>%
    select(-alc_colname)

  model <- randomForest(x = training_data, y = train[[alc_colname]], ntree = ntree, mtry = mtry)
  pred <- predict(model, test, type = "class")
  prob <- predict(model, test, type = "prob")

  return(clas_stats(data = test, alc_colname = alc_colname, pred = pred, prob = prob, plot_title = "Random Forest"))
}