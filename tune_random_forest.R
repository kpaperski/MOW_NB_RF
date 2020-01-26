source("alc_randomForestClassifier.R")

# Calculates accuracy of predictions for randomForest classifier with ntree and mtry parameters
#
# @param data - input data (x)
# @param alc_colname - name of column in which the class labels are located
# @oaram minus_alc_colname - name of column which should be removed from data
#
# returns the matrix in which the first row is the ntree parameter, the first column is the mtry parameter
# the other fields are the accuracy for the ntree and mtry parameters
tune_random_forest <- function(data, alc_colname, minus_alc_colname) {
  initial_data <- data %>%
    select(-minus_alc_colname)

  # split into training and test data
  idx <- initial_split(data = initial_data, prop = 0.8, strata = alc_colname)
  train <- training(idx)
  test <- testing(idx)

  # values for search random forest parameters
  ntrees <- c(100, 200, 300, 500, 700, 1000, 1200, 1500, 2000, 2500, 3000)
  mtrys <- c(3, 5, 6, 7, 9, 11, 13, 15, 17)

  accuracy_matrix <- matrix(0, nrow = length(ntrees), ncol = length(mtrys), dimnames = list(ntrees, mtrys))
  for (ntree in ntrees) {
    for (mtry in mtrys) {
      print("Random Forest")
      print(paste(c("mtry:", mtry), collapse = " "))
      print(paste(c("ntree:", ntree), collapse = " "))
      cm_rf <- alc_randomForestClassifier(train = train, test = test, alc_colname = alc_colname, ntree = ntree, mtry = mtry)
      accuracy_matrix[as.character(ntree), as.character(mtry)] <- round(cm_rf[["overall"]][["Accuracy"]], 4)
    }
  }
  return(accuracy_matrix)
}