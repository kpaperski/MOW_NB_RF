# Process data with naiveBayes classifier and randomForest classifier for number of iterations and calculates stats
#
# @param data - training data
# @param alc_colname - name of column in which the class labels are located
# @oaram unused_alc_colname - name of column which should be removed from data
# @oaram iterations - number of iterations of process
# @param ntree - ntree for randomForest model
# @param mtry - mtry for randomForest model
#
# returns the list of confusion matrix for each invocation and stats (mean , sd)
process_data <- function(data, alc_colname, unused_alc_colname, iterations, ntree, mtry) {
  classifier_results <- list()
  for (i in 1:iterations) {
    classifier_results[[i]] <- process_data_with_classifiers(data = stud_merge, alc_colname = alc_colname, unused_alc_colname = unused_alc_colname, ntree = ntree, mtry = mtry)
  }

  classifier_results_combined <- do.call(c, classifier_results)
  stat_results <- list()
  for (key in list("nb", "rf")) {
    values_with_key <- classifier_results_combined[names(classifier_results_combined) == key]
    accuracy_list <- map(values_with_key, function(x) return(x[["overall"]][["Accuracy"]]))
    stat_list <- calc_mean_sd(unlist(accuracy_list, use.names = FALSE))
    stat_results[[key]] <- stat_list
  }
  return(list(classifier_results = classifier_results, stats = stat_results))
}

# Process data with naiveBayes classifier and randomForest classifier
#
# @param data - training data
# @param alc_colname - name of column in which the class labels are located
# @oaram unused_alc_colname - name of column which should be removed from data
# @param ntree - ntree for randomForest model
# @param mtry - mtry for randomForest model
#
# returns the list of confusion matrix for naiveBayes classifier and randomForest classifier
process_data_with_classifiers <- function(data, alc_colname, unused_alc_colname, ntree, mtry) {
  initial_data <- data %>%
    select(-unused_alc_colname)

  # split into training and test data
  idx <- initial_split(data = initial_data, prop = 0.8, strata = alc_colname)
  train <- training(idx)
  test <- testing(idx)

  print("Naive Bayes")
  nb <- alc_NaiveBayesClassifier(train = train, test = test, alc_colname = alc_colname)
  print("Random Forest")
  rf <- alc_randomForestClassifier(train = train, test = test, alc_colname = alc_colname, ntree = ntree, mtry = mtry)

  return(list(nb = nb, rf = rf))
}

# Calculates mean and standard deviation
#
# @param values - input values
#
# returns the list with mean and sd elements
calc_mean_sd <- function(values) {
  mean <- mean(values)
  sdev <- sd(values)
  return(list(mean = mean, sd = sdev))
}