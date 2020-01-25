# Title     : TODO
# Objective : TODO
# Created by: karol
# Created on: 28.12.2019

library(e1071)
library(dplyr)
library(plyr)
library(rsample)
library(tidymodels)
library(caret)
library(ROCR)
library(infotheo)
library(randomForest)
source("alc_analysis.R")

clas_stats <- function(data, alc_colname, pred, prob, plot_title) {
  table <- select(data, alc_colname) %>%
    bind_cols(alc_pred = pred)

  table %>%
    conf_mat(alc_colname, alc_pred) %>%
    autoplot(type = "heatmap")

  # confusion matrix
  cm <- confusionMatrix(table$alc_pred, data[[alc_colname]])
  print(cm)

  # ROC curve plot
  class_labels <- sort(unique(table[[alc_colname]]), decreasing = FALSE)
  for (label in seq_along(class_labels)) {
    roc_data <- data.frame(prediction = prob[, label], actual = as.numeric(table[[alc_colname]] == label))
    pred_roc <- prediction(roc_data$prediction, roc_data$actual)
    perf <- performance(prediction.obj = pred_roc, measure = "tpr", x.measure = "fpr")
    title <- paste(plot_title, paste(c("class:", label), collapse = " "), collapse = " - ")
    # plot(perf, main = title)
    # abline(a = 0, b = 1)
  }

  return(cm)
}

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

alc_randomForestClassifier <- function(train, test, alc_colname, ntree, mtry) {
  training_data <- train %>%
    select(-alc_colname)

  model <- randomForest(x = training_data, y = train[[alc_colname]], ntree = ntree, mtry = mtry)
  pred <- predict(model, test, type = "class")
  prob <- predict(model, test, type = "prob")

  return(clas_stats(data = test, alc_colname = alc_colname, pred = pred, prob = prob, plot_title = "Random Forest"))
}

process_data_with_classifiers <- function(data, alc_colname, unused_alc_colname) {
  initial_data <- data %>%
    select(-unused_alc_colname)

  # split into training and test data
  idx <- initial_split(data = initial_data, prop = 0.8, strata = alc_colname)
  train <- training(idx)
  test <- testing(idx)

  print("Naive Bayes")
  nb <- alc_NaiveBayesClassifier(train = train, test = test, alc_colname = alc_colname)
  print("Random Forest")
  rf <- alc_randomForestClassifier(train = train, test = test, alc_colname = alc_colname, ntree = 3000, mtry = 10)

  data_nb <- extract_data_from_cm(nb)
  data_rf <- extract_data_from_cm(rf)

  return(list(nb = data_nb, rf = data_rf))
}

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
      data_rf <- extract_data_from_cm(conf_mat = cm_rf)
      accuracy_matrix[as.character(ntree), as.character(mtry)] <- round(data_rf[["accuracy"]], 4)
    }
  }
  return(accuracy_matrix)
}

assign_new_labels <- function(data, changed_columns, label_holder) {
  for (col in changed_columns) {
    for (row in seq_len(nrow(label_holder))) {
      old_label <- label_holder[row, "old"]
      new_label <- label_holder[row, "new"]
      levels(data[[col]])[levels(data[[col]]) == old_label] <- new_label
    }
  }
  return(data.frame(data))
}

extract_data_from_cm <- function(conf_mat) {
  accuracy <- conf_mat[["overall"]][["Accuracy"]]
  cm <- conf_mat[["table"]]
  return(list(accuracy = accuracy, cm = cm))
}

extract_data_from_cm_for_2_class <- function(conf_mat) {
  accuracy <- conf_mat[["overall"]][["Accuracy"]]
  precision <- conf_mat[["byClass"]][["Precision"]]
  recall <- conf_mat[["byClass"]][["Recall"]]
  fmeasure <- conf_mat[["byClass"]][["F1"]]
  cm <- conf_mat[["table"]]
  return(list(accuracy = accuracy, precision = precision, recall = recall, fmeasure = fmeasure, cm = cm))
}

process_data <- function(data, alc_colname, unused_alc_colname, iterations) {
  classifier_results <- list()
  for (i in 1:iterations) {
    classifier_results[[i]] <- process_data_with_classifiers(data = stud_merge, alc_colname = "Walc", unused_alc_colname = "Dalc")
  }

  classifier_results_combined <- do.call(c, classifier_results)
  stat_results <- list()
  for (key in list("nb", "rf")) {
    values_with_key <- classifier_results_combined[names(classifier_results_combined) == key]
    accuracy_list <- map(values_with_key, function(x) return(x[["accuracy"]]))
    stat_list <- calc_mean_sd(unlist(accuracy_list, use.names = FALSE))
    stat_results[[key]] <- stat_list
  }
  return (list(classifier_results = classifier_results, stats = stat_results))
}

calc_mean_sd <- function(values) {
  mean <- mean(values)
  sdev <- sd(values)
  return(list(mean = mean, sd = sdev))
}

# read data from csv sources
stud_math <- read.csv("student-mat.csv")
stud_port <- read.csv("student-por.csv")

stud_merge <- rbind(stud_math, stud_port)
stud_merge <- stud_merge[sample(1:nrow(stud_merge)),] #randomize rows

# change numeric values to discrete
stud_merge <- stud_merge %>%
  mutate(
    Medu = as.factor(Medu),
    Fedu = as.factor(Fedu),
    traveltime = as.factor(traveltime),
    studytime = as.factor(studytime),
    failures = as.factor(failures),
    famrel = as.factor(famrel),
    freetime = as.factor(freetime),
    goout = as.factor(goout),
    Dalc = as.factor(Dalc),
    Walc = as.factor(Walc),
    health = as.factor(health),
    average_grade = round((G1 + G2 + G3) / 3, 2)
  ) %>%
  select(-c(G1, G2, G3))

# calculations for 5 classes
res_dalc <- process_data(data = stud_merge, alc_colname = "Dalc", unused_alc_colname = "Walc", iterations = 2)
res_walc <- process_data(data = stud_merge, alc_colname = "Walc", unused_alc_colname = "Dalc", iterations = 2)

# tunging random forest parameters
tune_res_dalc <- tune_random_forest(data = stud_merge, alc_colname = "Dalc", minus_alc_colname = "Walc")
print(tune_res_dalc)
tune_res_walc <- tune_random_forest(data = stud_merge, alc_colname = "Walc", minus_alc_colname = "Dalc")
print(tune_res_walc)

# overriting class labels to new classes (2)
changed_columns <- c("Dalc", "Walc")
label_holder <- data.frame(new = c(1, 1, 2, 2, 2), old = c(1, 2, 3, 4, 5))
for (col in changed_columns) {
  for (row in seq_len(nrow(label_holder))) {
    old_label <- label_holder[row, "old"]
    new_label <- label_holder[row, "new"]
    levels(stud_merge[[col]])[levels(stud_merge[[col]]) == old_label] <- new_label
  }
}

# calculations for 2 classes
res_dalc_2_class <- process_data(data = stud_merge, alc_colname = "Dalc", unused_alc_colname = "Walc", iterations = 2)
res_walc_2_class <- process_data(data = stud_merge, alc_colname = "Walc", unused_alc_colname = "Dalc", iterations = 2)

