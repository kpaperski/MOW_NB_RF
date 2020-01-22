# Title     : TODO
# Objective : TODO
# Created by: karol
# Created on: 28.12.2019

library(e1071)
library(dplyr)
library(rsample)
library(tidymodels)
library(caret)
library(ROCR)
library(infotheo)
library(randomForest)
source("alc_analysis.R")

clas_stats <- function(test, alc_colname, pred, prob, title) {
  table <- select(test, alc_colname) %>%
    bind_cols(alc_pred = pred)

  table %>%
    conf_mat(alc_colname, alc_pred) %>%
    autoplot(type = "heatmap")

  print(confusionMatrix(table$alc_pred, test[[alc_colname]]))

  class_labels <- sort(unique(table[[alc_colname]]), decreasing = FALSE)
  for (label in seq_along(class_labels)) {
    roc_data <- data.frame(prediction = prob[, label], actual = as.numeric(table[[alc_colname]] == label))
    pred_roc <- prediction(roc_data$prediction, roc_data$actual)
    perf <- performance(prediction.obj = pred_roc, measure = "tpr", x.measure = "fpr")
    plot_title <- paste(title, paste(c("class:", label), collapse = " "), collapse = " - ")
    plot(perf, main = plot_title)
    abline(a = 0, b = 1)
  }
}

alc_nbc <- function(stud_data, alc_colname) {
  set.seed(100) #podział na zb testowy i treningowy
  idx <- initial_split(data = stud_data, prop = 0.8, strata = alc_colname)
  train <- training(idx)
  test <- testing(idx)

  prop.table(table(train[[alc_colname]]))
  prop.table(table(test[[alc_colname]]))

  model <- naiveBayes(x = train, y = train[[alc_colname]], laplace = 1)

  pred <- predict(model, test, type = "class")
  prob <- predict(model, test, type = "raw")

  clas_stats(test, alc_colname, pred, prob, "Native Bayes")
}

alc_rfc <- function(stud_data, alc_colname, ntree, mtry) {
  set.seed(123) #podział na zb testowy i treningowy
  idx <- initial_split(data = stud_data, prop = 0.8, strata = alc_colname)
  train <- training(idx)
  test <- testing(idx)

  training_data <- train %>%
    select(-alc_colname)

  model <- randomForest(x = training_data, y = train[[alc_colname]], ntree = ntree, mtry = mtry)
  plot(model)
  # saveRDS(model, "model/Random_Forest_model_1.RDS")
  #model <- readRDS("model/Random_Forest_model.RDS")

  pred <- predict(model, test, type = "class")
  prob <- predict(model, test, type = "prob")

  clas_stats(test, alc_colname, pred, prob, "Random Forest")
}


# process

stud_math <- read.csv("student-mat.csv")
stud_port <- read.csv("student-por.csv")
stud_merge <- rbind(stud_math, stud_port)
stud_merge <- stud_merge[sample(1:nrow(stud_merge)),] #randomize rows

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

#Workday analysis
stud_merge_d <- stud_merge %>%
  select(-Walc)

print("Workday")
alc_analysis(stud_merge_d)
print("Workday Naive Bayes")
alc_nbc(stud_merge_d, "Dalc")
print("Workday Random Forest")
alc_rfc(stud_merge_d, "Dalc", 500, 20)

#Weekend analysis
stud_merge_w <- stud_merge %>%
  select(-Dalc)

print("Weekend")
alc_analysis(stud_merge_w)
print("Weekend Naive Bayes")
alc_nbc(stud_merge_w, "Walc")
print("Weekend Random Forest")
alc_rfc(stud_merge_w, "Walc", 500, 20)