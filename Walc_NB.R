# Title     : TODO
# Objective : TODO
# Created by: karol
# Created on: 02.01.2020

walc_n_b <- function (stud_merge) {
  set.seed(100) #podział na zb testowy i treningowy
  idx <- initial_split(data = stud_merge, prop = 0.8, strata = "Walc")
  train <- training(idx)
  test <- testing(idx)

  prop.table(table(train$Walc))
  prop.table(table(test$Walc))

  model_NB_Day <- naiveBayes(x = train, y = train$Walc, laplace = 1)

  NB_Day_pred <- predict(model_NB_Day, test, type = "class")
  NB_Day_prob <- predict(model_NB_Day, test, type = "raw")

  NB_table_Day <- select(test, Dalc) %>%
    bind_cols(Dalc_pred = NB_Day_pred) %>%
    bind_cols(Dalc_1prob = round(NB_Day_prob[,1],4)) %>%
    bind_cols(Dalc_2prob = round(NB_Day_prob[,2],4)) %>%
    bind_cols(Dalc_3prob = round(NB_Day_prob[,3],4)) %>%
    bind_cols(Dalc_4prob = round(NB_Day_prob[,4],4)) %>%
    bind_cols(Dalc_5prob = round(NB_Day_prob[,5],4))

  NB_table_Day %>%
    conf_mat(Dalc, Dalc_pred) %>%
    autoplot(type = "heatmap")

  print(confusionMatrix(data = NB_Day_pred, reference = test$Walc))

  #head(NB_Day_prob)

  NB_ROC <- data.frame(prediction1 = NB_Day_prob[,1],
                        actual1 = as.numeric(NB_table_Day$Walc == 1),
                        prediction2 = NB_Day_prob[,2],
                        actual2 = as.numeric(NB_table_Day$Walc == 2),
                        prediction3 = NB_Day_prob[,3],
                        actual3 = as.numeric(NB_table_Day$Walc == 3),
                        prediction4 = NB_Day_prob[,4],
                        actual4 = as.numeric(NB_table_Day$Walc == 4),
                        prediction5 = NB_Day_prob[,5],
                        actual5 = as.numeric(NB_table_Day$Walc == 5))
  #head(NB_ROC)

  pred_roc1 <- prediction(NB_ROC$prediction1, NB_ROC$actual1)
  pred_roc2 <- prediction(NB_ROC$prediction2, NB_ROC$actual2)
  pred_roc3 <- prediction(NB_ROC$prediction3, NB_ROC$actual3)
  pred_roc4 <- prediction(NB_ROC$prediction4, NB_ROC$actual4)
  pred_roc5 <- prediction(NB_ROC$prediction5, NB_ROC$actual5)

  perf1 <- performance(prediction.obj = pred_roc1, measure = "tpr", x.measure = "fpr")
  perf2 <- performance(prediction.obj = pred_roc2, measure = "tpr", x.measure = "fpr")
  perf3 <- performance(prediction.obj = pred_roc3, measure = "tpr", x.measure = "fpr")
  perf4 <- performance(prediction.obj = pred_roc4, measure = "tpr", x.measure = "fpr")
  perf5 <- performance(prediction.obj = pred_roc5, measure = "tpr", x.measure = "fpr")

  plot(perf1)
  abline(a = 0, b = 1)
  plot(perf2)
  abline(a = 0, b = 1)
  plot(perf3)
  abline(a = 0, b = 1)
  plot(perf4)
  abline(a = 0, b = 1)
  plot(perf5)
  abline(a = 0, b = 1)
}