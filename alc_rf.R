# Title     : TODO
# Objective : TODO
# Created by: karol
# Created on: 02.01.2020

alc_rf <- function (stud_merge_D, colname) {
  set.seed(100) #podział na zb testowy i treningowy
  idx <- initial_split(data = stud_merge_D, prop = 0.8, strata = colname)
  train <- training(idx)
  test <- testing(idx)
  ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
  model_RF_Day <- train(Dalc~., data = train, method = "rf", ntree = 6, trControl = ctrl)
  saveRDS(model_RF_Day, "model/Random_Forest_model.RDS")
  #model_RF_Day <- readRDS("model/Random_Forest_model.RDS")

  RF_Day_pred <- predict(model_RF_Day, test, type = "raw")
  RF_Day_prob <- predict(model_RF_Day, test, type = "prob")

  RF_table_Day <- select(test, Dalc) %>%
    bind_cols(Dalc_pred = RF_Day_pred) %>%
    bind_cols(Dalc_1prob = round(RF_Day_prob[,1],4)) %>%
    bind_cols(Dalc_2prob = round(RF_Day_prob[,2],4)) %>%
    bind_cols(Dalc_3prob = round(RF_Day_prob[,3],4)) %>%
    bind_cols(Dalc_4prob = round(RF_Day_prob[,4],4)) %>%
    bind_cols(Dalc_5prob = round(RF_Day_prob[,5],4))

  RF_table_Day %>%
    conf_mat(Dalc, Dalc_pred) %>%
    autoplot(type = "heatmap")

  print(confusionMatrix(data = RF_Day_pred, reference = test[[colname]]))

  RF_ROC <- data.frame(prediction1 = RF_Day_prob[,1],
                        actual1 = as.numeric(RF_table_Day[[colname]] == 1),
                        prediction2 = RF_Day_prob[,2],
                        actual2 = as.numeric(RF_table_Day[[colname]] == 2),
                        prediction3 = RF_Day_prob[,3],
                        actual3 = as.numeric(RF_table_Day[[colname]] == 3),
                        prediction4 = RF_Day_prob[,4],
                        actual4 = as.numeric(RF_table_Day[[colname]] == 4),
                        prediction5 = RF_Day_prob[,5],
                        actual5 = as.numeric(RF_table_Day[[colname]] == 5))
  #head(NB_ROC)

  pred_roc1_RF <- prediction(RF_ROC$prediction1, RF_ROC$actual1)
  pred_roc2_RF <- prediction(RF_ROC$prediction2, RF_ROC$actual2)
  pred_roc3_RF <- prediction(RF_ROC$prediction3, RF_ROC$actual3)
  pred_roc4_RF <- prediction(RF_ROC$prediction4, RF_ROC$actual4)
  pred_roc5_RF <- prediction(RF_ROC$prediction5, RF_ROC$actual5)

  perf1_RF <- performance(prediction.obj = pred_roc1_RF, measure = "tpr", x.measure = "fpr")
  perf2_RF <- performance(prediction.obj = pred_roc2_RF, measure = "tpr", x.measure = "fpr")
  perf3_RF <- performance(prediction.obj = pred_roc3_RF, measure = "tpr", x.measure = "fpr")
  perf4_RF <- performance(prediction.obj = pred_roc4_RF, measure = "tpr", x.measure = "fpr")
  perf5_RF <- performance(prediction.obj = pred_roc5_RF, measure = "tpr", x.measure = "fpr")

  plot(perf1_RF)
  abline(a = 0, b = 1)
  plot(perf2_RF)
  abline(a = 0, b = 1)
  plot(perf3_RF)
  abline(a = 0, b = 1)
  plot(perf4_RF)
  abline(a = 0, b = 1)
  plot(perf5_RF)
  abline(a = 0, b = 1)
}