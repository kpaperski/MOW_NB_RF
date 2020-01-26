library(dplyr)
library(ROCR)
library(tidymodels)
library(caret)

# Calculates confusion matrix and plot roc curves
#
# @param data - input data
# @param alc_colname - name of column in which the class labels are located
# @param pred - vector of predictions for data
# @param prob - vector of probabilities of predictions for data
# @param plot_title - title prefix for roc curve plot
#
# returns the confusion matrix of classifier predictions for data
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