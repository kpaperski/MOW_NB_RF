library(plyr)
library(rsample)

source("alc_analysis.R")
source("alc_NaiveBayesClassifier.R")
source("process_data.R")
source("tune_random_forest.R")

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

# tunging random forest parameters
tune_res_dalc <- tune_random_forest(data = stud_merge, alc_colname = "Dalc", minus_alc_colname = "Walc")
print(tune_res_dalc)
tune_res_walc <- tune_random_forest(data = stud_merge, alc_colname = "Walc", minus_alc_colname = "Dalc")
print(tune_res_walc)

# calculations for 5 classes
res_dalc <- process_data(data = stud_merge, alc_colname = "Dalc", unused_alc_colname = "Walc", iterations = 10, ntree = 2000, mtry = 7)
res_walc <- process_data(data = stud_merge, alc_colname = "Walc", unused_alc_colname = "Dalc", iterations = 10, ntree = 2500, mtry = 7)

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

# tunging random forest parameters
tune_res_dalc_2_class <- tune_random_forest(data = stud_merge, alc_colname = "Dalc", minus_alc_colname = "Walc")
print(tune_res_dalc)
tune_res_walc_2_class <- tune_random_forest(data = stud_merge, alc_colname = "Walc", minus_alc_colname = "Dalc")
print(tune_res_walc)

# calculations for 2 classes
res_dalc_2_class <- process_data(data = stud_merge, alc_colname = "Dalc", unused_alc_colname = "Walc", iterations = 10, ntree = 1500, mtry = 7)
res_walc_2_class <- process_data(data = stud_merge, alc_colname = "Walc", unused_alc_colname = "Dalc", iterations = 10, ntree = 2000, mtry = 7)

