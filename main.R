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
source("alc_analysis.R")
source("alc_nb.R")
source("alc_rf.R")

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
    average_grade = round((G1+G2+G3)/3,2)
  ) %>%
  select(-c(G1,G2,G3))

#Workday analysis
stud_merge_D <- stud_merge %>%
  select(-Walc)

print("Workday")
alc_analysis(stud_merge_D)
print("Workday Naive Bayes")
alc_nb(stud_merge_D, "Dalc")
print("Workday Random Forest")
alc_rf(stud_merge_D, "Dalc")


#Weekend analysis
stud_merge_W <- stud_merge %>%
  select(-Dalc)
print("Weekend")
alc_analysis(stud_merge_W)
print("Weekend Naive Bayes")
alc_nb(stud_merge_W, "Walc")
print("Weekend Random Forest")
alc_rf(stud_merge_W, "Walc")