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
source("Dalc_RF.R")
source("Dalc_analysis.R")
source("Dalc_NB.R")
source("Walc_RF.R")
source("Walc_NB.R")

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

stud_merge_D <- stud_merge %>%
  select(-c(Walc))

#dalc_analysis(stud_merge_D)
#dalc_n_b(stud_merge_D)
#dalc_r_f(stud_merge_D)

stud_merge_W <- stud_merge %>%
  select(-c(Dalc))

#dalc_analysis(stud_merge_W)
#walc_n_b(stud_merge_D)
#walc_r_f(stud_merge_D)