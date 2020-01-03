# Title     : TODO
# Objective : TODO
# Created by: karol
# Created on: 02.01.2020

dalc_analysis <- function (stud_merge_D) {
  st_mg <-discretize(stud_merge_D)
  mutinf <- c()
  chisqTestpValue <- c()
  for(i in seq(1, 30)) {
    chisqTestpValue <- c(chisqTestpValue, chisq.test(table(stud_merge_D[,i], stud_merge_D[,27]))$p.value)
    mutinf <- c(mutinf, mutinformation(st_mg[,i], st_mg[,27]))
  }
  names(mutinf) <- names(stud_merge_D)
  names(chisqTestpValue) <- names(stud_merge_D)
  print("Mutinf:")
  print(sort(mutinf))
  print("Test chi2:")
  print(sort(chisqTestpValue))
}