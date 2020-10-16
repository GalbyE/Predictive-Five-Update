#This file is for the erros in study 2, presented in table 1:


#load library:

library(rrpack)
library(data.table)

#loading data:
  
positive_data <- fread("positive experiences.csv", header = T)
rotation.mat<- fread("rotation_mat.csv", header = T)
pc5 <-fread("pc5.csv", header = T)
q.pc5<- as.matrix(positive_data[,q1.x:q100]) %*% as.matrix(pc5)
embu_all<- as.data.table(cbind(positive_data, q.pc5))


#sub_embu <- -embu_all[,c('V1','V2','V3','V4','V5','O','C','E','A','N','F_Rejection','F_Warmth','F_Overprotection',
#                         'M_Rejection','M_Warmth','M_Overprotection','Positive_experience')]

#View(cor(sub_embu))
#cor(sub_embu$V4,sub_embu$F_Rejection)


set.seed(1)
folds <- 4
fold.assignment <- sample(1:folds, nrow(embu_all), replace = TRUE)

################################## F_Rejection#####################

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  F_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Rejection.cross.test <- embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Rejection~V1+V2+V3+V4+V5 ,data = F_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Rejection.cross.test)
  .errors <-   (F_Rejection.cross.test$F_Rejection)- (.predictions) # save prediction errors in the fold
  errors<- c(errors, .errors) # aggregate error over folds.
}

errors.pf.F_Rejection <-errors


#Big5 model with CV, fold=4:

errors <- NULL


for (k in 1:folds){
  F_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Rejection~O+C+E+A+N ,data = F_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Rejection.cross.test)
  .errors <-   (F_Rejection.cross.test$F_Rejection)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.big5.F_Rejection <-errors


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  F_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Rejection~PC1+PC2+PC3+PC4+PC5 ,data = F_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Rejection.cross.test)
  .errors <-(F_Rejection.cross.test$F_Rejection)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.F_Rejection <-errors

mean(abs(errors.pc5.F_Rejection))
mean(abs(errors.pf.F_Rejection))
mean(abs(errors.big5.F_Rejection))

errors.F_Rejection<- data.frame(abs(errors.pc5.F_Rejection), abs(errors.pf.F_Rejection),abs(errors.big5.F_Rejection))

#write.csv(errors.F_Rejection,'errors.F_Rejection.csv')


##################################F_Warmth#####################

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  F_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Warmth.cross.test <- embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Warmth~V1+V2+V3+V4+V5 ,data = F_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Warmth.cross.test)
  .errors <-   (F_Warmth.cross.test$F_Warmth)- (.predictions) # save prediction errors in the fold
  errors<- c(errors, .errors) # aggregate error over folds.
}

errors.pf.F_Warmth <-errors


#Big5 model with CV, fold=4:

errors <- NULL


for (k in 1:folds){
  F_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Warmth~O+C+E+A+N ,data = F_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Warmth.cross.test)
  .errors <-   (F_Warmth.cross.test$F_Warmth)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.big5.F_Warmth <-errors


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  F_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Warmth~PC1+PC2+PC3+PC4+PC5 ,data = F_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Warmth.cross.test)
  .errors <-(F_Warmth.cross.test$F_Warmth)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.F_Warmth <-errors

mean(abs(errors.pc5.F_Warmth))
mean(abs(errors.pf.F_Warmth))
mean(abs(errors.big5.F_Warmth))

errors.F_Warmth<- data.frame(abs(errors.pc5.F_Warmth), abs(errors.pf.F_Warmth),abs(errors.big5.F_Warmth))

#write.csv(errors.F_Warmth,'errors.F_Warmth.csv')


#######################################F_Overprotection###############


#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  F_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Overprotection.cross.test <- embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Overprotection~V1+V2+V3+V4+V5 ,data = F_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Overprotection.cross.test)
  .errors <-   (F_Overprotection.cross.test$F_Overprotection)- (.predictions) # save prediction errors in the fold
  errors<- c(errors, .errors) # aggregate error over folds.
}

errors.pf.F_Overprotection <-errors


#Big5 model with CV, fold=4:

errors <- NULL


for (k in 1:folds){
  F_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Overprotection~O+C+E+A+N ,data = F_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Overprotection.cross.test)
  .errors <-   (F_Overprotection.cross.test$F_Overprotection)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.big5.F_Overprotection <-errors


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  F_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Overprotection~PC1+PC2+PC3+PC4+PC5 ,data = F_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Overprotection.cross.test)
  .errors <-(F_Overprotection.cross.test$F_Overprotection)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.F_Overprotection <-errors

mean(abs(errors.pc5.F_Overprotection))
mean(abs(errors.pf.F_Overprotection))
mean(abs(errors.big5.F_Overprotection))

errors.F_Overprotection<- data.frame(abs(errors.pc5.F_Overprotection), abs(errors.pf.F_Overprotection),abs(errors.big5.F_Overprotection))

#write.csv(errors.F_Overprotection,'errors.F_Overprotection.csv')

################################## M_Rejection#####################

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  M_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Rejection.cross.test <- embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Rejection~V1+V2+V3+V4+V5 ,data = M_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Rejection.cross.test)
  .errors <-   (M_Rejection.cross.test$M_Rejection)- (.predictions) # save prediction errors in the fold
  errors<- c(errors, .errors) # aggregate error over folds.
}

errors.pf.M_Rejection <-errors


#Big5 model with CV, fold=4:

errors <- NULL


for (k in 1:folds){
  M_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Rejection~O+C+E+A+N ,data = M_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Rejection.cross.test)
  .errors <-   (M_Rejection.cross.test$M_Rejection)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.big5.M_Rejection <-errors


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  M_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Rejection~PC1+PC2+PC3+PC4+PC5 ,data = M_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Rejection.cross.test)
  .errors <-(M_Rejection.cross.test$M_Rejection)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.M_Rejection <-errors

mean(abs(errors.pc5.M_Rejection))
mean(abs(errors.pf.M_Rejection))
mean(abs(errors.big5.M_Rejection))

errors.M_Rejection<- data.frame(abs(errors.pc5.M_Rejection), abs(errors.pf.M_Rejection),abs(errors.big5.M_Rejection))

#write.csv(errors.M_Rejection,'errors.M_Rejection.csv')


##################################M_Warmth#####################

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  M_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Warmth.cross.test <- embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Warmth~V1+V2+V3+V4+V5 ,data = M_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Warmth.cross.test)
  .errors <-   (M_Warmth.cross.test$M_Warmth)- (.predictions) # save prediction errors in the fold
  errors<- c(errors, .errors) # aggregate error over folds.
}

errors.pf.M_Warmth <-errors


#Big5 model with CV, fold=4:

errors <- NULL


for (k in 1:folds){
  M_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Warmth~O+C+E+A+N ,data = M_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Warmth.cross.test)
  .errors <-   (M_Warmth.cross.test$M_Warmth)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.big5.M_Warmth <-errors


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  M_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Warmth~PC1+PC2+PC3+PC4+PC5 ,data = M_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Warmth.cross.test)
  .errors <-(M_Warmth.cross.test$M_Warmth)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.M_Warmth <-errors

mean(abs(errors.pc5.M_Warmth))
mean(abs(errors.pf.M_Warmth))
mean(abs(errors.big5.M_Warmth))

errors.M_Warmth<- data.frame(abs(errors.pc5.M_Warmth), abs(errors.pf.M_Warmth),abs(errors.big5.M_Warmth))

#write.csv(errors.M_Warmth,'errors.M_Warmth.csv')


#######################################M_Overprotection###############


#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  M_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Overprotection.cross.test <- embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Overprotection~V1+V2+V3+V4+V5 ,data = M_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Overprotection.cross.test)
  .errors <-   (M_Overprotection.cross.test$M_Overprotection)- (.predictions) # save prediction errors in the fold
  errors<- c(errors, .errors) # aggregate error over folds.
}

errors.pf.M_Overprotection <-errors


#Big5 model with CV, fold=4:

errors <- NULL


for (k in 1:folds){
  M_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Overprotection~O+C+E+A+N ,data = M_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Overprotection.cross.test)
  .errors <-   (M_Overprotection.cross.test$M_Overprotection)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.big5.M_Overprotection <-errors


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  M_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Overprotection~PC1+PC2+PC3+PC4+PC5 ,data = M_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Overprotection.cross.test)
  .errors <-(M_Overprotection.cross.test$M_Overprotection)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.M_Overprotection <-errors

mean(abs(errors.pc5.M_Overprotection))
mean(abs(errors.pf.M_Overprotection))
mean(abs(errors.big5.M_Overprotection))

errors.M_Overprotection<- data.frame(abs(errors.pc5.M_Overprotection), abs(errors.pf.M_Overprotection),abs(errors.big5.M_Overprotection))

#write.csv(errors.M_Overprotection,'errors.M_Overprotection.csv')
