#This file is for the R2 in study 2, presented in table 1:


source('utility.R')

positive_data <- fread("Data/positive experiences.csv", header = T)
rotation.mat<- fread("Data/rotation_mat.csv", header = T)
pc5 <-fread("Data/pc5.csv", header = T)
q.pc5<- as.matrix(positive_data[,q1.x:q100]) %*% as.matrix(pc5)
embu_all<- as.data.table(cbind(positive_data, q.pc5))

#creating the df for the results:

embu.results<- data.table(dv=rep(c("F_Rejection", "F_Warmth", "F_Overprotection",
                                   "M_Rejection", "M_Warmth", "M_Overprotection"), each=24), 
                          Type= rep(c("Predictive Five", "Big Five", "PC Five"), each=8, times= 6),
                          Set= rep(c("Test", "Train"),each=4, times= 18))


set.seed(1)
folds <- 4
fold.assignment <- sample(1:folds, nrow(embu_all), replace = TRUE)
r2<-NULL
sd<-NULL

################################## F_Rejection#####################

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  F_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Rejection.cross.test <- embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Rejection~V1+V2+V3+V4+V5 ,data = F_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Rejection.cross.test)
  .errors <-   R2(F_Rejection.cross.test$F_Rejection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  F_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Rejection~V1+V2+V3+V4+V5 ,data = F_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Rejection.cross.train)
  .errors <-   R2(F_Rejection.cross.train$F_Rejection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

#Big5 model with CV, fold=4:

errors <- NULL


for (k in 1:folds){
  F_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Rejection~O+C+E+A+N ,data = F_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Rejection.cross.test)
  .errors <-   R2(F_Rejection.cross.test$F_Rejection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  F_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Rejection~O+C+E+A+N ,data = F_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Rejection.cross.train)
  .errors <-   R2(F_Rejection.cross.train$F_Rejection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  F_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Rejection~PC1+PC2+PC3+PC4+PC5 ,data = F_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Rejection.cross.test)
  .errors <-   R2(F_Rejection.cross.test$F_Rejection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  F_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Rejection~PC1+PC2+PC3+PC4+PC5 ,data = F_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Rejection.cross.train)
  .errors <-   R2(F_Rejection.cross.train$F_Rejection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

##########################################3F_Warmth##########################

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  F_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Warmth.cross.test <- embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Warmth~V1+V2+V3+V4+V5 ,data = F_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Warmth.cross.test)
  .errors <-   R2(F_Warmth.cross.test$F_Warmth, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  F_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Warmth~V1+V2+V3+V4+V5 ,data = F_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Warmth.cross.train)
  .errors <-   R2(F_Warmth.cross.train$F_Warmth, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

#Big5 model with CV, fold=4:

errors <- NULL


for (k in 1:folds){
  F_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Warmth~O+C+E+A+N ,data = F_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Warmth.cross.test)
  .errors <-   R2(F_Warmth.cross.test$F_Warmth, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  F_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Warmth~O+C+E+A+N ,data = F_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Warmth.cross.train)
  .errors <-   R2(F_Warmth.cross.train$F_Warmth, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  F_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Warmth~PC1+PC2+PC3+PC4+PC5 ,data = F_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Warmth.cross.test)
  .errors <-   R2(F_Warmth.cross.test$F_Warmth, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  F_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Warmth~PC1+PC2+PC3+PC4+PC5 ,data = F_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Warmth.cross.train)
  .errors <-   R2(F_Warmth.cross.train$F_Warmth, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

#######################################F_Overprotection###############

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  F_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Overprotection.cross.test <- embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Overprotection~V1+V2+V3+V4+V5 ,data = F_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Overprotection.cross.test)
  .errors <-   R2(F_Overprotection.cross.test$F_Overprotection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  F_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Overprotection~V1+V2+V3+V4+V5 ,data = F_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Overprotection.cross.train)
  .errors <-   R2(F_Overprotection.cross.train$F_Overprotection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

#Big5 model with CV, fold=4:

errors <- NULL


for (k in 1:folds){
  F_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Overprotection~O+C+E+A+N ,data = F_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Overprotection.cross.test)
  .errors <-   R2(F_Overprotection.cross.test$F_Overprotection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  F_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Overprotection~O+C+E+A+N ,data = F_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Overprotection.cross.train)
  .errors <-   R2(F_Overprotection.cross.train$F_Overprotection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  F_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Overprotection~PC1+PC2+PC3+PC4+PC5 ,data = F_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Overprotection.cross.test)
  .errors <-   R2(F_Overprotection.cross.test$F_Overprotection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  F_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  F_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(F_Overprotection~PC1+PC2+PC3+PC4+PC5 ,data = F_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=F_Overprotection.cross.train)
  .errors <-   R2(F_Overprotection.cross.train$F_Overprotection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

################################## M_Rejection#####################

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  M_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Rejection.cross.test <- embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Rejection~V1+V2+V3+V4+V5 ,data = M_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Rejection.cross.test)
  .errors <-   R2(M_Rejection.cross.test$M_Rejection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  M_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Rejection~V1+V2+V3+V4+V5 ,data = M_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Rejection.cross.train)
  .errors <-   R2(M_Rejection.cross.train$M_Rejection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

#Big5 model with CV, fold=4:

errors <- NULL


for (k in 1:folds){
  M_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Rejection~O+C+E+A+N ,data = M_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Rejection.cross.test)
  .errors <-   R2(M_Rejection.cross.test$M_Rejection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  M_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Rejection~O+C+E+A+N ,data = M_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Rejection.cross.train)
  .errors <-   R2(M_Rejection.cross.train$M_Rejection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  M_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Rejection~PC1+PC2+PC3+PC4+PC5 ,data = M_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Rejection.cross.test)
  .errors <-   R2(M_Rejection.cross.test$M_Rejection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  M_Rejection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Rejection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Rejection~PC1+PC2+PC3+PC4+PC5 ,data = M_Rejection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Rejection.cross.train)
  .errors <-   R2(M_Rejection.cross.train$M_Rejection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

##########################################M_Warmth##########################

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  M_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Warmth.cross.test <- embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Warmth~V1+V2+V3+V4+V5 ,data = M_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Warmth.cross.test)
  .errors <-   R2(M_Warmth.cross.test$M_Warmth, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  M_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Warmth~V1+V2+V3+V4+V5 ,data = M_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Warmth.cross.train)
  .errors <-   R2(M_Warmth.cross.train$M_Warmth, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

#Big5 model with CV, fold=4:

errors <- NULL


for (k in 1:folds){
  M_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Warmth~O+C+E+A+N ,data = M_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Warmth.cross.test)
  .errors <-   R2(M_Warmth.cross.test$M_Warmth, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  M_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Warmth~O+C+E+A+N ,data = M_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Warmth.cross.train)
  .errors <-   R2(M_Warmth.cross.train$M_Warmth, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  M_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Warmth~PC1+PC2+PC3+PC4+PC5 ,data = M_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Warmth.cross.test)
  .errors <-   R2(M_Warmth.cross.test$M_Warmth, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  M_Warmth.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Warmth.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Warmth~PC1+PC2+PC3+PC4+PC5 ,data = M_Warmth.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Warmth.cross.train)
  .errors <-   R2(M_Warmth.cross.train$M_Warmth, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

#######################################M_Overprotection###############

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  M_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Overprotection.cross.test <- embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Overprotection~V1+V2+V3+V4+V5 ,data = M_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Overprotection.cross.test)
  .errors <-   R2(M_Overprotection.cross.test$M_Overprotection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  M_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Overprotection~V1+V2+V3+V4+V5 ,data = M_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Overprotection.cross.train)
  .errors <-   R2(M_Overprotection.cross.train$M_Overprotection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

#Big5 model with CV, fold=4:

errors <- NULL


for (k in 1:folds){
  M_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Overprotection~O+C+E+A+N ,data = M_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Overprotection.cross.test)
  .errors <-   R2(M_Overprotection.cross.test$M_Overprotection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  M_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Overprotection~O+C+E+A+N ,data = M_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Overprotection.cross.train)
  .errors <-   R2(M_Overprotection.cross.train$M_Overprotection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  M_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Overprotection~PC1+PC2+PC3+PC4+PC5 ,data = M_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Overprotection.cross.test)
  .errors <-   R2(M_Overprotection.cross.test$M_Overprotection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  M_Overprotection.cross.train <- embu_all[fold.assignment!=k,] # train subset
  M_Overprotection.cross.test <-  embu_all[fold.assignment==k,] # test subset
  .ols <- lm(M_Overprotection~PC1+PC2+PC3+PC4+PC5 ,data = M_Overprotection.cross.train) # train
  .predictions <- predict(.ols, newdata=M_Overprotection.cross.train)
  .errors <-   R2(M_Overprotection.cross.train$M_Overprotection, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

####################combine all:
embu.results$r2<- r2

dv6.results.for <- subset(embu.results, Set == "Test")
sum.embu.results<- embu.results[,.(Mean=mean(r2)), by=.(Type,Set,dv)]

#write.csv(sum.embu.results, 'six.embu.results.csv')
