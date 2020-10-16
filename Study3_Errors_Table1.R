##This file is for the Errors in Study 3 presented in table 1:

#load library:

library(rrpack)
library(data.table)


new.data<- fread("new.mturk.reverse.csv", header = T)
rotation.mat<- fread("rotation_mat.csv", header = T)
pc5 <-fread("pc5.csv", header = T)
new.data.100<- na.omit (new.data)

#scoring the big 5 by aggregation of the 20 related q:

new.data.100$O<- rowMeans(new.data.100[,c("q1","q4","q7","q16","q21","q24",
                                          "q31","q34","q41","q44","q51","q54",
                                          "q61","q64","q71","q74","q81","q84",
                                          "q91","q94")])

new.data.100$C<- rowMeans(new.data.100[,c("q5","q8","q15","q20","q25","q28",
                                          "q35","q38","q45","q48","q55","q58",
                                          "q65","q68","q75","q78","q85","q88",
                                          "q95","q98")])

new.data.100$E<- rowMeans(new.data.100[,c("q3","q10","q14","q18","q23","q29",
                                          "q33","q39","q43","q49","q53","q59",
                                          "q63","q69","q73","q79","q83","q89",
                                          "q93","q99")])

new.data.100$A<- rowMeans(new.data.100[,c("q2","q6","q9","q13","q22","q26",
                                          "q32","q36","q42","q46","q52","q56",
                                          "q62","q66","q72","q76","q82","q86",
                                          "q92","q96")])

new.data.100$N<- rowMeans(new.data.100[,c("q11","q12","q17","q19","q27","q30",
                                          "q37","q40","q47","q50","q57","q60",
                                          "q67","q70","q77","q80","q87","q90",
                                          "q97","q100")])


only.100<- as.matrix(new.data.100[,q1:q100])
new5<- only.100 %*% as.matrix(rotation.mat)
newpc5<- only.100 %*%as.matrix(pc5)
all.new<-  as.data.table(cbind(new.data.100, new5,newpc5))

#sub.new <-all.new[,c('V1','V2','V3','V4','V5','O','C','E','A','N','DV.1','DV.2','DV.3','DV.4',
#                     'DV.5','DV.6','DV.7','DV.8','DV.9','DV.10','DV.11','DV.12')]

#cor.mat <- cor(sub.new)

##############################################################################

set.seed(1)
folds <-4
fold.assignment <- sample(1:folds, nrow(all.new), replace = TRUE)

r2.12<-NULL
sd.12<-NULL

########################################d.v.1#########################
#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv.1.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.1.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.1~V1+V2+V3+V4+V5 ,data = dv.1.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.1.cross.test)
  .errors <-   (dv.1.cross.test$DV.1) -  (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pf.dv.1<- errors

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv.1.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.1.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.1~O+C+E+A+N ,data = dv.1.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.1.cross.test)
  .errors <-   (dv.1.cross.test$DV.1)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


errors.big5.dv.1<- errors

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv.1.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.1.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.1~PC1+PC2+PC3+PC4+PC5 ,data = dv.1.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.1.cross.test)
  .errors <-   (dv.1.cross.test$DV.1)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.dv.1<- errors

mean(abs(errors.pf.dv.1))
mean(abs(errors.big5.dv.1))
mean(abs(errors.pc5.dv.1))

errors.dv.1<- data.frame(abs(errors.pc5.dv.1), abs(errors.pf.dv.1),abs(errors.big5.dv.1))

#write.csv(errors.dv.1,'errors.dv.1.csv')


########################################d.v.2#########################
#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv.2.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.2.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.2~V1+V2+V3+V4+V5 ,data = dv.2.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.2.cross.test)
  .errors <-   (dv.2.cross.test$DV.2) -  (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pf.dv.2<- errors

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv.2.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.2.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.2~O+C+E+A+N ,data = dv.2.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.2.cross.test)
  .errors <-   (dv.2.cross.test$DV.2)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


errors.big5.dv.2<- errors

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv.2.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.2.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.2~PC1+PC2+PC3+PC4+PC5 ,data = dv.2.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.2.cross.test)
  .errors <-   (dv.2.cross.test$DV.2)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.dv.2<- errors

mean(abs(errors.pf.dv.2))
mean(abs(errors.big5.dv.2))
mean(abs(errors.pc5.dv.2))

errors.dv.2<- data.frame(abs(errors.pc5.dv.2), abs(errors.pf.dv.2),abs(errors.big5.dv.2))

#write.csv(errors.dv.2,'errors.dv.2.csv')



########################################d.v.3#########################
#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv.3.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.3.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.3~V1+V2+V3+V4+V5 ,data = dv.3.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.3.cross.test)
  .errors <-   (dv.3.cross.test$DV.3) -  (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pf.dv.3<- errors

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv.3.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.3.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.3~O+C+E+A+N ,data = dv.3.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.3.cross.test)
  .errors <-   (dv.3.cross.test$DV.3)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


errors.big5.dv.3<- errors

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv.3.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.3.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.3~PC1+PC2+PC3+PC4+PC5 ,data = dv.3.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.3.cross.test)
  .errors <-   (dv.3.cross.test$DV.3)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.dv.3<- errors

mean(abs(errors.pf.dv.3))
mean(abs(errors.big5.dv.3))
mean(abs(errors.pc5.dv.3))

errors.dv.3<- data.frame(abs(errors.pc5.dv.3), abs(errors.pf.dv.3),abs(errors.big5.dv.3))

#write.csv(errors.dv.3,'errors.dv.3.csv')



########################################d.v.4#########################
#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv.4.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.4.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.4~V1+V2+V3+V4+V5 ,data = dv.4.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.4.cross.test)
  .errors <-   (dv.4.cross.test$DV.4) -  (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pf.dv.4<- errors

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv.4.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.4.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.4~O+C+E+A+N ,data = dv.4.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.4.cross.test)
  .errors <-   (dv.4.cross.test$DV.4)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


errors.big5.dv.4<- errors

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv.4.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.4.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.4~PC1+PC2+PC3+PC4+PC5 ,data = dv.4.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.4.cross.test)
  .errors <-   (dv.4.cross.test$DV.4)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.dv.4<- errors

mean(abs(errors.pf.dv.4))
mean(abs(errors.big5.dv.4))
mean(abs(errors.pc5.dv.4))

errors.dv.4<- data.frame(abs(errors.pc5.dv.4), abs(errors.pf.dv.4),abs(errors.big5.dv.4))

#write.csv(errors.dv.4,'errors.dv.4.csv')



########################################d.v.5#########################
#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv.5.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.5.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.5~V1+V2+V3+V4+V5 ,data = dv.5.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.5.cross.test)
  .errors <-   (dv.5.cross.test$DV.5) -  (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pf.dv.5<- errors

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv.5.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.5.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.5~O+C+E+A+N ,data = dv.5.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.5.cross.test)
  .errors <-   (dv.5.cross.test$DV.5)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


errors.big5.dv.5<- errors

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv.5.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.5.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.5~PC1+PC2+PC3+PC4+PC5 ,data = dv.5.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.5.cross.test)
  .errors <-   (dv.5.cross.test$DV.5)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.dv.5<- errors

mean(abs(errors.pf.dv.5))
mean(abs(errors.big5.dv.5))
mean(abs(errors.pc5.dv.5))

errors.dv.5<- data.frame(abs(errors.pc5.dv.5), abs(errors.pf.dv.5),abs(errors.big5.dv.5))

#write.csv(errors.dv.5,'errors.dv.5.csv')

########################################d.v.6#########################
#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv.6.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.6.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.6~V1+V2+V3+V4+V5 ,data = dv.6.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.6.cross.test)
  .errors <-   (dv.6.cross.test$DV.6) -  (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pf.dv.6<- errors

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv.6.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.6.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.6~O+C+E+A+N ,data = dv.6.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.6.cross.test)
  .errors <-   (dv.6.cross.test$DV.6)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


errors.big5.dv.6<- errors

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv.6.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.6.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.6~PC1+PC2+PC3+PC4+PC5 ,data = dv.6.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.6.cross.test)
  .errors <-   (dv.6.cross.test$DV.6)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.dv.6<- errors

mean(abs(errors.pf.dv.6))
mean(abs(errors.big5.dv.6))
mean(abs(errors.pc5.dv.6))

errors.dv.6<- data.frame(abs(errors.pc5.dv.6), abs(errors.pf.dv.6),abs(errors.big5.dv.6))

#write.csv(errors.dv.6,'errors.dv.6.csv')

########################################d.v.7#########################

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv.7.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.7.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.7~V1+V2+V3+V4+V5 ,data = dv.7.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.7.cross.test)
  .errors <-   (dv.7.cross.test$DV.7) -  (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pf.dv.7<- errors

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv.7.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.7.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.7~O+C+E+A+N ,data = dv.7.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.7.cross.test)
  .errors <-   (dv.7.cross.test$DV.7)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


errors.big5.dv.7<- errors

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv.7.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.7.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.7~PC1+PC2+PC3+PC4+PC5 ,data = dv.7.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.7.cross.test)
  .errors <-   (dv.7.cross.test$DV.7)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.dv.7<- errors

mean(abs(errors.pf.dv.7))
mean(abs(errors.big5.dv.7))
mean(abs(errors.pc5.dv.7))

errors.dv.7<- data.frame(abs(errors.pc5.dv.7), abs(errors.pf.dv.7),abs(errors.big5.dv.7))

#write.csv(errors.dv.7,'errors.dv.7.csv')

########################################d.v.8#########################

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv.8.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.8.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.8~V1+V2+V3+V4+V5 ,data = dv.8.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.8.cross.test)
  .errors <-   (dv.8.cross.test$DV.8) -  (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pf.dv.8<- errors

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv.8.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.8.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.8~O+C+E+A+N ,data = dv.8.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.8.cross.test)
  .errors <-   (dv.8.cross.test$DV.8)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


errors.big5.dv.8<- errors

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv.8.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.8.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.8~PC1+PC2+PC3+PC4+PC5 ,data = dv.8.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.8.cross.test)
  .errors <-   (dv.8.cross.test$DV.8)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.dv.8<- errors

mean(abs(errors.pf.dv.8))
mean(abs(errors.big5.dv.8))
mean(abs(errors.pc5.dv.8))

errors.dv.8<- data.frame(abs(errors.pc5.dv.8), abs(errors.pf.dv.8),abs(errors.big5.dv.8))

#write.csv(errors.dv.8,'errors.dv.8.csv')

########################################d.v.9#########################

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv.9.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.9.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.9~V1+V2+V3+V4+V5 ,data = dv.9.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.9.cross.test)
  .errors <-   (dv.9.cross.test$DV.9) -  (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pf.dv.9<- errors

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv.9.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.9.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.9~O+C+E+A+N ,data = dv.9.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.9.cross.test)
  .errors <-   (dv.9.cross.test$DV.9)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


errors.big5.dv.9<- errors

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv.9.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.9.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.9~PC1+PC2+PC3+PC4+PC5 ,data = dv.9.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.9.cross.test)
  .errors <-   (dv.9.cross.test$DV.9)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.dv.9<- errors

mean(abs(errors.pf.dv.9))
mean(abs(errors.big5.dv.9))
mean(abs(errors.pc5.dv.9))

errors.dv.9<- data.frame(abs(errors.pc5.dv.9), abs(errors.pf.dv.9),abs(errors.big5.dv.9))

#write.csv(errors.dv.9,'errors.dv.9.csv')

########################################d.v.10#########################
#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv.10.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.10.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.10~V1+V2+V3+V4+V5 ,data = dv.10.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.10.cross.test)
  .errors <-   (dv.10.cross.test$DV.10) -  (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pf.dv.10<- errors

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv.10.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.10.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.10~O+C+E+A+N ,data = dv.10.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.10.cross.test)
  .errors <-   (dv.10.cross.test$DV.10)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


errors.big5.dv.10<- errors

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv.10.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.10.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.10~PC1+PC2+PC3+PC4+PC5 ,data = dv.10.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.10.cross.test)
  .errors <-   (dv.10.cross.test$DV.10)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.dv.10<- errors

mean(abs(errors.pf.dv.10))
mean(abs(errors.big5.dv.10))
mean(abs(errors.pc5.dv.10))

errors.dv.10<- data.frame(abs(errors.pc5.dv.10), abs(errors.pf.dv.10),abs(errors.big5.dv.10))

#write.csv(errors.dv.10,'errors.dv.10.csv')

########################################d.v.11#########################
#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv.11.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.11.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.11~V1+V2+V3+V4+V5 ,data = dv.11.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.11.cross.test)
  .errors <-   (dv.11.cross.test$DV.11) -  (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pf.dv.11<- errors

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv.11.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.11.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.11~O+C+E+A+N ,data = dv.11.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.11.cross.test)
  .errors <-   (dv.11.cross.test$DV.11)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


errors.big5.dv.11<- errors

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv.11.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.11.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.11~PC1+PC2+PC3+PC4+PC5 ,data = dv.11.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.11.cross.test)
  .errors <-   (dv.11.cross.test$DV.11)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.dv.11<- errors

mean(abs(errors.pf.dv.11))
mean(abs(errors.big5.dv.11))
mean(abs(errors.pc5.dv.11))

errors.dv.11<- data.frame(abs(errors.pc5.dv.11), abs(errors.pf.dv.11),abs(errors.big5.dv.11))

#write.csv(errors.dv.11,'errors.dv.11.csv')

########################################d.v.12#########################

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv.12.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.12.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.12~V1+V2+V3+V4+V5 ,data = dv.12.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.12.cross.test)
  .errors <-   (dv.12.cross.test$DV.12) -  (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pf.dv.12<- errors

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv.12.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.12.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.12~O+C+E+A+N ,data = dv.12.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.12.cross.test)
  .errors <-   (dv.12.cross.test$DV.12)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


errors.big5.dv.12<- errors

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv.12.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv.12.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.12~PC1+PC2+PC3+PC4+PC5 ,data = dv.12.cross.train) # train
  .predictions <- predict(.ols, newdata=dv.12.cross.test)
  .errors <-   (dv.12.cross.test$DV.12)- (.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

errors.pc5.dv.12<- errors

mean(abs(errors.pf.dv.12))
mean(abs(errors.big5.dv.12))
mean(abs(errors.pc5.dv.12))

errors.dv.12<- data.frame(abs(errors.pc5.dv.12), abs(errors.pf.dv.12),abs(errors.big5.dv.12))

#write.csv(errors.dv.12,'errors.dv.12.csv')



