
##This file is for the Errors in Study 1:

source('utility.R')



#reading the test files:
test.iq<- fread("Data/Study1_test_data/test.iq.csv", header = T)
test.swl<- fread("Data/Study1_test_data/test.swl.csv", header = T)
test.cesd<- fread("Data/Study1_test_data/test.cesd.csv", header = T)
test.liberal<- fread("Data/Study1_test_data/test.liberal.csv", header = T)
test.risky<- fread("Data/Study1_test_data/test.risky.csv", header = T)
test.network<- fread("Data/Study1_test_data/test.network.csv", header = T)
test.nonhealthy<- fread("Data/Study1_test_data/test.nonhealthy.csv", header = T)
test.svq.trans<- fread("Data/Study1_test_data/test.svq.trans.csv", header = T)
test.svq.openness<- fread("Data/Study1_test_data/test.svq.openness.csv", header = T)
test.empathy<- fread("Data/Study1_test_data/test.empathy.csv", header = T)


#Load the data:
load('Data/xy.Rdata')

#scaling the data:

y.train.scale<- scale(y.train,center=T, scale=F)
x.train.scale<- scale(x.train,center=T, scale=F)

#RRR with RRpack:

rrpack.model<- rrr.fit(y.train.scale,
                       x.train.scale,
                       nrank = 5, 
                       coefSVD = T)

#Extracting the rotation matrix from the model:

rotation.mat <- as.matrix(rrpack.model$coef.ls %*% rrpack.model$A)
#write.csv(rotation.mat,'rotation_mat.csv',  row.names=FALSE)
# You can find this matrix as rotation.mat.csv


##create the PCA object and the PC5:####

dim(x.train)
PC <-prcomp(x.train)
summary(PC)
pc5<-as.matrix(PC$rotation[,1:5])
#write.csv(pc5,'pc5.csv',  row.names=FALSE)


#################################IQ#############################

iq.100<- as.matrix(test.iq[,q1:q100])
dim(iq.100)

iq.new5<- iq.100%*% rotation.mat
iq.pca<- iq.100%*%pc5
iq.new5.all<- as.data.table(cbind(test.iq, iq.new5,iq.pca))

set.seed(10)
folds <- 4
fold.assignment <- sample(1:folds, nrow(iq.new5.all), replace = TRUE)

#New 5 model with CV, fold=4:
errors <- NULL
real<- NULL

for (k in 1:folds){
  iq.cross.train <- iq.new5.all[fold.assignment!=k,] # train subset
  iq.cross.test <-  iq.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(iq~V1+V2+V3+V4+V5  ,data = iq.cross.train) # train
  .predictions <- predict(.ols, newdata=iq.cross.test)
   real <- c(real,iq.cross.test$iq)
  .errors <-   (iq.cross.test$iq)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pf<- real
errors.pf.iq<- errors

#Big5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  iq.cross.train <- iq.new5.all[fold.assignment!=k,] # train subset
  iq.cross.test <-  iq.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(iq~O+C+E+A+N ,data = iq.cross.train) # train
  .predictions <- predict(.ols, newdata=iq.cross.test)
  real <- c(real,iq.cross.test$iq)
  .errors <-   (iq.cross.test$iq)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.big5<- real
errors.big5.iq<- errors

#PC5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  iq.cross.train <- iq.new5.all[fold.assignment!=k,] # train subset
  iq.cross.test <-  iq.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(iq~PC1+PC2+PC3+PC4+PC5 ,data = iq.cross.train) # train
  .predictions <- predict(.ols, newdata=iq.cross.test)
   real <- c(real,iq.cross.test$iq)
  .errors <-  (iq.cross.test$iq)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pc5<- real
errors.pc5.iq<- errors

errors.iq<- data.frame(abs(errors.pc5.iq), abs(errors.pf.iq),abs(errors.big5.iq))

#write.csv(errors.iq,'errors.iq.csv')
mean(abs(errors.pf.iq))


#################################SWL#############################

swl.100<- as.matrix(test.swl[,q1:q100])
dim(swl.100)

swl.new5<- swl.100%*% rotation.mat
swl.pca<- swl.100%*%pc5
swl.new5.all<- as.data.table(cbind(test.swl, swl.new5,swl.pca ))

dim(swl.new5.all)
names(swl.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(1:folds, nrow(swl.new5.all), replace = TRUE)

#New 5 model with CV, fold=4:
errors <- NULL
real<- NULL

for (k in 1:folds){
  swl.cross.train <- swl.new5.all[fold.assignment!=k,] # train subset
  swl.cross.test <-  swl.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(swl~V1+V2+V3+V4+V5  ,data = swl.cross.train) # train
  .predictions <- predict(.ols, newdata=swl.cross.test)
  real <- c(real,swl.cross.test$swl)
  .errors <-   (swl.cross.test$swl)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pf<- real
errors.pf.swl<- errors

#Big5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  swl.cross.train <- swl.new5.all[fold.assignment!=k,] # train subset
  swl.cross.test <-  swl.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(swl~O+C+E+A+N ,data = swl.cross.train) # train
  .predictions <- predict(.ols, newdata=swl.cross.test)
  real <- c(real,swl.cross.test$swl)
  .errors <-   (swl.cross.test$swl)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.big5<- real
errors.big5.swl<- errors

#PC5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  swl.cross.train <- swl.new5.all[fold.assignment!=k,] # train subset
  swl.cross.test <-  swl.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(swl~PC1+PC2+PC3+PC4+PC5 ,data = swl.cross.train) # train
  .predictions <- predict(.ols, newdata=swl.cross.test)
  real <- c(real,swl.cross.test$swl)
  .errors <-  (swl.cross.test$swl)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pc5<- real
errors.pc5.swl<- errors

errors.swl<- data.frame(abs(errors.pc5.swl), abs(errors.pf.swl),abs(errors.big5.swl))

#write.csv(errors.swl,'errors.swl.csv')
mean(abs(errors.pc5.swl))
mean(abs(errors.pf.swl))
mean(abs(errors.big5.swl))

#########################CESD############################

cesd.100<- as.matrix(test.cesd[,q1:q100])
dim(cesd.100)

cesd.new5<- cesd.100%*% rotation.mat
cesd.pca<- cesd.100%*%pc5
cesd.new5.all<- as.data.table(cbind(test.cesd, cesd.new5,cesd.pca ))

set.seed(1)
folds <- 4
fold.assignment <- sample(1:folds, nrow(cesd.new5.all), replace = TRUE)

#New 5 model with CV, fold=4:
errors <- NULL
real<- NULL

for (k in 1:folds){
  score.cesd.cross.train <- cesd.new5.all[fold.assignment!=k,] # train subset
  score.cesd.cross.test <-  cesd.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(score.cesd~V1+V2+V3+V4+V5  ,data = score.cesd.cross.train) # train
  .predictions <- predict(.ols, newdata=score.cesd.cross.test)
  real <- c(real,score.cesd.cross.test$score.cesd)
  .errors <-   (score.cesd.cross.test$score.cesd)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pf<- real
errors.pf.cesd<- errors

#Big5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  score.cesd.cross.train <- cesd.new5.all[fold.assignment!=k,] # train subset
  score.cesd.cross.test <-  cesd.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(score.cesd~O+C+E+A+N ,data = score.cesd.cross.train) # train
  .predictions <- predict(.ols, newdata=score.cesd.cross.test)
  real <- c(real,score.cesd.cross.test$score.cesd)
  .errors <-   (score.cesd.cross.test$score.cesd)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.big5<- real
errors.big5.cesd<- errors

#PC5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  score.cesd.cross.train <- cesd.new5.all[fold.assignment!=k,] # train subset
  score.cesd.cross.test <-  cesd.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(score.cesd~PC1+PC2+PC3+PC4+PC5 ,data = score.cesd.cross.train) # train
  .predictions <- predict(.ols, newdata=score.cesd.cross.test)
  real <- c(real,score.cesd.cross.test$score.cesd)
  .errors <-  (score.cesd.cross.test$score.cesd)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pc5<- real
errors.pc5.cesd<- errors

errors.cesd<- data.frame(abs(errors.pc5.cesd), abs(errors.pf.cesd),abs(errors.big5.cesd))
errors2.cesd<- data.frame((errors.pc5.cesd)^2, (errors.pf.cesd)^2,(errors.big5.cesd)^2)

#write.csv(errors2.cesd,'errors_2.cesd.csv')

#write.csv(errors.cesd,'errors.cesd.csv')

mean(abs(errors.pc5.cesd))
mean(abs(errors.pf.cesd))
mean(abs(errors.big5.cesd))

###########################LIBERAL##############################

liberal.100<- as.matrix(test.liberal[,q1:q100])
dim(liberal.100)

liberal.new5<- liberal.100%*% rotation.mat
liberal.pca<- liberal.100%*%pc5
liberal.new5.all<- as.data.table(cbind(test.liberal, liberal.new5,liberal.pca ))

dim(liberal.new5.all)
names(liberal.new5.all)


set.seed(1)
folds <- 4
fold.assignment <- sample(
                   1:folds, nrow(liberal.new5.all), replace = TRUE)


#New 5 model with CV, fold=4:
errors <- NULL
real<- NULL

for (k in 1:folds){
  liberal.cross.train <- liberal.new5.all[fold.assignment!=k,] # train subset
  liberal.cross.test <-  liberal.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(liberalness~V1+V2+V3+V4+V5  ,data = liberal.cross.train) # train
  .predictions <- predict(.ols, newdata=liberal.cross.test)
  real <- c(real,liberal.cross.test$liberal)
  .errors <-   (liberal.cross.test$liberal)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pf<- real
errors.pf.liberal<- errors

#Big5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  liberal.cross.train <- liberal.new5.all[fold.assignment!=k,] # train subset
  liberal.cross.test <-  liberal.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(liberalness~O+C+E+A+N ,data = liberal.cross.train) # train
  .predictions <- predict(.ols, newdata=liberal.cross.test)
  real <- c(real,liberal.cross.test$liberal)
  .errors <-   (liberal.cross.test$liberal)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.big5<- real
errors.big5.liberal<- errors

#PC5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  liberal.cross.train <- liberal.new5.all[fold.assignment!=k,] # train subset
  liberal.cross.test <-  liberal.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(liberalness~PC1+PC2+PC3+PC4+PC5 ,data = liberal.cross.train) # train
  .predictions <- predict(.ols, newdata=liberal.cross.test)
  real <- c(real,liberal.cross.test$liberal)
  .errors <-  (liberal.cross.test$liberal)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pc5<- real
errors.pc5.liberal<- errors

errors.liberal<- data.frame(abs(errors.pc5.liberal), abs(errors.pf.liberal),abs(errors.big5.liberal))

#write.csv(errors.liberal,'errors.liberal.csv')
mean(abs(errors.pc5.liberal))
mean(abs(errors.pf.liberal))
mean(abs(errors.big5.liberal))

##################################RISKY##################################


risky.100<- as.matrix(test.risky[,q1:q100])
dim(risky.100)

risky.new5<- risky.100%*% rotation.mat
risky.pca<- risky.100%*%pc5
risky.new5.all<- as.data.table(cbind(test.risky, risky.new5,risky.pca ))

dim(risky.new5.all)
names(risky.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(
                    1:folds, nrow(risky.new5.all), replace = TRUE)

#New 5 model with CV, fold=4:

errors <- NULL
real<- NULL

for (k in 1:folds){
  risky.cross.train <- risky.new5.all[fold.assignment!=k,] # train subset
  risky.cross.test <-  risky.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(risky~V1+V2+V3+V4+V5  ,data = risky.cross.train) # train
  .predictions <- predict(.ols, newdata=risky.cross.test)
  real <- c(real,risky.cross.test$risky)
  .errors <-   (risky.cross.test$risky)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pf<- real
errors.pf.risky<- errors

#Big5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  risky.cross.train <- risky.new5.all[fold.assignment!=k,] # train subset
  risky.cross.test <-  risky.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(risky~O+C+E+A+N ,data = risky.cross.train) # train
  .predictions <- predict(.ols, newdata=risky.cross.test)
  real <- c(real,risky.cross.test$risky)
  .errors <-   (risky.cross.test$risky)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.big5<- real
errors.big5.risky<- errors

#PC5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  risky.cross.train <- risky.new5.all[fold.assignment!=k,] # train subset
  risky.cross.test <-  risky.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(risky~PC1+PC2+PC3+PC4+PC5 ,data = risky.cross.train) # train
  .predictions <- predict(.ols, newdata=risky.cross.test)
  real <- c(real,risky.cross.test$risky)
  .errors <-  (risky.cross.test$risky)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pc5<- real
errors.pc5.risky<- errors

errors.risky<- data.frame(abs(errors.pc5.risky), abs(errors.pf.risky),abs(errors.big5.risky))

#write.csv(errors.risky,'errors.risky.csv')
mean(abs(errors.pc5.risky))
mean(abs(errors.pf.risky))
mean(abs(errors.big5.risky))
###################################network################################

network.100<- as.matrix(test.network[,q1:q100])
dim(network.100)

network.new5<- network.100%*% rotation.mat
network.pca<- network.100%*%pc5
network.new5.all<- as.data.table(cbind(test.network, network.new5,network.pca ))
dim(network.new5.all)
names(network.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(
            1:folds, nrow(network.new5.all), replace = TRUE)

#New 5 model with CV, fold=4:

errors <- NULL
real<- NULL

for (k in 1:folds){
  network.cross.train <- network.new5.all[fold.assignment!=k,] # train subset
  network.cross.test <-  network.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(network_size~V1+V2+V3+V4+V5  ,data = network.cross.train) # train
  .predictions <- predict(.ols, newdata=network.cross.test)
  real <- c(real,network.cross.test$network_size)
  .errors <-   (network.cross.test$network_size)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pf<- real
errors.pf.network<- errors

#Big5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  network.cross.train <- network.new5.all[fold.assignment!=k,] # train subset
  network.cross.test <-  network.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(network_size~O+C+E+A+N ,data = network.cross.train) # train
  .predictions <- predict(.ols, newdata=network.cross.test)
  real <- c(real,network.cross.test$network_size)
  .errors <-   (network.cross.test$network_size)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.big5<- real
errors.big5.network<- errors

#PC5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  network.cross.train <- network.new5.all[fold.assignment!=k,] # train subset
  network.cross.test <-  network.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(network_size~PC1+PC2+PC3+PC4+PC5 ,data = network.cross.train) # train
  .predictions <- predict(.ols, newdata=network.cross.test)
  real <- c(real,network.cross.test$network_size)
  .errors <-  (network.cross.test$network_size)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pc5<- real
errors.pc5.network<- errors

errors.network<- data.frame(abs(errors.pc5.network), abs(errors.pf.network),abs(errors.big5.network))

#write.csv(errors.network,'errors.network.csv')
mean(abs(errors.pc5.network))
mean(abs(errors.pf.network))
mean(abs(errors.big5.network))

###################################nonhealthy################################

nonhealthy.100<- as.matrix(test.nonhealthy[,q1:q100])
dim(nonhealthy.100)

nonhealthy.new5<- nonhealthy.100%*% rotation.mat
nonhealthy.pca<- nonhealthy.100%*%pc5
nonhealthy.new5.all<- as.data.table(cbind(test.nonhealthy, nonhealthy.new5,nonhealthy.pca ))

dim(nonhealthy.new5.all)
names(nonhealthy.new5.all)

set.seed(4)
folds <- 4
fold.assignment <- sample(
  1:folds, nrow(nonhealthy.new5.all), replace = TRUE)

#New 5 model with CV, fold=4:
errors <- NULL
real<- NULL

for (k in 1:folds){
  nonhealthy.cross.train <- nonhealthy.new5.all[fold.assignment!=k,] # train subset
  nonhealthy.cross.test <-  nonhealthy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(nonhealthy~V1+V2+V3+V4+V5  ,data = nonhealthy.cross.train) # train
  .predictions <- predict(.ols, newdata=nonhealthy.cross.test)
  real <- c(real,nonhealthy.cross.test$nonhealthy)
  .errors <-   (nonhealthy.cross.test$nonhealthy)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pf<- real
errors.pf.nonhealthy<- errors

#Big5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  nonhealthy.cross.train <- nonhealthy.new5.all[fold.assignment!=k,] # train subset
  nonhealthy.cross.test <-  nonhealthy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(nonhealthy~O+C+E+A+N ,data = nonhealthy.cross.train) # train
  .predictions <- predict(.ols, newdata=nonhealthy.cross.test)
  real <- c(real,nonhealthy.cross.test$nonhealthy)
  .errors <-   (nonhealthy.cross.test$nonhealthy)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.big5<- real
errors.big5.nonhealthy<- errors

#PC5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  nonhealthy.cross.train <- nonhealthy.new5.all[fold.assignment!=k,] # train subset
  nonhealthy.cross.test <-  nonhealthy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(nonhealthy~PC1+PC2+PC3+PC4+PC5 ,data = nonhealthy.cross.train) # train
  .predictions <- predict(.ols, newdata=nonhealthy.cross.test)
  real <- c(real,nonhealthy.cross.test$nonhealthy)
  .errors <-  (nonhealthy.cross.test$nonhealthy)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pc5<- real
errors.pc5.nonhealthy<- errors

errors.nonhealthy<- data.frame(abs(errors.pc5.nonhealthy), abs(errors.pf.nonhealthy),abs(errors.big5.nonhealthy))
#write.csv(errors.nonhealthy,'errors.nonhealthy.csv')

mean(abs(errors.pf.nonhealthy))
mean(abs(errors.big5.nonhealthy))
mean(abs(errors.pc5.nonhealthy))
###################################svq.trans################################

svq.trans.100<- as.matrix(test.svq.trans[,q1:q100])
dim(svq.trans.100)

svq.trans.new5<- svq.trans.100%*% rotation.mat
svq.trans.pca<- svq.trans.100%*%pc5
svq.trans.new5.all<- as.data.table(cbind(test.svq.trans, svq.trans.new5,svq.trans.pca ))

dim(svq.trans.new5.all)
names(svq.trans.new5.all)

set.seed(246)
folds <- 4
fold.assignment <- sample(
  1:folds, nrow(svq.trans.new5.all), replace = TRUE)

#New 5 model with CV, fold=4:

errors <- NULL
real<- NULL

for (k in 1:folds){
  svq.trans.cross.train <- svq.trans.new5.all[fold.assignment!=k,] # train subset
  svq.trans.cross.test <-  svq.trans.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.trans~V1+V2+V3+V4+V5  ,data = svq.trans.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.trans.cross.test)
  real <- c(real,svq.trans.cross.test$sv.trans)
  .errors <-   (svq.trans.cross.test$sv.trans)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pf<- real
errors.pf.svq.trans<- errors

#Big5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  svq.trans.cross.train <- svq.trans.new5.all[fold.assignment!=k,] # train subset
  svq.trans.cross.test <-  svq.trans.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.trans~O+C+E+A+N ,data = svq.trans.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.trans.cross.test)
  real <- c(real,svq.trans.cross.test$sv.trans)
  .errors <-   (svq.trans.cross.test$sv.trans)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.big5<- real
errors.big5.svq.trans<- errors

#PC5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  svq.trans.cross.train <- svq.trans.new5.all[fold.assignment!=k,] # train subset
  svq.trans.cross.test <-  svq.trans.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.trans~PC1+PC2+PC3+PC4+PC5 ,data = svq.trans.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.trans.cross.test)
  real <- c(real,svq.trans.cross.test$sv.trans)
  .errors <-  (svq.trans.cross.test$sv.trans)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pc5<- real
errors.pc5.svq.trans<- errors
mean(abs(errors.pf.svq.trans))
mean(abs(errors.big5.svq.trans))
mean(abs(errors.pc5.svq.trans))

errors.svq.trans<- data.frame(abs(errors.pc5.svq.trans), abs(errors.pf.svq.trans),abs(errors.big5.svq.trans))

#write.csv(errors.svq.trans,'errors.svq.trans.csv')

###################################svq.openness################################

svq.openness.100<- as.matrix(test.svq.openness[,q1:q100])
dim(svq.openness.100)

svq.openness.new5<- svq.openness.100%*% rotation.mat
svq.openness.pca<- svq.openness.100%*%pc5
svq.openness.new5.all<- as.data.table(cbind(test.svq.openness, svq.openness.new5,svq.openness.pca ))

dim(svq.openness.new5.all)
names(svq.openness.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(
             1:folds, nrow(svq.openness.new5.all), replace = TRUE)

#New 5 model with CV, fold=4:

errors <- NULL
real<- NULL

for (k in 1:folds){
  svq.openness.cross.train <- svq.openness.new5.all[fold.assignment!=k,] # train subset
  svq.openness.cross.test <-  svq.openness.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.openness~V1+V2+V3+V4+V5  ,data = svq.openness.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.openness.cross.test)
  real <- c(real,svq.openness.cross.test$sv.openness)
  .errors <-   (svq.openness.cross.test$sv.openness)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pf<- real
errors.pf.svq.openness<- errors

#Big5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  svq.openness.cross.train <- svq.openness.new5.all[fold.assignment!=k,] # train subset
  svq.openness.cross.test <-  svq.openness.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.openness~O+C+E+A+N ,data = svq.openness.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.openness.cross.test)
  real <- c(real,svq.openness.cross.test$sv.openness)
  .errors <-   (svq.openness.cross.test$sv.openness)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.big5<- real
errors.big5.svq.openness<- errors

#PC5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  svq.openness.cross.train <- svq.openness.new5.all[fold.assignment!=k,] # train subset
  svq.openness.cross.test <-  svq.openness.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.openness~PC1+PC2+PC3+PC4+PC5 ,data = svq.openness.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.openness.cross.test)
  real <- c(real,svq.openness.cross.test$sv.openness)
  .errors <-  (svq.openness.cross.test$sv.openness)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pc5<- real
errors.pc5.svq.openness<- errors
mean(abs(errors.pf.svq.openness))
mean(abs(errors.big5.svq.openness))
mean(abs(errors.pc5.svq.openness))

errors.svq.openness<- data.frame(abs(errors.pc5.svq.openness), abs(errors.pf.svq.openness),abs(errors.big5.svq.openness))

#write.csv(errors.svq.openness,'errors.svq.openness.csv')

###################################empathy################################

empathy.100<- as.matrix(test.empathy[,q1:q100])
dim(empathy.100)

empathy.new5<- empathy.100%*% rotation.mat
empathy.pca<- empathy.100%*%pc5
empathy.new5.all<- as.data.table(cbind(test.empathy, empathy.new5,empathy.pca ))
dim(empathy.new5.all)
names(empathy.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(
                  1:folds, nrow(empathy.new5.all), replace = TRUE)

#New 5 model with CV, fold=4:
errors <- NULL
real<- NULL

for (k in 1:folds){
  empathy.cross.train <- empathy.new5.all[fold.assignment!=k,] # train subset
  empathy.cross.test <-  empathy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(empathy~V1+V2+V3+V4+V5  ,data = empathy.cross.train) # train
  .predictions <- predict(.ols, newdata=empathy.cross.test)
  real <- c(real,empathy.cross.test$empathy)
  .errors <-   (empathy.cross.test$empathy)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pf<- real
errors.pf.empathy<- errors

#Big5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  empathy.cross.train <- empathy.new5.all[fold.assignment!=k,] # train subset
  empathy.cross.test <-  empathy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(empathy~O+C+E+A+N ,data = empathy.cross.train) # train
  .predictions <- predict(.ols, newdata=empathy.cross.test)
  real <- c(real,empathy.cross.test$empathy)
  .errors <-   (empathy.cross.test$empathy)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.big5<- real
errors.big5.empathy<- errors

#PC5 model with CV, fold=4:

errors <- NULL
real <- NULL

for (k in 1:folds){
  empathy.cross.train <- empathy.new5.all[fold.assignment!=k,] # train subset
  empathy.cross.test <-  empathy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(empathy~PC1+PC2+PC3+PC4+PC5 ,data = empathy.cross.train) # train
  .predictions <- predict(.ols, newdata=empathy.cross.test)
  real <- c(real,empathy.cross.test$empathy)
  .errors <-  (empathy.cross.test$empathy)-(.predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

real.pc5<- real
errors.pc5.empathy<- errors

errors.empathy<- data.frame(abs(errors.pc5.empathy), abs(errors.pf.empathy),abs(errors.big5.empathy))

#write.csv(errors.empathy,'errors.empathy.csv')
mean(abs(errors.pf.empathy))
mean(abs(errors.big5.empathy))
mean(abs(errors.pc5.empathy))
