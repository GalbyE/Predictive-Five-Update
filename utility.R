message('Make sure to setwd() to the scripts location.')

library(plyr)
library(dplyr)
library(rrpack)
library(data.table)


R2 <- function(y, y.hat){
  numerator <- sum((y-y.hat)^2) 
  denominator <- sum((y-mean(y))^2) 
  1-numerator/denominator
}
