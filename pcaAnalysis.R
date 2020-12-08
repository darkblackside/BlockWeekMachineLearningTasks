library(readr)
library(dplyr)
library(ggplot2)
#PCA analysis
data <- read_delim("../09_PCA_Data_v01.csv", ";", escape_double = FALSE, trim_ws = TRUE)

covMatrix <- cov(data)

var10 <- var(data$x)
var01 <- var(data$y)

dataChanged <- data %>% apply(1, function(oneRow) {
  vector <- c(oneRow["x"], oneRow["y"])
  winkel <- -pi/4
  change <- matrix(c(cos(winkel), -sin(winkel), sin(winkel), cos(winkel)), ncol=2, nrow=2)
  
  vector %*% change
})

scalar1 <- function(x) {x / sqrt(sum(x^2))}

unitVector11 <- scalar1(c(1, 1))
unitVector21 <- scalar1(c(2, 1))
unitVector12 <- scalar1(c(1, 2))

ev <- eigen(covMatrix)
ev1 <- ev$vectors[,1]
ev2 <- ev$vectors[,2]

#plot(data$x, data$y)
#plot(dataChanged[1,], dataChanged[2,])

var11x <- var(dataChanged[1,])
var11 <- t(unitVector11) %*% covMatrix %*% unitVector11
var21 <- t(unitVector21) %*% covMatrix %*% unitVector11
var12 <- t(unitVector12) %*% covMatrix %*% unitVector11

varEv1 <- t(ev1) %*% covMatrix %*% ev1
varEv2 <- t(ev2) %*% covMatrix %*% ev2

dataPC1Rotated <- data %>% apply(1, function(oneRow) {
  vector <- c(oneRow["x"], oneRow["y"])
  xAxis <- c(1, 0)
  winkel <- -acos((xAxis %*% ev1)/(sqrt(ev1[1]^2 + ev1[2]^2) * sqrt(xAxis[1]^2 + xAxis[2]^2)))
  
  change <- matrix(c(cos(winkel), -sin(winkel), sin(winkel), cos(winkel)), ncol=2, nrow=2)
  
  vector %*% change
})


dataPC2Rotated <- data %>% apply(1, function(oneRow) {
  vector <- c(oneRow["x"], oneRow["y"])
  xAxis <- c(1, 0)
  winkel <- -acos((xAxis %*% ev2)/(sqrt(ev2[1]^2 + ev2[2]^2) * sqrt(xAxis[1]^2 + xAxis[2]^2)))
  
  change <- matrix(c(cos(winkel), -sin(winkel), sin(winkel), cos(winkel)), ncol=2, nrow=2)
  
  vector %*% change
})
plot(data$x, data$y)
plot(dataPC1Rotated[1,], dataPC1Rotated[2,])
plot(dataPC2Rotated[1,], dataPC2Rotated[2,])