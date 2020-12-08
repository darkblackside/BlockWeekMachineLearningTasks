library(readr)
library(dplyr)
library(tidyverse)
library(caret)
library(matlib)


multivariateGaussian <- function(mean, sigma, x) {
  invSigma <- inv(sigma)
  xMinusMean <- x-mean
  pointMeanSigmapointMean <- t(xMinusMean) %*% invSigma %*% xMinusMean
  (1/((2*pi)^(length(mean)/2)*det(sigma)^(1/2)))*exp(-(1/2)*pointMeanSigmapointMean)
}

data <- read_delim("02_03_Generative Models Data_v01.csv", ";", escape_double = FALSE, trim_ws = TRUE)

print(class(data))

reducedData <- data %>% select(air_pressure_9am, air_temp_9am, `Binary Class`)

classA <- reducedData %>% filter(`Binary Class` == "A")
classB <- reducedData %>% filter(`Binary Class` == "A")

summary(classA)
summary(classB)

covClassA <- cov(classA %>% select(air_pressure_9am, air_temp_9am))
covClassB <- cov(classB %>% select(air_pressure_9am, air_temp_9am))

inverseClassA <- inv(covClassA)
inverseClassB <- inv(covClassB)

detClassA <- det(covClassA)
detClassB <- det(covClassB)

multivariateGaussian()