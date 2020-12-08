library(readr)
library(dplyr)
library(tidyverse)
library(caret)
library(matlib)
library(ggplot2)

data <- read_delim("02_03_Generative Models Data_v01.csv", ";", escape_double = FALSE, trim_ws = TRUE)

reducedData <- data %>% select(relative_humidity_9am, relative_humidity_3pm, HumidityClass)

class0 <- reducedData %>% filter(HumidityClass == 0)
class1 <- reducedData %>% filter(HumidityClass == 1)
class2 <- reducedData %>% filter(HumidityClass == 2)
class3 <- reducedData %>% filter(HumidityClass == 3)

mean0 <- apply(class0, 2, mean)
mean1 <- apply(class1, 2, mean)
mean2 <- apply(class2, 2, mean)
mean3 <- apply(class3, 2, mean)

standardDev0 <- apply(class0, 2, sd)
standardDev1 <- apply(class1, 2, sd)
standardDev2 <- apply(class2, 2, sd)
standardDev3 <- apply(class3, 2, sd)

dataframe <- data.frame(x = seq(0, 40, by = .1))
dataframe$y0 <- dnorm(dataframe$x, mean = mean0["relative_humidity_9am"], sd = standardDev0["relative_humidity_9am"])
dataframe$y1 <- dnorm(dataframe$x, mean = mean1["relative_humidity_9am"], sd = standardDev1["relative_humidity_9am"])
dataframe$y2 <- dnorm(dataframe$x, mean = mean2["relative_humidity_9am"], sd = standardDev2["relative_humidity_9am"])
dataframe$y3 <- dnorm(dataframe$x, mean = mean3["relative_humidity_9am"], sd = standardDev3["relative_humidity_9am"])

ggplot() + geom_line(dataframe, aes(x=x, y=y2, color = "blue"))

ggplot(dataframe,aes(x,y0))+geom_line(aes(color="Class 0"))+
  geom_line(data=dataframe,aes(x, y1, color="Class 1"))+
  geom_line(data=dataframe,aes(x, y2, color="Class 2"))+
  geom_line(data=dataframe,aes(x, y3, color="Class 3"))+
  labs(color="Legend")

ggsave(
  "simpleNormalDistributionOfHumidityData.png",
)

set.seed(101)
sample <- sample.int(n = nrow(reducedData), size = floor(.75*nrow(reducedData)), replace = F)
#train <- reducedData[sample, ]
#test  <- reducedData[-sample, ]

train <- reducedData
test <- reducedData

class0 <- train %>% filter(HumidityClass == 0)
class1 <- train %>% filter(HumidityClass == 1)
class2 <- train %>% filter(HumidityClass == 2)
class3 <- train %>% filter(HumidityClass == 3)

mean0 <- apply(class0, 2, mean)
mean1 <- apply(class1, 2, mean)
mean2 <- apply(class2, 2, mean)
mean3 <- apply(class3, 2, mean)

standardDev0 <- apply(class0, 2, sd)
standardDev1 <- apply(class1, 2, sd)
standardDev2 <- apply(class2, 2, sd)
standardDev3 <- apply(class3, 2, sd)

dataframe <- data.frame(x = seq(0, 40, by = .1))
dataframe$y0 <- dnorm(dataframe$x, mean = mean0["relative_humidity_9am"], sd = standardDev0["relative_humidity_9am"])
dataframe$y1 <- dnorm(dataframe$x, mean = mean1["relative_humidity_9am"], sd = standardDev1["relative_humidity_9am"])
dataframe$y2 <- dnorm(dataframe$x, mean = mean2["relative_humidity_9am"], sd = standardDev2["relative_humidity_9am"])
dataframe$y3 <- dnorm(dataframe$x, mean = mean3["relative_humidity_9am"], sd = standardDev3["relative_humidity_9am"])

ggplot(dataframe,aes(x,y0))+geom_line(aes(color="Class 0"))+
  geom_line(data=dataframe,aes(x, y1, color="Class 1"))+
  geom_line(data=dataframe,aes(x, y2, color="Class 2"))+
  geom_line(data=dataframe,aes(x, y3, color="Class 3"))+
  labs(color="Legend")

clusterNew <- function(mean0, mean1, mean2, mean3, sd0, sd1, sd2, sd3, probClass0, probClass1, probClass2, probClass3, x) {
  value0 <- dnorm(x, mean0, sd0)*probClass0
  value1 <- dnorm(x, mean1, sd1)*probClass1
  value2 <- dnorm(x, mean2, sd2)*probClass2
  value3 <- dnorm(x, mean3, sd3)*probClass3
  
  result <- 0
  if(value0 > value1 && value0 > value2 && value0 > value3) {
    result <- 0
  } else if(value1 > value0 && value1 > value2 && value1 > value3) {
    result <- 1
  } else if(value2 > value0 && value2 > value1 && value2 > value3) {
    result <- 2
  } else if(value3 > value0 && value3 > value1 && value3 > value2) {
    result <- 3
  }
  result
}

predicted = test

predicted$predicted <- apply(test %>% select(relative_humidity_9am), 1, function(row) {
  clusterNew(mean0, mean1, mean2, mean3,
             standardDev0, standardDev1, standardDev2, standardDev3,
             nrow(class0)/nrow(train), nrow(class1)/nrow(train), nrow(class2)/nrow(train), nrow(class3)/nrow(train),
             row)
})

countCorrect <- nrow(predicted %>% filter(HumidityClass == predicted))

countCorrect/nrow(reducedData)
