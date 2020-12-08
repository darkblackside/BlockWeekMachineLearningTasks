data <- data.frame(holiday=c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
                   examTomorrow=c(1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0),
                   age=c(10, 15, 20, 10, 15, 20, 10, 15, 20, 10, 15, 20),
                   studyTodayYes=c(1, 0, 7, 1, 4, 5, 11, 11, 13, 11, 18, 26),
                   studyTodayNo=c(2, 1, 1, 19, 16, 4, 2, 2, 0, 24, 16, 5))

totals <- sum(data$studyTodayYes) + sum(data$studyTodayNo)

giniIndex <- function(a1, p1, a2, p2) {
  a1*p1*(1-p1) + a2*p2*(1-p2)
}

splitDecision <- function(data1, data2) {
  left <- data1
  sumLeft <- sum(left$studyTodayYes)+sum(left$studyTodayNo)
  right <- data2
  sumRight <- sum(right$studyTodayYes)+sum(right$studyTodayNo)
  
  giniIndex(sumLeft, sum(left$studyTodayYes)/sumLeft, sumRight, sum(right$studyTodayYes)/sumRight)
}

splitDecision(data %>% filter(holiday == 0), data %>% filter(holiday == 1))
splitDecision(data %>% filter(examTomorrow == 0), data %>% filter(examTomorrow == 1))
splitDecision(data %>% filter(age <= 10), data %>% filter(age > 10))
splitDecision(data %>% filter(age <= 15), data %>% filter(age > 15))
splitDecision(data %>% filter(age < 15), data %>% filter(age >= 15))
#first split: age <= 15 & age > 15

#split
ageTo15 <- data %>% filter(age <= 15)
age20 <- data %>% filter(age > 15)

splitDecision(ageTo15 %>% filter(holiday == 0), ageTo15 %>% filter(holiday == 1))
splitDecision(ageTo15 %>% filter(examTomorrow == 0), ageTo15 %>% filter(examTomorrow == 1))
splitDecision(ageTo15 %>% filter(age <15), ageTo15 %>% filter(age == 15))
#second split: split exam tomorrow or not

splitDecision(age20 %>% filter(holiday == 0), age20 %>% filter(holiday == 1))
splitDecision(age20 %>% filter(examTomorrow == 0), age20 %>% filter(examTomorrow == 1))
#second split: split exam tomorrow or not
