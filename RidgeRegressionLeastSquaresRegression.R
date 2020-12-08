library(ggplot2)
library(Metrics)

getCorrect <- function(data, rightCol, predictedCol) {
  nrow(data %>% filter(rightCol == predictedCol))/nrow(data)
}



data <- data.frame(x1 = c(5, 2, 0, -2, -3), x2 = c(1, 2, 4, 5, 7), y = c(20, 15, 10, 5, 0))
data$x0 <- rep(1, nrow(data))

X <- data.matrix(data %>% select(x0, x1, x2), rownames.force = NA)
y <- data.matrix(data %>% select(y), rownames.force = NA)

#closed form for ridge regression: omega=(X^T*X+lambda*I)^-1*(X^T*y)
identity <- diag(3)
identity[1,1] = 0
lambda <- 5
omegaRidge <- inv(t(X) %*% X + lambda*identity) %*% (t(X) %*% y)
omegaRidge

data$predicted <- apply(X, 1, function(row) {
  c(omegaRidge) %*% c(row)
})

rmse(data$y, data$predicted)


#closed form for least squares: omega=(X^T*X)^-1*(X^T*y)
#add ones X
omegaLS <- inv(t(X) %*% X) %*% (t(X) %*% y)
omegaLS

data$predicted <- apply(X, 1, function(row) {
  c(omegaLS) %*% c(row)
})

rmse(data$y, data$predicted)

# With given values:
omegaFree <- c(0.996909546, -1.979734231)
b <- 17.12422483

data$predicted <- apply(X, 1, function(oneRow) {
  c(omegaFree) %*% c(oneRow[c(2,3)]) + b
})

rmse(data$y, data$predicted)


#Missing: Calculate L1 & L2