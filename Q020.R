num <- 10000
x <- runif(num) < 1/10
x
timing <- seq(num)[x == 1]
timing
data <- c(timing[1], diff(timing))
hist(data)