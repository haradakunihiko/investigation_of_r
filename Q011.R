num <- 100
head <- runif(num) < 0.5
target <- runif(num) < 0.2

sum(head|target)