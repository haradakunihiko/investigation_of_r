times <- 100
dice <- 1:6
result <- sample(dice, times, replace = TRUE)
sum <- cumsum(result)
rem <- sum %% 10
rem == 0
min(seq(times)[rem == 0],times)