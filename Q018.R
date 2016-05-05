dice <- 3
res <- sample(1:6, dice, replace = TRUE)
res
duplicated(res)
res[duplicated(res)]