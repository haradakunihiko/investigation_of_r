number <- 100
people <- 40
res <- apply(matrix( sample(0:1, number * people, replace = TRUE), people, number), 1, sum)
hist(res)