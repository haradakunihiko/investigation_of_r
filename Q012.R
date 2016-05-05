goal <- 10
toto <- rep(1:5,c(50,30,10,8,2))
res <- sample(toto, goal, replace = TRUE)
sum <- cumsum(res)
min(seq(10)[sum >= 10])