num <- 50
seq(num)
x <- (2 ^ seq(num) - 2 ) %% seq(num)
seq(num)[x == 0]