num <- 1000
a <-cumsum(1/seq(num)) %% 1
all(a[-1] != 0)