coin <- sample(0:1, 100, replace = TRUE)
a <- runif(100) < 0.2
b <- runif(100) < 0.1
awin <- coin == 0 & a
bwin <- coin == 1 & b

both <- awin + bwin
min(seq(100)[both == 1])

