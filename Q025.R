number <- 100
a <- sample(1:3, number, replace = TRUE)
b <- sample(1:3, number, replace = TRUE)
tb <- table(a,b)
tb

awin <- tb[1,2] + tb[2,3] + tb[3,1]
bwin <- tb[2,1] + tb[3,2] + tb[1,3]
even <- sum(diag(tb))
c(awin,bwin,even)