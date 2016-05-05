people <- 100
number <- 5
toto <- sample(1:10,people * number, replace = TRUE)
toto
result <- matrix(toto, number, people)
tousen <- sample(1:10, number, replace = TRUE)
tousen2<- matrix(rep(tousen,each= people),number, people, byrow = TRUE)
result
tousen2
n <- apply(result == tousen2, 1, sum)
n
round(100000/n)