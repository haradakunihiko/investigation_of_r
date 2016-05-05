numberOfPeople <- 5
numberOfTest <- 3
ar <- sample(1:100, numberOfPeople * numberOfTest, replace = TRUE)
mat <- matrix(ar,numberOfPeople, numberOfTest)

names <- c("a", "b", "c", "d", "e")
tests <- c("A", "B", "C")
dimnames(mat) <- list(names, tests)
mat