num <- 40
students <- seq(40)
x <- sample(students, 100, replace = TRUE)
unique(x)
num - length(unique(x))

seq(num)[is.na(match(seq(num),unique(x) ))]