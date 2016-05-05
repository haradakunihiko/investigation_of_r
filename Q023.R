students <- 40
number <- 100
dice <- sample(1:6,number * students , replace = TRUE)
mat <- matrix(dice,students, number)
letters <- apply(mat,1,"paste",sep="",collapse="")
letters
people <- grep("666", letters)
length(people)