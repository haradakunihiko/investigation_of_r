cards <- seq(1,13)
cards
a <- sample(cards)
b <- sample(cards)
a
b


result <- a - b
result

sum(a[result > 0]) + sum(b[result > 0]) + sum(a[result == 0])
sum(b[result < 0]) + sum(a[result < 0]) + sum(b[result == 0])

