野球で同点
a <- 1:10
b <- 1:10
outer(a,b)
outer(a,b, "-")
outer(a,b, "+")
res <- outer(a,b, "-")/outer(a,b, "+")
res[res<0] = NA
res
