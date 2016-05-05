library(quantmod)

# use single quotes and specify data source:
# googleだと時間がとれてない、、？
getSymbols("YHOO", src = "google")  # src = "yahoo" is the default
head(YHOO)

# coerce from an xts object to a standard numerical R vector:
yhoo.close <- as.vector(YHOO[, "YHOO.Close"])  
head(yhoo.close)
head(yhoo.close[-1])
head(yhoo.close[-length(yhoo.close)])

tail(yhoo.close)
tail(yhoo.close[-1])
tail(yhoo.close[-length(yhoo.close)])

length(yhoo.close)
length(yhoo.close[-1])
length(yhoo.close[-length(yhoo.close)])

log.yahoo <- log(yhoo.close[-1]/yhoo.close[-length(yhoo.close)])
head(log.yahoo)                              
