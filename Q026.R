number <-13
A<-paste("ハ",seq(number),sep="")
B<-paste("ス",seq(number),sep="")
M<-rbind(A,B)
M
newM<-apply(M,2,sample, 1)
newM