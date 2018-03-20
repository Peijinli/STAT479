rm(list = ls())
library(Matrix)
setwd("C:\\Users\\Peiji\\Desktop\\spring2018\\STAT479\\class\\hw3")
dat<-read.csv("US_county.csv")
str(dat$urladdress)

split<-function(x)
  strsplit(as.character(x)," ")
}

j<-apply(as.matrix(dat$nodes),1,split)

findindex<-function(x){
  which(dat$urladdress==x)
}

send=c()
get=c()

for (ind in 1:length(j)){
  print(ind)
  j2<-apply(unique(as.matrix(unlist(j[ind]))),1,findindex)
  i2<-rep(dat$X[ind],length(j2))
  send<-c(send,i2)
  get<-c(get,j2)
}
length(send)
length(get)
x = rep(1, length(get))
data<-data.frame(send,get,x)

A = spMatrix(3142,3142,i = send, j = get, x = as.numeric(rep(1, length(get))))
#check 
#A[1,]
#j2<-sort(apply(unique(as.matrix(unlist(j[1]))),1,findindex))
#A==j2
dim(A)
save(A , file = "wikiCountyGraph.RData")
data<-load("wikiCountyGraph.RData")

writeMM(A,file='test.txt')
readMM(file='test.txt')

