#190805 non parametric lesser dimension
#preliminaries ---------------------------------------------------------------------------------------
library(Hmisc)
library(optimx)
library(rmgarch)
library(tidyverse)
library(plotly)
library(rgl) 
library(xts)
library(readxl)
library(rmgarch)
library(MTS)
library(gdata)
library(xtable)
library(data.table)
library(MCMCpack)
library(matrixcalc)
library(Matrix)
library(numbers)
#load the data
load("C:/Users/willi/Desktop/working/projects_git/finance_article/finance.RData")
#non parametric building -------------------------------------------
train <- basets["/20190401"]
hftr <- hf_data["/20190401"]
#build kernel function, bi-squared function
k <- 21
a <- 1  #amplitude parameter
f <- function(x){(1-(x/k)^2)^2}
x <- seq(-k,0)
y <- f(x)
y2 <- y/a #make it uniform so that the weights sum 1
plot(x,y2)
sum(y2)
#a general window k for all ----------------------------------------
#bi-squared kernel
smooth <- function(k,a,jj){
  per1 <- dim(train)[1]
  ve1 <- train[,jj]
  f <- function(x){(1-(x/k)^2)^2}
  x <- seq(-k,0)
  y <- f(x)
  y2 <- y/a 
  #t <- 12
  sm1 <- c() #smoothed variance
  t <- 20
  for ( t in k:per1) {
    pr1 <- (t-k+1):t
    ho1 <- ve1[pr1]^2
    y1 <- y2*as.numeric(ho1) 
    sm1[t]  <- sum(y1)
  }
  smts1 <- xts(sm1, order.by = index(train))
  smts1
}
a1 <- smooth(k=10,a=5,jj=1)
plot(a1)
a2 <- cbind(a1,train[,jj],hftr[,jj])
plot(a2["2018/"])


#another funtion, with the whole sample
k <- 3
a <- 20
smooth2 <- function(k,a,jj){
  per1 <- dim(basets)[1]
  ve1 <- basets[,jj]
  f <- function(x){(1-(x/k)^2)^2}
  x <- seq(-k,0)
  y <- f(x)
  y2 <- y/a 
  y2 %>% sum
  #t <- 12
  sm1 <- c() #smoothed variance
  t <- 20
  for ( t in k:per1) {
    pr1 <- (t-k+1):t
    ho1 <- ve1[pr1]^2
    y1 <- y2*as.numeric(ho1) 
    sm1[t]  <- sum(y1)
  }
  smts1 <- xts(sm1, order.by = index(basets))
  smts1
}
a2 <- smooth2(k=10,a=5,jj=1)
a2 %>% class
plot(a2)
jj <- 1
a2 <- cbind(a1,train[,jj],hftr[,jj])
plot(a2["2018/"])


#maximize fit for l and a ----------------------------------------
jj <- 2
ve1 <- train[,jj]
per1 <- dim(train)[1]
ra1 <- 1:100  #range a 1
ma1 <- as.data.frame(matrix(NA,ncol=length(ra1),nrow = per1))
names(ma1) <- as.character(ra1)
k <- 20
f <- function(x){(1-(x/k)^2)^2}
x <- seq(-k,0)
y <- f(x)
a <- 5
for (a in ra1) {
  y2 <- y/a 
  sm1 <- c() #smoothed variance
  for ( t in k:per1) {
    pr1 <- (t-k+1):t
    ho1 <- ve1[pr1]
    y1 <- y2*as.numeric(ho1) 
    sm1[t]  <- mean(y1)
  }
  pos1 <- which(a==ra1) 
  ma1[,pos1] <- sm1
  print(a)
}
smts1 <- xts(ma1, order.by = index(train))
plot(smts1[,c(1,10,100)])
tts1 <- cbind(train[,jj], hftr[,jj],smts1)
plot(tts1["201805/"][,c(2,3,4,30)])

i <- 40
h1 <- c()
for (i in 3:dim(tts1)[2]) {
  x <- tts1[,i]
  x2 <- (x-tts1[,2])^2
  x3 <- x2[!is.na(x2)]
  x4 <- mean(x3)
  h1 <- append(h1,x4)
}
plot(h1[-(1:20)], type='l')


#build loop for testing which value for k is optimum----------------------
per1 <- dim(train)[1]
d2 <- dim(train)[2]
#seqk <- c(seq(5,40,by=10),seq(40,70,by=5),seq(75,200,by=15),seq(205,500,by=50))
#seqk <- c(seq(1,9,by=1),seq(10,100,by=10))
seqk <- seq(1,10,by=1)
dfes <- as.data.frame((matrix(NA,nrow=length(seqk),ncol=(d2+1)))) #data frame error storage
names(dfes) <- c("k",names(train))
k <- 10
rana <- c(seq(5,100,by=5)) #range of a
list1 <- list()
hfst1 <- data.frame(matrix(NA,ncol=3,nrow=length(rana))) 
names(hfst1) <- c("opt_a",'opt_k','mse')
hfst1[,1] <- rana
a <- 6
k <- 5
for (a in rana) {
  for (k in seqk) {
    f <- function(x){(1-(x/k)^2)^2}
    x <- seq(-k+1,0)
    y <- f(x)
    #sy <- sum(y)
    sy <- a
    y2 <- y/sy #make it uniform so that the weights sum 1
    #build data frame to hold smoothed variances
    dfsm1 <- matrix(NA,nrow=per1,ncol=d2)   #data frame of smoothed variances
    jj <- 1
    for (jj in 1:d2) {
      ve1 <- train[,jj]
      #t <- 12
      sm1 <- c() #smoothed variance
      for ( t in k:per1) {
        pr1 <- (t-k+1):t
        ho1 <- ve1[pr1]
        y1 <- y2*as.numeric(ho1) 
        sm1[t]  <- sum(y1)
      }
      dfsm1[,jj] <- sm1
      print(paste("series ",as.character(jj),", window ",as.character(k),
                  ", amplitude ",as.character(a)))
    }
    dfts1 <- xts(dfsm1, order.by = index(train)[1:dim(dfsm1)[1]])
    #data frame of measurement error
    ip1 <- index(hftr) #important period
    ln1 <- length(ip1)
    dfme1 <- matrix(NA,ncol=d2,nrow=ln1)
    jj <- 1
    for (jj in 1:d2) {
      cmt1 <- cbind(dfts1[,jj],hftr[,jj])[ip1]
      er1 <- cmt1[,1]-cmt1[,2]
      dfme1[,jj] <- er1
    }
    s1 <- apply(dfme1,2,function(x){sum(x[!is.na(x)]^2)})
    pos1 <- which(k==seqk)
    dfes[pos1,1:d2] <- s1
  }
  rownames(dfes) <- as.character(seqk) 
  names(dfes) <- names(basets)
  posa <- which(a==rana)
  list1[[posa]] <- dfes
  
}
names(list1) <- paste("ampl",as.character(rana)) 


#for each ticker find the optimum value
st1 <- data.frame(matrix(NA,ncol=2,nrow=75))
names(st1) <- c('row','col')
row.names(st1) <- names(basets)
i <- 1
for (i in 1:75) {
  m3 <- sapply(list1,function(x){x[,i]})
  posi1 <- which(m3 == min(m3), arr.ind = TRUE)
  st1[i,] <- posi1
}
st1[,2] <- st1[,2]*5

#for the general minimum
m3 <- sapply(list1, function(x){x[,1]})
for (i in 2:75) {
  m3 <- m3 + sapply(list1, function(x){x[,i]})
}
which(m3 == min(m3), arr.ind = TRUE)


#create smoothed series with the precribed values ------------------
save(st1, file = "st1.RData")
load("st1.RData") #data with optimal values for 
smo1 <- matrix(NA,ncol=75,nrow=dim(basets)[1])
i <- 1
for (i in 1:75) {
  #val1 <- st1[i,]
  k <-as.numeric(val1[1])  
  a <-  as.numeric(val1[2]) 
  a1 <- smooth2(k,a,i)
  smo1[,i] <- a1
  print(i)
}
#transform smo1 into a xts
smo1 <- xts(smo1, order.by = index(basets))
jj <- 15
a2 <- cbind(smo1[,jj],train[,jj]^2,hftr[,jj])
plot(a2["2018/"])
plot(a2["2018/"][,c(1,3)])
#rename the smoothed series to be used for analysis
smoothed1 <- smo1



#finis -----------------------------------------------------------

gdata::keep(basets, 
            hf_data, 
            daily_cov, 
            hf_cov, 
            vech_daily,
            vech_hf,
            smoothed1,
            sure=TRUE)
save(list = ls(all=TRUE), file = "finance2.RData")

































