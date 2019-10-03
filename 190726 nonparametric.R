# 190726 nonparametric
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
train <- vech_daily["/20190401"]
cvtr <- vech_hf["/20190401"]
#build kernel function, bi-squared function
k <- 21
a <- 20  #amplitude parameter
f <- function(x){(1-(x/k)^2)^2}
x <- seq(-k,0)
y <- f(x)
y2 <- y/a #make it uniform so that the weights sum 1
plot(x,y2)
sum(y2)

#a general window k for all ----------------------------------------
#bi-squared kernel
per1 <- dim(train)[1]
jj <- 1854
ve1 <- train[,jj]
k <- 21
a <- 20  #amplitude parameter
f <- function(x){(1-(x/k)^2)^2}
x <- seq(-k,0)
y <- f(x)
y2 <- y/a 
plot(x,y2)
sum(y2)
#t <- 12
sm1 <- c() #smoothed variance
t <- 20
for ( t in k:per1) {
  pr1 <- (t-k+1):t
  ho1 <- ve1[pr1]
  y1 <- y2*as.numeric(ho1) 
  sm1[t]  <- sum(y1)
}
smts1 <- xts(sm1, order.by = index(train))
plot(smts1)
tts1 <- cbind(smts1, train[,jj], cvtr[,jj])
plot(tts1["201805/"])


anyNA(tts1[-(1:(k+10) ),])
anyNA(tts1[,1])
anyNA(tts1[,2])
anyNA(tts1[,3])

tts1[,1][ -(1:k) ] %>% anyNA()
is.na(tts1[,3]) %>% which
in1 <-  as.numeric(is.na(tts1[,3]) )  
plot(in1)
#only in the beginning are NA

#maximize fit for l and a ----------------------------------------
jj <- 1854
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
tts1 <- cbind(train[,jj], cvtr[,jj],smts1)
plot(tts1["201805/"][,c(2,3,4,5)])

i <- 40
h1 <- c()
for (i in 3:dim(tts1)[2]) {
  x <- tts1[,i]
  x2 <- (x-tts1[,2])^2
  x3 <- x2[!is.na(x2)]
  x4 <- mean(x3)
  h1 <- append(h1,x4)
}
plot(h1, type='l')





#build the loop for all covariances -------------------------------
per1 <- dim(train)[1]
d2 <- dim(train)[2]
k <- 60
f <- function(x){(1-(x/k)^2)^2}
x <- seq(-k+1,0)
y <- f(x)
sy <- sum(y)
y2 <- y/sy #make it uniform so that the weights sum 1
#build data frame to hold smoothed variances
dfsm1 <- matrix(NA,nrow=per1,ncol=d2)   #data frame of smoothed variances
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
  print(jj)
}

dfts2 <-  dfts1
#rm(dfts2)
#save(dfts2, file = "df60.RData")
#load("df60.RData")

dfts2 <- xts(dfsm1, order.by = index(train)[1:dim(dfsm1)[1]])
#just a visual inspection
jj <- 500
cmts <- cbind(dfts1[,jj],train[,jj],cvtr[,jj]) 
plot(cmts["201804/"])
plot(cmts)
plot(cmts[,-2])

#data frame of measurement error
ip1 <- index(cvtr) #important period
ln1 <- length(ip1)
dfme1 <- matrix(NA,ncol=d2,nrow=ln1)
jj <- 1
for (jj in 1:d2) {
  cmt1 <- cbind(dfts1[,jj],cvtr[,jj])[ip1]
  er1 <- cmt1[,1]-cmt1[,2]
  dfme1[,jj] <- er1
  print(jj)
}
dfme2 <- dfme1^2
sum(dfme2) is.na(dfme2)
dfme2[1:10,1:5]



#build loop for testing which value for k is best DEPRECATED----------------------
per1 <- dim(train)[1]
d2 <- dim(train)[2]
perd1 <- seq(4,1000,by=5) #period to analyze
jj <- 1
for (jj in 1:d2) {
  k <- 10
  dfk <-data.frame(matrix(NA,nrow=length(perd1),ncol=2))#data frame for many k
  for (k in perd1) {
    pos1 <- which(k==perd1)
    names(dfk) <- c("k","fit")
    f <- function(x){(1-(x/k)^2)^2}
    x <- seq(-k+1,0)
    y <- f(x)
    sy <- sum(y)
    y2 <- y/sy #make it uniform so that the weights sum 1
    ve1 <- train[,jj]
    #t <- 12
    sm1 <- c() #smoothed variance
    for ( t in k:per1) {
      pr1 <- (t-k+1):t
      ho1 <- ve1[pr1]
      y1 <- y2*as.numeric(ho1) 
      sm1[t]  <- sum(y1)
    }
    print(paste(as.character(jj),as.character(k)))
    sm2 <- xts(sm1, order.by = index(train)[1:dim(dfsm1)[1]])
    ip1 <- index(cvtr) #important period
    cm <- cbind(sm2,cvtr[,jj])[ip1]
    val1 <- sum((cm[,1] - cm[,2])^2) 
    dfk[pos1,1] <- k
    dfk[pos1,2] <- val1
    
    
  }
  val2 <- dfk$fit
  dfk2 <- dfk[order(val2),]
  
}
dfme1 <- matrix(NA,ncol=d2,nrow=ln1)
jj <- 1
for (jj in 1:d2) {
  cmt1 <- cbind(dfts1[,jj],cvtr[,jj])[ip1]
  er1 <- cmt1[,1]-cmt1[,2]
  dfme1[,jj] <- er1
  print(jj)
}
dfme2 <- dfme1^2
sum(dfme2) is.na(dfme2)
dfme2[1:10,1:5]






#build loop for testing which value for k is best new try ----------------------
per1 <- dim(train)[1]
d2 <- dim(train)[2]
seqk <- c(seq(5,40,by=10),seq(40,70,by=5),seq(75,200,by=15),seq(205,500,by=50))
dfes <- as.data.frame((matrix(NA,nrow=length(seqk),ncol=d2+1))) #data frame error storage
names(dfes) <- c("k",names(train))
for (k in seqk) {
  f <- function(x){(1-(x/k)^2)^2}
  x <- seq(-k+1,0)
  y <- f(x)
  sy <- sum(y)
  y2 <- y/sy #make it uniform so that the weights sum 1
  #build data frame to hold smoothed variances
  dfsm1 <- matrix(NA,nrow=per1,ncol=d2)   #data frame of smoothed variances
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
    print(paste(as.character(jj),as.character(k)))
  }
  dfts1 <- xts(dfsm1, order.by = index(train)[1:dim(dfsm1)[1]])
  #data frame of measurement error
  ip1 <- index(cvtr) #important period
  ln1 <- length(ip1)
  dfme1 <- matrix(NA,ncol=d2,nrow=ln1)
  jj <- 1
  for (jj in 1:d2) {
    cmt1 <- cbind(dfts1[,jj],cvtr[,jj])[ip1]
    er1 <- cmt1[,1]-cmt1[,2]
    dfme1[,jj] <- er1
  }
  #dfme2 <- dfme1^2
  dfme2 <- abs(dfme1)
  s1 <- apply(dfme2,2,function(x){sum(x^2)})
  pos1 <- which(k==seqk)
  dfes[pos1,2:(d2+1)] <- s1
}
dfes[,1] <- seqk

dfes %>% dim
dfes[1:5,1:20]

#dfes2 <- dfes
#save(dfes2, file = "mse_all2.RData")
#save(dfes, file = "mse_all.RData")

#inspection for each series, which is the best fit
di2 <- dim(dfes)[2]-1
dfs1 <- as.data.frame(matrix(NA,nrow=di2,ncol=2)) #data frame storage 1, for optimal k for each series
names(dfs1) <- c('series','best_k')
dfs1[,1] <- names(dfes)[-1]
jj<- 2
for (jj in 1:di2+1 ) {
  v1 <- dfes[,c(1,jj)]
  v2 <- v1[,2]
  df1 <- v1[order(v2),]
  dfs1[jj-1,2] <- df1
}
k <- 100
dfs1[(k+1):(k+100),]

#summarize the best k as mean

dfes2 <- apply(dfes[,-1],1,function(x){sum(x)})
dfes2 %>% length
df2 <- data.frame(seqk,dfes2)
df2[order(df2[,2]),]
df3 <- df2[-(1:3),]
plot(df3[,1],df3[,2], type='l')

#    seqk        dfes2
#12    60 2.151340e-07
#13    65 2.153443e-07
#11    55 2.156340e-07
#14    70 2.159013e-07
#15    75 2.166012e-07
#16    80 2.172793e-07
#10    50 2.172948e-07
#17    85 2.179496e-07
#18    90 2.186647e-07
#19    95 2.194166e-07
#9     45 2.201016e-07
#20   100 2.201710e-07
#21   105 2.208806e-07
#22   110 2.215850e-07


#

































#finis ---------------------------------------------------------------
















