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
library(base)
library(MCMCpack)

path1 <- "C:/Users/willi/Desktop/working/RAW_DATA/saved_RData/"
basets1 <-as.data.frame(fread(paste(path1,"basets.csv",sep='')))  
hf_data1 <-as.data.frame(fread(paste(path1,"hf_data.csv",sep='')))
name4 <- paste(path1,"hf_cov.rds",sep='')
hf_cov <- readRDS(file = name4)
name5 <- paste(path1,"daily_cov.rds",sep='')
daily_cov <- readRDS(file = name5)


#tranform data frames into xts
date1 <- basets1$date
basets2 <- basets1[,-c(1,2)]
date2 <- as.Date(date1, format = "%Y-%m-%d")
basets <- xts(x = basets2, order.by = date2)
basets %>% class
dim(basets)

date1 <- hf_data1$date
date1 %>% class
hf_data2 <- hf_data1[,-c(1,2)]
date2 <- as.Date(date1,format = "%Y-%m-%d")
hf_data <- xts(x = hf_data2, order.by = date2)
hf_data %>% class
dim(hf_data)

#calculate univariate EWMA -------------------------------------------------
#use rugarch package to calculate ewma
#the estimation here is a univariate EWMA model
# a standard specification
spec1 = ugarchspec()
spec1
# an example which keep the ar1 and ma1 coefficients fixed:
spec2 = ugarchspec(mean.model=list(armaOrder=c(2,2),
                                   fixed.pars=list(ar1=0.3,ma1=0.3)))
spec2
# an example of the EWMA Model
spec3 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                   distribution.model="norm", fixed.pars=list(omega=0))



#calculate muultivariate EWMA by hand ------------------------------------------
#initial value for vol matrix
d1 <- dim(daily_cov[[1]])
#function for the mean of the covariance matrices
mhf_cov <- function(){ #mean hf_cov
  l1 <- length(daily_cov)
  maux1 <- matrix(rep(0,d1[1]^2), ncol=d1[1])
  for (ii in 1:l1) {
    maux1 <- maux1 + daily_cov[[ii]]
  }
  maux2 <- maux1/l1
} 
mcov <- mhf_cov()
mcov[1:5,1:5]
#use the mean of the covariance matrice as the initial value for the sequencing
Sig <- list()
Sig[[1]] <- mcov
l1 <- length(daily_cov)
lambda <- .94 #use caninocal value for the multivariate EWMA model
for (t in 2:l1) {
  Sig[[t]] <- (1-lambda)*daily_cov[[t-1]] + lambda * Sig[[t-1]]
}
aux1 <- sapply(Sig, function(x){det(x)})
plot(aux1,type='l')
det(Sig[[90]])
Sig[[90]][1:5,1:5]
sum(aux1==0)
aux1
ii <- 23
det(Sig[[ii]])
solve(Sig[[ii]])

plot(basets[,1:4])

#build data frame for forecasting error ----------------------------------------
hf_test <- hf_data["20190401/20190527"] #test period
dim1 <- dim(hf_test); dim1
tf1 <- data.frame(matrix(rep(NA,dim1[1]*dim1[2]),nrow=dim1[1]))  #table forecast 1
dim(tf1)
rn2 <- as.character(index(hf_test))  
rownames(tf1) <- rn2
names(tf1) <- names(hf_test)  
View(tf1)
dim(tf1)
lng1 <- dim(basets)[1]

#tranform matrix form into vech form
#just an example
symmat <- matrix(c(1,2,3,4,2,4,5,6,3,5,7,8,4,6,8,9),4,4)
vech(symmat)


sigvech <- sapply(Sig, function(x){vech(x)})
sigvech %>% length
sigvech %>% class
dim11 <- dim(sigvech)
sigvech %>% dim
sigvech2 <- t(sigvech)
sigvech3 <- xts(sigvech2,order.by = index(basets))
names(sigvech3)
plot(sigvech3[,9])

hfvech <- sapply(hf_cov, function(x){vech(x)})
hfvech %>% dim
hfvech2 <- t(hfvech)
hfvech3 <- xts(hfvech2, order.by = index(hf_data))
plot(hfvech3[,3])

#create names for columns of vech matrices ----------------------------------------
names1 <- names(basets)
names1
d1 <- length(names1)

#just an example
nam1 <- c("A","B","C","D")
paste("A",nam1,sep='_')
paste("B",nam1[-1],sep='_')
paste("C",nam1[-(1:2)],sep='_')

nvech <- c()
kk <- 0
for (kk in 0:(length(nam1)-1)) {
  n1 <- nam1[kk+1]
  if (kk==0) {
    nvech <- append(nvech,paste(n1,nam1,sep='_')) 
  }
  if (kk!=0) {
    nvech <- append(nvech,paste(n1,nam1[-(1:kk)],sep='_'))   
  }
}
nvech

#create vector to hold names
nvech <- c()
kk <- 0
for (kk in 0:(d1-1)) {
  n1 <- names1[kk+1]
  if (kk==0) {
    nvech <- append(nvech,paste(n1,names1,sep='_')) 
  }
  if (kk!=0) {
    nvech <- append(nvech,paste(n1,names1[-(1:kk)],sep='_'))   
  }
}
length(nvech)
tail(nvech)
tail(names1)

#rename xts vech objects and join objects-------------------------------------------
hfvech3 %>% dim
hfvech3 %>% class
names(hfvech3) <- nvech
#hfvech3 %>% View
sigvech3 %>% tail
names(sigvech3) <- nvech
#sigvech3 %>% View


difvech <-  hfvech3["20190401/20190527"]-sigvech3["20190401/20190527"]
difvech %>% dim


difvech[,1:4]
difvech[,1:4] %>% plot



#finis -------------------------------------




#a test for possible values of lambda DEPRECATED ----------------------------------------------------
ranl <- c(0,seq(0.1,.9,by=0.1),seq(.9,.99,by=.001),seq(.99,.9999,by=.00001)) #range lambda
la1 <- length(ranl)
df1 <- data.frame(matrix(rep(NA,2*la1),ncol=2))
names(df1) <- c('lambda', 'n_sing')
df1[,1] <- ranl
for (lambda in ranl) {
  print(lambda)
  Sig <- list()
  Sig[[1]] <- mcov
  for (t in 2:l1) {
    Sig[[t]] <- (1-lambda)*daily_cov[[t-1]] + lambda * Sig[[t-1]]
  }
  aux1 <- sapply(Sig, function(x){det(x)})
  w1 <- which(lambda==ranl)
  df1[w1,2] <- sum(aux1==0)
}
plot(x=df1$lambda,y=df1$n_sing)
df2 <- df1[df1$n_sing==0,] 
View(df2)
#beyond .938 is ok, lesser than this value covariance matrices are singular


# maximum likelihood estimation DEPRECATED ----------------------------------------------------

ii <- 2
jj <- 3
x1 <- sapply(Sig,function(x){x[ii,jj]})
plot(x1, type='l')
x2 <-  sapply(daily_cov,function(x){x[ii,jj]})
plot(x2, type='l')

lambda <- .99
ewmaf <- function(lambda){ #ewma function
  Sig <- list()
  Sig[[1]] <- mcov
  for (t in 2:l1) {
    Sig[[t]] <- (1-lambda)*daily_cov[[t-1]] + lambda * Sig[[t-1]]
  }
  #build log maximumlikelihood
  k <- d1[1]
  tobs <- l1
  #sum log determinants of covariance matrices
  #for the first component
  sumdet1 <- sapply(Sig, function(x){log(det(x))})
  logsum <- sum(sumdet1)
  #for the second component
  comp2 <- 0
  len2 <- dim(basets)[1]
  for (t in 1:len2) {
    line1 <- as.numeric(basets[t,])
    comp2 <- comp2 + as.numeric(t(line1)%*%solve(Sig[[t]])%*%line1)  #second component
    
  }
  -(k/2)*log(2*pi) - 1/(tobs*2) * (logsum + comp2) 
}

seq1 <- seq(.938,.999,by=.001)
lee1 <- length(seq1)
dfe1 <- data.frame(matrix(rep(NA,2*lee1),ncol=2))
names(dfe1) <- c('lambda','lik')
dfe1[,1] <- seq1
for (lambda in seq1) {
  pos1 <- which(lambda==seq1)
  dfe1[pos1, 2] <- ewmaf(lambda)
}
plot(x=dfe1$lambda,y=dfe1$lik, type='l')
View(dfe1)
#the optimal value tends to 1, which is not good





