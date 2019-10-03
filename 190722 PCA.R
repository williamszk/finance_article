#190722 PCA
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

#PCA with penalties is likelihood fuction ---------------------------
covd <- cov(vech_daily)
pc1 <- eigen(covd)
s2 <- sum(pc1$values)
cumsum(pc1$values)/s2
(pc1$values/s2)[1:10]
#build the FVC for 1 factor only
vec1 <- pc1$vectors
#columns contain the eigenvectors
vec1[,1] %>% length
len1 <- dim(vech_daily)[1]
dim1 <- dim(covd)[1]
de1 <- 100 #how many pc we want
pca <- matrix(NA,ncol =de1 , nrow = len1)
pca %>% dim
ii <- 1
for (ii in 1:de1) { #use only the first 100 
  pca[,ii] <- apply(vech_daily,1,function(x){sum(x*vec1[,ii])})  
  print(ii)
}
pts <- xts(pca, order.by = index(vech_daily))
plot(pts[,2])

#a test in building volatility equation
omega <- 0
a1 <- .05
b1 <- .95
c1 <- 0
pca1 <- pca[,1]
lng1 <- length(pca1)
#the asset that we want to analyze
jj <- 1584
r <- vech_daily[,jj]
sig <- c(var(r))
for (t in 2:lng1) {
  sig[t] <- omega + a1*r[t-1] + b1*sig[t-1] + c1*pca1[t-1]
}
sigt1 <- xts(sig, order.by =index(vech_daily))
sigts <- cbind(sigt1, r, vech_hf[,jj])
plot(sigts["20180501/"])

#build function for optimization
parm <- c(omega, a1, b1)
likef <- 
  function(parm){
    omega <- parm[1]
    a1 <- parm[2]
    b1 <- parm[3]
    
    r <- vech_daily[,1]
    sig <- c(var(r))
    for (t in 2:lng1) {
      sig[t] <- omega + a1*r[t-1] + b1*sig[t-1] + 0*pca1[t-1]
    }
    
    like1 <- -1/2*log(2*pi) - 1/(lng1*2) * sum(log(sig)) 
    - 1/lng1 * sum(r/(2*sig)) 
    like1*(-1) #change sign to minimize
  }



likef(parm)
likef(c(0,0.063173,0.913047)) #ugarch package
likef(c(0,0.06092133,0.8943585)) #nelder mead
likef(c(0,0.06092133,0.8943585))



iparm <- c(0,0.1,0.9) #for the optimx function this is the starting values
optimx(iparm,likef,lower=-Inf, upper=Inf, method=c("Nelder-Mead","BFGS"))

iparm1 <- c(0,0.1,0.9,0)
iparm2 <- c(0,0.1,0.9,-.5)
iparm3 <- c(0,0.1,0.9,.5)
#optim(iparm1,likef,
#      method = c("Nelder-Mead"))
vec_ipar <- list(iparm1,iparm2,iparm3)
for (ii in vec_ipar) {
  opt1 <- optim(ii,likef,
                method = c("Nelder-Mead"))
  print(opt1)
}


### some options...
optim(iparm,likef,
      method = c("SANN"))
optim(iparm,likef,
      method = c("CG")) #gives error
optim(iparm,likef,
      method = c("L-BFGS-B"))#gives error
optim(iparm,likef,
      method = c("BFGS")) #gives error


model11 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                      mean.model = list(armaOrder=c(0,0)),
                      distribution.model = "std")
m1  <- ugarchfit(spec = model11, data = vech_daily[,jj])
m1

#maximum likelihood function without log of likelihood
jj <- 1584
likef <- 
  function(parm){
    omega <- parm[1]
    a1 <- parm[2]
    b1 <- parm[3]
    
    r <- vech_daily[,jj]
    sig <- c(var(r))
    for (t in 2:lng1) {
      sig[t] <- omega + a1*r[t-1] + b1*sig[t-1] + 0*pca1[t-1]
    }
    sig2 <- sig*10^5
    like1 <- 1/sqrt(2*pi*sig2)
    x <- log(sig2) 
    is.nan(x) %>% sum
    like1*(-1) #change sign to minimize
  }




#Principal Component Analysis ----------------------------------------
#Factor variance-covariance model (FVC) without correction HAC
covd <- cov(vech_daily)
pc1 <- eigen(covd)
s2 <- sum(pc1$values)
cumsum(pc1$values)/s2
(pc1$values/s2)[1:10]
#build the FVC for 1 factor only
vec1 <- pc1$vectors
#columns contain the eigenvectors
vec1[,1] %>% length
len1 <- dim(vech_daily)[1]
dim1 <- dim(covd)[1]
pca <- matrix(NA,ncol =dim1 , nrow = len1)
pca %>% dim
ii <- 1
for (ii in 1:dim1) {
  pca[,ii] <- apply(vech_daily,1,function(x){sum(x*vec1[,ii])})  
  print(ii)
}
pts <- xts(pca, order.by = index(vech_daily))
plot(pts[,10])

listmo1 <- list()
sigt <- matrix(NA,ncol =dim1 , nrow = len1)
model11 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                      mean.model = list(armaOrder=c(1,1)),
                      distribution.model = "std")
for (ii in 1:dim1) {
  try(listmo1[[ii]] <- ugarchfit(spec = model11, data = pca[,ii])
      , silent = TRUE  )
  try(sigt[,ii] <- sigma(listmo1[[ii]])
      ,silent = TRUE)
  print(ii)
}
#from 1578 onwards there are problems for convergence...

jj <- 1
ii <- 1
infg <-  function(jj){
  storage1 <- matrix(NA, ncol = dim1, nrow = 2)
  for (ii in 1:dim1) {
    if (anyNA(sigt[,ii])==FALSE) {
      model1 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,0),
                                                 external.regressors = as.matrix(sigt[,ii]) ),
                           mean.model = list(armaOrder=c(1,1)),distribution.model = "std")
      train <- vech_daily[,jj]["/20190401"]
      try(m1 <- ugarchfit(spec = model1, data = train), silent = TRUE )
      try(storage1[ii,1] <- ii, silent = TRUE)
      try(storage1[ii,2] <- infocriteria(m1)[1], silent = TRUE)
      print(ii)
    } 
  }
  storage1
}
plot(bb1[,c(1,2,3,4)])

model1 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,0),
                                           external.regressors = as.matrix(sigt[,ii]) ),
                     mean.model = list(armaOrder=c(1,1)),distribution.model = "std")
model2 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),
                                           external.regressors = NULL),
                     mean.model = list(armaOrder=c(1,1)),distribution.model = "std")
train <- vech_daily[,jj]["/20190401"]
m1 <- ugarchfit(spec = model1, data = train)
m2 <- ugarchfit(spec = model2, data = train)
sigf1 <- sigma(m1)
sigf2 <- sigma(m2)
bb1 <- cbind(sigf1,sigf2,vech_hf[,jj],vech_daily[,jj])


#fit diagnosis some other useful fuctions... ---------------------
infocriteria(mp1)
signbias(mp1)
nymblom(mp1)
sig1 <- sigma(mp1)
plot(sig1)



#try some PCA in GARCH ----------------------------------------------
pts <- cbind(sig1,sig2)
pts <- cbind(sig1,sig3)
pts <- cbind(sig2,sig3)
pts <- cbind(sig1)
pts <- cbind(sig2)
pts <- cbind(sig3)

model1 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,0),
                                           external.regressors = pts),
                     mean.model = list(armaOrder=c(1,1)),
                     distribution.model = "std")
model2 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),
                                           external.regressors = NULL),
                     mean.model = list(armaOrder=c(1,1)),
                     distribution.model = "std")
ii <- 8
train <- vech_daily[,ii]["/20190401"]
#plot(train)
m1 <- ugarchfit(spec = model1, data = train)
m2 <- ugarchfit(spec = model2, data = train)
m1
met1 <- mean(vech_daily[,ii]) #mean of daily variance 
sigf1 <- sigma(m1)
sigf2 <- sigma(m2)
bb1 <- cbind(sigf1,sigf2,vech_hf[,ii],vech_daily[,ii])
plot(bb1[,c(1,2,3)])
plot(bb1[,c(2,4)])
plot(bb1[,c(1,4)])






#estiamtion of GARCH by hand and other stuff --------------------------------------------------
#build FVC model with this new variable
omega <- 0
a1 <- .05
b1 <- .95
c1 <- -.1
lng1 <- length(pca1)
r <- vech_daily[,1]
sig <- c(var(r))
for (t in 2:lng1) {
  sig[t] <- omega + a1*r[t-1] + b1*sig[t-1] + c1*pca1[t-1]
}
sigts <- xts(sig, order.by = index(vech_daily))
plot(sigts)

parm <- c(omega, a1, b1, c1)
likef <- 
  function(parm){
  omega <- parm[1]
  a1 <- parm[2]
  b1 <- parm[3]
  c1 <- parm[4]
  
  r <- vech_daily[,1]
  sig <- c(var(r))
  for (t in 2:lng1) {
    sig[t] <- omega + a1*r[t-1] + b1*sig[t-1] + c1*pca1[t-1]
  }
  
  like1 <- -1/2*log(2*pi) - 1/(lng1*2) * sum(log(sig)) - 1/lng1 * sum(r/(2*sig)) 
  like1*(-1) #change sign to minimize
}

likef(parm)

iparm <- c(0,0.1,0.9,-1) #for the optimx function this is the starting values
optimx(iparm,likef,lower=-Inf, upper=Inf, method=c("Nelder-Mead","BFGS"))


iparm1 <- c(0,0.1,0.9,0)
iparm2 <- c(0,0.1,0.9,-.5)
iparm3 <- c(0,0.1,0.9,.5)
#optim(iparm1,likef,
#      method = c("Nelder-Mead"))
vec_ipar <- list(iparm1,iparm2,iparm3)
for (ii in vec_ipar) {
  opt1 <- optim(ii,likef,
            method = c("Nelder-Mead"))
  print(opt1)
}


### some options...
optim(iparm,likef,
      method = c("SANN"))
optim(iparm,likef,
      method = c("CG")) #gives error
optim(iparm,likef,
      method = c("L-BFGS-B"))#gives error
optim(iparm,likef,
      method = c("BFGS")) #gives error

### rugarch package manual
args(ugarchspec)

function (variance.model = 
            list(model = "sGARCH", 
                 garchOrder = c(1,1), 
                 submodel = NULL, 
                 external.regressors = NULL, 
                 variance.targeting = FALSE), 
          mean.model = 
            list(armaOrder = c(1, 1), 
                 include.mean = TRUE, 
                 archm = FALSE, 
                 archpow = 1, 
                 arfima = FALSE, 
                 external.regressors = NULL, 
                 archex = FALSE), 
            distribution.model = "norm", 
            start.pars = list(), 
            fixed.pars = list(), ...) 

model (default = "sGARCH" (vanilla GARCH). 
       Valid models are "iGARCH", "gjrGARCH", "eGARCH", "apARCH","fGARCH","csGARCH", "mcsGARCH")

expca <- matrix(pca1)
expca %>% dim
expca <- cbind(pca1,pca2)
expca %>% class
pca1 %>% length

model1 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),
                                                external.regressors = expca),
                          mean.model = list(armaOrder=c(0,0)),
                          distribution.model = "std")
train <- vech_daily[,1]["/20190401"]
plot(train)
m1 <- ugarchfit(spec = model1, data = train)
m1
# some tests with PCA --------------------------------------------------

#build the covariance matrix
d1 <- dim(daily_cov[[1]])[1]
mean_dc <- matrix(rep(NA,d1^2), ncol = d1) #mean of daily_cov
ii <- 1; jj <- 1
for (ii in 1:d1) {
  for (jj in 1:d1) {
    hold1 <- sapply(daily_cov, function(x){x[ii,jj]})    
    hold2 <- mean(hold1)
    mean_dc[ii,jj] <- hold2
  }
}
mean_dc[1:10,1:5]

#function to find position of variance in variance-covariance matrices
vp <- function(n,k=75){
  if (n==1) {
    out <- 1
    return(out)
  }
  if (n>1) {
    m <- n-2
    (m+1)*k - m*(m+1)/2 + 1  
  }
}
vp(n=50)

#auto correlation function for the series in basets
ii <- 1
plot(basets[,ii])
acf(basets[,ii])
#the level of autocorrelation does not show autocorrelation
acf(basets[,ii]^2)
#the squared of the series show autocorrelation

###
#PCA on the daily variance and covariance, ie r^2
names(vech_daily)
ii <- vp(60)
ii <- sample(1:dim(vech_daily)[2], 1)
ii <- vp(1)
plot(vech_daily[,ii])
acf(vech_daily[,ii])

#use PCA without correction
covd <- cov(vech_daily)
dim(covd)
covd[1:5,1:5]
pc1 <- eigen(covd)
s2 <- sum(pc1$values)
cumsum(pc1$values)/s2
#apply HAC correcion before using PCA on the data







###

e1 <- eigen(mean_dc)
names(e1)
e1$values
s1 <- sum(e1$values) 
cumsum(e1$values)/s1

pcats <- prcomp(basets)
summary(pcats)
#both methods give the same result

###
#cholesky decomposition

tm1 <- daily_cov[[1]]
tm2 <- chol(tm1)


A = as.matrix(data.frame(c(3,4,3),c(4,8,6),c(3,6,9)))
colnames(A) <- NULL
A
is.positive.definite(A, tol=1e-8)
A.chol <- chol(A)
A.chol

di1 <- diag(A.chol)
Achol2 <- A.chol
diag(Achol2) <- 1
G <- diag(di1,nrow=3)

t(Achol2) %*% G %*% Achol2
A

ch <- chol(A)
dd <- diag(ch)
L <- t(ch/dd)
DD <- dd^2
L
G <- diag(DD,nrow=3)

L %*% G %*% t(L)
A

###
mtcars2 <- mtcars[,c(1:7,10,11)]
pca1 <- prcomp(mtcars2)
names(pca1)
pca1$sdev
su1 <- sum(pca1$sdev)
cumsum(pca1$sdev)/su1
summary(pca1)

library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
ggbiplot(pca1)







#finis ---------------------------------------------------------------





















