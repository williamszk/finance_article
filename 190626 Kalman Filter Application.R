#190626 Kalman Filter Application

#preliminaries -------------------------------------------------------------

library(Hmisc)
library(optimx)
library(rmgarch)
library(tidyverse)
library(plotly)
library(rgl) 
library(xts)
path2 <- "C:/Users/willi/Desktop/working/RAW_DATA/saved_RData/"
load(paste(path2,"190626_finance_article.RData",sep=''))

# simulation of univariate Kalman Filter ----------------------------------------
Tobs <- 500
lamb <- 1
phi <- .8
sigu <- .5^2
sigeta <- 1^2
set.seed(123)
u <- rnorm(Tobs,0,sigu) 
eta <- rnorm(Tobs,0,sigeta)
zeros <- rep(0, Tobs)
y <- zeros
s <- zeros
for (t in 2: Tobs) {
  y[t] <- lamb * s[t-1] + u[t]
  s[t] <- phi * s[t-1] + eta[t]
}
plot(y, type='l')
lines(s, type = 'l', col='red')
# Kalman Filter ------------------------------------------------------------------------------
Tobs <- 500
lamb <- 1
phi <- .8
sigu <- .5^2
sigeta <- 1^2
s <- c(0,rep(NA,Tobs-1))
P <- c(1/(1-phi^2),rep(NA,Tobs-1))
#take a series in the basets
#y1 <- coredata(basets)[,2]
#y <- cumsum(y1)[1:Tobs]
#plot(y,type='l')
#create vectors before the loop
nas <- rep(NA,Tobs)
mu <- nas
V <- nas
kappa <- nas
#the loop
for (t in 2:Tobs) {
  mu[t-1] <- lamb*s[t-1]
  V[t-1] <- lamb^2 * P[t-1] + sigu
  kappa[t-1] <- (lamb^2 * P[t-1])/V[t-1]
  s[t] <- s[t-1] + kappa[t-1] * (y[t] - mu[t-1])
  P[t] <- P[t-1] - (lamb^2 * P[t-1]^2)/(V[t-1])
}
plot(y, type='l')
lines(s,type='l',col='red')
#finis -------------------------------------------------------------------------

















































