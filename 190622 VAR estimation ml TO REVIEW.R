#190622 script1

#preliminaries-------------------------------------------------------------

library(Hmisc)
library(optimx)
library(rmgarch)
library(tidyverse)
library(plotly)
library(rgl) 
library(xts)

#simulate a VAR(2) model with 3 variables---------------------------------------------------------------
Tobs = 500 #number of periods
k=3 #number of variables

phi1_11 =0.4;phi1_12 =0.2;phi1_13=0.4
phi1_22 =0.1;phi1_23=0.1
phi1_33=0.2
phi2_11 =0.2;phi2_12 =0;phi2_13=0
phi2_22 =0.2;phi2_23=0
phi2_33=0.2
sig_11 =1;sig_12 =0;sig_13=0
sig_22 =1;sig_23=0
sig_33=1

phi1 = matrix(c(phi1_11, phi1_12, phi1_13,
                phi1_12, phi1_22, phi1_23,
                phi1_13, phi1_23, phi1_33),nrow=3)
phi2 = matrix(c(phi2_11, phi2_12, phi2_13,
                phi2_12, phi2_22, phi2_23,
                phi2_13, phi2_23, phi2_33),nrow=3)
y = matrix(rep(0,Tobs*k), ncol = 3)
#initial values

#create matrix of shocks, for now use a iid matrix, all variances are 1
set.seed(123)
u = matrix(rnorm(Tobs*k), ncol = 3)

for (t in 3:Tobs) {
  y[t,] = phi1 %*% y[t-1,] + phi2 %*% y[t-2,] + u[t,]  
}
auxdf1 = data.frame(y)
as.Date(10957) #is the year 2000 in the first day
aux_date1 = as.Date(10957:(10957+Tobs-1))
base1 = xts(auxdf1, order.by = aux_date1)
plot(base1)

#verify if the VAR process is stable--------------------------------
#buil the A matrix
aux_m1 = cbind(phi1,phi2)
aux_m2 = cbind(diag(k),diag(k)*0)
A = rbind(aux_m1,aux_m2)
ev <- eigen(A)
ev$values
#if all eigenvalues are less than 1 in module than the VAR is stable
# calculate likelihood of the true parameters---------------------------------------------------------------
SIG = matrix(c(sig_11,sig_12,sig_13,
               sig_12,sig_22,sig_23,
               sig_13,sig_23,sig_33),nrow=3)

mu =  matrix(rep(y[1,],Tobs),ncol=k,byrow = TRUE) 
for (t in 3:Tobs) {
  mu[t,] = phi0 + phi1 %*% y[t-1,] + phi2 %*% y[t-2,]
} 
head(mu)
muts = xts(data.frame(mu), order.by = aux_date1 )
plot(muts)
l = rep(0,Tobs)
for (t in 1:Tobs) {
  l[t] = -(k/2)*log(2*pi) - .5*log(det(SIG)) - 
    .5*( t(matrix(y[t,] - mu[t,],nrow=k))%*%solve(SIG)%*%(matrix(y[t,]-mu[t,],nrow=k)))
}
mean_like = mean(l)
mean_like
#[1] -4.227992
#estimate a VAR model for 3 return assets---------------------------------------------------------------

#define par##########
par = c(
  phi0_1,
  phi0_2,
  phi0_3,
  phi1_11, phi1_12, phi1_13,
  phi1_22, phi1_23,
  phi1_33,
  phi2_11, phi2_12, phi2_13,
  phi2_22, phi2_23,
  phi2_33,
  sig_11, sig_12, sig_13,
  sig_22, sig_23,
  sig_33
)

#define function##########
like_f = function(par){
  
  phi0_1<-par[1]
  phi0_2<-par[2]
  phi0_3<-par[3]
  phi1_11<-par[4]
  phi1_12<-par[5]
  phi1_13<-par[6]
  phi1_22<-par[7]
  phi1_23<-par[8]
  phi1_33<-par[9]
  phi2_11<-par[10]
  phi2_12<-par[11]
  phi2_13<-par[12]
  phi2_22<-par[13]
  phi2_23<-par[14]
  phi2_33<-par[15]
  sig_11<-par[16]
  sig_12<-par[17]
  sig_13<-par[18]
  sig_22<-par[19]
  sig_23<-par[20]
  sig_33<-par[21]
  
  
  SIG = matrix(c(sig_11,sig_12,sig_13,
                 sig_12,sig_22,sig_23,
                 sig_13,sig_23,sig_33),nrow=3)
  
  phi0 = matrix(c(phi0_1,phi0_2,phi0_3), nrow = 3)
  
  phi1 = matrix(c(phi1_11, phi1_12, phi1_13,
                  phi1_12, phi1_22, phi1_23,
                  phi1_13, phi1_23, phi1_33),nrow=3)
  
  phi2 = matrix(c(phi2_11, phi2_12, phi2_13,
                  phi2_12, phi2_22, phi2_23,
                  phi2_13, phi2_23, phi2_33),nrow=3)
  
  mu =  matrix(rep(y[1,],Tobs),ncol=k,byrow = TRUE) 
  
  for (t in 3:Tobs) {
    mu[t,] = phi0 + phi1 %*% y[t-1,] + phi2 %*% y[t-2,]
  } 
  
  l = rep(0,Tobs)
  for (t in 1:Tobs) {
    l[t] = -(k/2)*log(2*pi) - .5*log(det(SIG)) - 
      .5*( t(matrix(y[t,] - mu[t,],nrow=k))%*%solve(SIG)%*%(matrix(y[t,]-mu[t,],nrow=k)))
  }
  mean_like = mean(l)
  mean_like
}
mu

like_f(par)

#test with the wrong set of parameters
par2 = c(25 , 0.3	,	0.4,	0.3,	0.2,	0.4,	0.1,	0.1,	0.2,	0.2,	0,	0,	
  0.2,	0,	0.2,	1,	0,	0,	1,	0,	1)
aux_len1 = length(par2) #auxiliary length measure for parameters vector
like_f(par2)

identical(par,par2)
aux_ver = par==par2
par[!aux_ver]
par2[!aux_ver]



# estimate VAR using optimx function---------------------------------------------------------------
#for thr optimx function this is the starting values
inicparm <- c(25 , 0.3	,	0.4,	0.3,	0.2,	0.4,	0.1,	0.1,	0.2,	0.2,	0,	0,	
              0.2,	0,	0.2,	1,	0,	0,	1,	0,	10)
like_f(par2)
optimx(inicparm,like_f,lower=-Inf, upper=Inf, method=c("Nelder-Mead","BFGS"))
optim(inicparm,like_f,
      method = c("Nelder-Mead"))
optim(inicparm,like_f,
      method = c("SANN"))
optim(inicparm,like_f,
      method = c("CG")) #
optim(inicparm,like_f,
      method = c("L-BFGS-B"))
optim(inicparm,like_f,
      method = c("BFGS"))

#it takes too much time to estimate, maybe I should find the score function for VAR

#use a package to estimate the simulated VAR---------------------------------------------------------------


library(vars)
summary(auxdf1)
attach(auxdf1)
adf1 <- ur.df(X1,type='trend',lags=2)
summary(adf1)

VARselect(auxdf1, lag.max = 8, type = "both")

p1ct <- VAR(auxdf1, p = 2, type = "both")
p1ct











#finis---------------------------------------------------------------








