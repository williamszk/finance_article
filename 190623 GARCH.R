#190623 GARCH
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

load("C:/Users/willi/Desktop/working/projects_git/finance_article/finance.RData")

#GARCH  ------------------------------------------------------------------------
#question: how to export data frame to latex
#https://stackoverflow.com/questions/9274013/generating-latex-output-from-r-data-frame
# xtable(my.data.frame)

#use the training sample to estimate the model:
#starting of the test sample is 20190402
index(basets) %>% range #[1] "2013-01-07" "2019-05-27"
basetst <- basets["20130117/20190401"]
#38 days for forecasting and testing
nlines1 <- dim(basetst)[1]
ncols1 <- dim(basetst)[2]
plot(basetst[,1])
#create auxiliary table
names1aux <- c('ticker','omega','alpha1','beta1','Q(1)','Q(5)')
len1aux <- length(names1aux)
df1aux <- data.frame(matrix(rep(NA,ncols1*len1aux),ncol=len1aux))
names(df1aux) <- names1aux
model1GARCH <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                          mean.model = list(armaOrder=c(0,0)),
                          distribution.model = "std")

ii <- 1
for (ii in 1:ncols1) {
  aux1names <- names(basetst)[ii]
  df1aux[ii,1] <- aux1names
  
  m1 <- ugarchfit(spec = model1GARCH, data = basetst[,ii])
  c1aux <- coef(m1) 
  res <- residuals(m1)
  
  if (!is.null(c1aux)) {
    
    lbtest1 <- Box.test(res, lag=1 ,type="Ljung-Box")
    veclbtest1 <- as.numeric(lbtest1)
    
    lbtest5 <- Box.test(res, lag=5 ,type="Ljung-Box")
    veclbtest5 <- as.numeric(lbtest5)
        
    ic1aux <- infocriteria(m1)
    
    df1aux[ii,2] <- c1aux[2]
    df1aux[ii,3] <- c1aux[3]
    df1aux[ii,4] <- c1aux[4]
    
    df1aux[ii,5] <- veclbtest1[3]
    df1aux[ii,6] <- veclbtest5[3]
    print(ii)
  }
}
#3 are strange
#xtable(df1aux[1:10,],digits = 3)
xtable(df1aux,digits = 3)
View(df1aux)
q1 <- df1aux$`Q(1)`
q5 <- df1aux$`Q(5)`
ln1 <- length(q1)
sum(q1<.01)/ln1
sum(q1<.05)/ln1
sum(q1<.10)/ln1

sum(q5<.01)/ln1
sum(q5<.05)/ln1
sum(q5<.10)/ln1


#forecasting, static ------------------------------------------ 
#forecasting period: 2019-04-02/2019-05-27
hf_test <- hf_data["20190401/20190527"]
dim(hf_test)
dim1 <- dim(hf_test)
hf_test %>% class
rn1 <- index(hf_test)
rn2 <- as.character(rn1)  
testim1 <- df1aux #table of estimates, change name
names(testim1)

lng1 <- dim(basets)[1]

ii <- 1
fore1 <- function(ii) {
  #build list to store estimates
  outlist1 <- list()
  #high frequency data
  hf1 <- hf_test[,ii]
  hf2 <- as.numeric(hf1)
  lgnte <- length(hf1) #length test
  #import estimated parameters
  omega <- testim1[ii,2]
  alpha1 <- testim1[ii,3]
  beta1 <- testim1[ii,4]
  #import the daily return series
  rxts <- basets[,ii] #use the whole base that is basets
  r <- as.numeric(rxts)#goes till 2019-05-27
  #create series for volatility
  sig <- c()
  sig[1] <- 0
  res <- c()
  for (t in 2:lng1) {
    sig[t] <- omega + alpha1*r[t-1]^2 + beta1*sig[t-1]  
    res[t] <- r[t]/(sig[t]^.5) 
  }
  #Box.test(res, lag=2 ,type="Ljung-Box")
  #Box.test(res, lag=20 ,type="Ljung-Box")
  #plot(sig,type='l')
  sigts <- xts(sig,order.by = index(basets)) #sigma time series
  tsall <- cbind(sigts,hf_data[,ii])
  # plot(tsall["2017/"])
  # View(tsall)
  tsall
}

test1 <- fore1(1)
plot(test1)

# build table of estimated sigma ----------------------------------------------

tbsig <- data.frame(matrix(NA, nrow=1570, ncol=75))
names(tbsig) <- names(basets)
i <- 1
for (i in 1:75) {
  bts1 <- fore1(i) #base time series 1
  v1 <- bts1[,1]
  v2 <- v1["20130117/20190527"]
  tbsig[,i] <- v2
}
ind1 <- index(basets["20130117/20190527"])
tbsig <- xts(tbsig, order.by = ind1)
tbsig %>% class
anyNA(tbsig)
save(tbsig, file = "tbsig.RData")
load("tbsig.RData") #data with optimal values for 

#error of prediction ------------------------------------------------------------
#build table to store the forecast values
tf1 <- data.frame(matrix(rep(NA,dim1[1]*dim1[2]),nrow=dim1[1]))  #table forecast 1
dim(tf1)
rownames(tf1) <- rn2
names(tf1) <- names(hf_test)  
dim(tf1)

#build table for errors of prediction
ii <- 1
for (ii in 1:dim1[2]){
  bts1 <- fore1(ii) #base time series 1
  #plot(bts1["20170101/"])
  #find the NAs in the two series
  na_hf <- is.na(bts1[,2])
  na_sig <- is.na(bts1[,1])
  #drop where both are na
  bts1_1 <- bts1[!na_hf,]
  na_sig2 <- is.na(bts1_1[,1])
  bts1_2 <- bts1_1[!na_sig2,]
  #plot(bts1_2["20180601/"])
  #dim1[1] #size of forecasting evaluation sample
  if (dim(bts1_2)[1]!=0) {
    bts2 <- tail(bts1_2,dim1[1])
    diff1 <- bts2[,1] - bts2[,2]
    #diff1 %>% class
    diff1_num <- as.numeric(diff1)
    tf1[,ii] <- diff1_num 
    #View(tf1)
  }
  print(ii)
}
View(tf1)
class(tf1)
tf2 <- xts(tf1, order.by=as.Date(rn2,format = '%Y-%m-%d'))
plot(tf2[,1:3])

baux1 <- fore1(7)
plot(baux1["2018/"])

#total mean squared error of the univariate GARCH method, static forecasting
tsaux1 <- apply(tf1,1,function(x){sum(x[!is.na(x)]^2)})
tsaux1 %>% class
tsaux1_1 <- xts(as.numeric(tsaux1), order.by = index(hf_test))
tsaux1_1 %>% View
names(tsaux1_1) <- "MSE"
plot(tsaux1_1)

#total mean average error of the univariate GARCH method, static forecasting
tsaux1 <- apply(tf1,1,function(x){sum(abs(x[!is.na(x)]))})
tsaux1 %>% class
tsaux1_1 <- xts(as.numeric(tsaux1), order.by = index(hf_test))
tsaux1_1 %>% View
names(tsaux1_1) <- "MSE"
plot(tsaux1_1)






#finis -------------------------------------

































