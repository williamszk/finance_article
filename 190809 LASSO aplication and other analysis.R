#190809 LASSO aplication
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
library(glmnet)
#load the data
load("C:/Users/willi/Desktop/working/projects_git/finance_article/finance2.RData")
load("tbsig.RData") 
#an example -------------------------------------------------------------------------------
#https://www.rstatisticsblog.com/data-science-in-action/lasso-regression/
# Loaging the library

# Loading the data
data(swiss)

x_vars <- model.matrix(Fertility~. , swiss)[,-1]
y_var <- swiss$Fertility
lambda_seq <- 10^seq(2, -2, by = -.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), nrow(x_vars)/2)
x_test = x_vars[-train,]
y_test = y_var[-train]

cv_output <- cv.glmnet(x_vars[train,], y_var[train], 
                       alpha = 1, lambda = lambda_seq)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam <- 1.999

# Rebuilding the model with best lambda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
summary(lasso_best)
names(lasso_best)
lasso_best$beta
pred <- predict(lasso_best, s = best_lam, newx = x_test)
swiss[-train,]


final <- cbind(y_var[-train], pred)
# Checking the first six obs
head(final)


actual <- y_var[-train]
preds <- pred
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq


# Inspecting beta coefficients
coef(lasso_best)

#apply to finance --------------------------------------------------------------
tm1 <-  smoothed1["20190401/"]
plot(tm1[,2])
tm1 %>% dim
tm2 <- as.matrix(tm1)

smoothed1 %>% class
sm1 <- smoothed1["20130117/20190401"]
plot(sm1[,1])
sm1 %>% dim
#a1 <- apply(sm1,2, function(x){ sum(is.na(x))  }  )
d1 <- dim(sm1)
df1 <- as.data.frame(sm1)
df1 %>% dim
df2 <- df1[-d1[1],]
df2 %>% class
df3 <- df1[-1,]

#df2,df3
names(df2) <- names(basets)
names(df3) <- names(basets)
dim(df2)
dim(df3)

lambda_seq <- seq(0.000001,.001, by=0.000001)
i <- 1
y_var <-  df2[,i]
x_var <-  as.matrix(df3) 

cv_output <- cv.glmnet(x_var, y_var, alpha = 1, lambda = lambda_seq)

# identifying best lamda
best_lam <- cv_output$lambda.min

lasso_best <- glmnet(x_var[,1:5], y_var, alpha = 1, lambda = best_lam)
lasso_best$beta
m1 <- lm(y_var ~ x_var[,1:5])
summary(m1)

lasso_best <- glmnet(x_var, y_var, alpha = 1, lambda = best_lam)
lasso_best$beta
m1 <- lm(y_var ~ x_var)
summary(m1)

x_test <- tm2
pred <- predict(lasso_best, s = best_lam, newx = x_test)
x_test %>% dim
plot(pred, type='l')

final <- cbind(tm2[,i], pred)
finalts <- xts(final, order.by = index(tm1))
plot(finalts)
# Checking the first six obs
head(final)

actual <- tm2[,i]
preds <- pred
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

#loop for estimation ----------------------------------------------------------
tm1 <-  smoothed1["20190401/"]
tm2 <- as.matrix(tm1)

sm1 <- smoothed1["/20190401"]
sm1 <- sm1["20130117/"]
d1 <- dim(sm1)
df1 <- as.data.frame(sm1)
df2 <- df1[-d1[1],]
df3 <- df1[-1,]

names(df2) <- names(basets)
names(df3) <- names(basets)

tb1 <- data.frame(matrix(NA, nrow=75, ncol=75)) 
names(tb1) <- names(basets)

i <- 1
for (i in 1:75) {
  y_var <-  df2[,i]
  x_var <-  as.matrix(df3) 
  # identifying best lamda
  best_lam <- 0.00001
  lasso_best <- glmnet(x_var, y_var, alpha = 1, lambda = best_lam)
  tb1[,i] <- as.character(lasso_best$beta)
}

#table to analyze interconnectivity
tb1 %>% class
cn1 <- apply(tb1,1, function(x){sum(x!=0) }  ) #counter1
dfc1 <- data.frame(matrix( NA, nrow=75, ncol=2 ))
dfc1[,1] <- names(basets)
dfc1[,2] <- cn1
dfc3 <- dfc1[order(-dfc1$X2),]
dfc2 <- cbind(dfc3[1:25,], dfc3[26:50,], dfc3[51:75,]) 
names(dfc2) <- rep( c('ticker', 'n. conn.' ), 3 )
print(xtable(dfc2), include.rownames=FALSE)
# histogram of distribution
path1 <- "C:/Users/willi/Desktop/working/projects_git/article_finance_2019/"
pdf( paste(path1,"hist1.pdf", sep = "/") ) 
hist(cn1, breaks=75)
dev.off() 


#tb1 table to store estimates of VAR-LASSO estimation. from moving window non-parametric 
tb1[1:10,1:10]
tb11 <- matrix(NA,nrow=75,ncol=75)
for (i in 1:75) {
  for (j in 1:75) {
    aa1 <- tb1[i,j]  
    if (!is.na(aa1)) {
      if (aa1=="0") {
        tb11[i,j] <- "."
      }
      if (aa1!="0") {
        tb11[i,j] <- substr(aa1,1,7)
      }
    }
    if (is.na(aa1)) {
      tb11[i,j] <- "."
    }
  }
}
tb11[1:10,1:10]
#just edit the matrix, to be better for xtable.
tb2 %>% dim

tb2 <- data.frame(cbind(names(basets),tb11[,1:15])) 
names(tb2) <- c("ticker", names(basets)[1:15])
tb3 <- data.frame(cbind(names(basets),tb11[,16:30]))
names(tb3) <- c("ticker", names(basets)[16:30])
tb4 <- data.frame(cbind(names(basets),tb11[,31:45]))
names(tb4) <- c("ticker", names(basets)[31:45])
tb5 <- data.frame(cbind(names(basets),tb11[,46:60]))
names(tb5) <- c("ticker", names(basets)[46:60])
tb6 <- data.frame(cbind(names(basets),tb11[,61:75]))
names(tb6) <- c("ticker", names(basets)[61:75])

print(xtable(tb2), include.rownames=FALSE)
print(xtable(tb3), include.rownames=FALSE)
print(xtable(tb4), include.rownames=FALSE)
print(xtable(tb5), include.rownames=FALSE)
print(xtable(tb6), include.rownames=FALSE)

#forecasting of lasso --------------------------------------------------------
hf1 <- hf_data["20190401/20190527"]
tm1 <-  smoothed1["20190401/"]
tm2 <- as.matrix(tm1)
sm1 <- smoothed1["/20190401"]
sm1 <- sm1["20130117/"]
d1 <- dim(sm1)
df1 <- as.data.frame(sm1)
df2 <- df1[-d1[1],]
df3 <- df1[-1,]
names(df2) <- names(basets)
names(df3) <- names(basets)
lambda_seq <- seq(0.000001,.001, by=0.000001)

stfore1 <- data.frame(matrix(NA,nrow=length(index(hf1)), ncol=75)) #storage of forecast, non-par/VAR
names(stfore1) <- names(basets)
stfore1 <- xts(stfore1, order.by = index(hf1))
tbpred <- data.frame(matrix(NA, nrow=75,ncol=2))
names(tbpred) <- c("ticker","pred_var_lasso")
tbpred[,1] <- names(basets)
i <- 1
for (i in 1:75) {
  
  y_var <-  df2[,i]
  x_var <-  as.matrix(df3) 
  
  #cv_output <- cv.glmnet(x_var, y_var, alpha = 1, lambda = lambda_seq)
  #best_lam <- cv_output$lambda.min
  best_lam <- 0.00001
  
  lasso_best <- glmnet(x_var, y_var, alpha = 1, lambda = best_lam)
  #lasso_best$beta
  
  x_test <- tm2
  pred <- predict(lasso_best, s = best_lam, newx = x_test)
  
  stfore1[,i] <- pred
  
  final <- cbind(tm2[,i], pred)
  finalts <- xts(final, order.by = index(tm1))
  # plot(finalts)
  
  actual <- hf1[,i]
  preds <- pred
  rss <- sum((preds - actual) ^ 2)
  tss <- sum((actual - mean(actual)) ^ 2)
  rsq <- 1 - rss/tss
  tbpred[i,2] <- rsq
}
#table with ESS for prediction of moving window var-lasso 
tbpred
stfore1
plot(stfore1)
#xtable(tbpred)

#solo prediction smoothed----------------------------------------------------------
hf1 <- hf_data["20190401/20190527"]
tm1 <-  smoothed1["20190401/"]
i <- 1
tbp2 <- data.frame(matrix(NA, nrow=75,ncol=1))
names(tbp2) <- c("pred_smooth")
for (i in 1:75) {
  actual <- hf1[,i]
  dif1 <- tm1[,i] - actual
  rss <- sum(dif1^2)
  tss <- sum((actual - mean(actual)) ^ 2)
  rsq <- 1 - rss/tss
  tbp2[i,1] <- rsq
}
tbp2
tbpred2 <- cbind(tbpred, tbp2)
tbpred2
#GARCH VAR LASSO prediction ---------------------------------------------------------
tbsig %>% class
v8 <- apply(tbsig,2, function(x){  sum(is.na(x)) }   )
v8
#someone did not run the GARCH
which(v8!=0) #TRPL3 
#best_lam <- 0.00001
hf1 <- hf_data["20190401/20190527"]
hf1 <- hf1[,-38]
tm1 <-  tbsig["20190401/"]
tm2 <- as.matrix(tm1)[,-38]
sm1 <- tbsig["20130117/20190401"]
sm1 <- sm1[,-38]
d1 <- dim(sm1)
df1 <- as.data.frame(sm1)
df2 <- df1[-d1[1],]
df3 <- df1[-1,]
names(df2) <- names(basets)[-38]
names(df3) <- names(basets)[-38]

stfgarch <- data.frame(matrix(NA, nrow= length(index(hf1)), ncol=75 )) #storage forecast GARCH
names(stfgarch) <- names(basets)
stfgarch <- xts(stfgarch, order.by = index(hf1))
tbpredgarch <- data.frame(matrix(NA, nrow=74,ncol=2))
names(tbpredgarch) <- c("ticker","pred_var_garch")
tbpredgarch[,1] <- names(basets)[-38]
i <- 24
for (i in 1:74) {
  
  y_var <-  df2[,i]
  x_var <-  as.matrix(df3) 
  best_lam <- 0.00001
  
  lasso_best <- glmnet(x_var, y_var, alpha = 1, lambda = best_lam)
  #  lasso_best$beta
  
  x_test <- tm2
  pred <- predict(lasso_best, s = best_lam, newx = x_test)
  
  if (i<=37) {
    stfgarch[,i] <- pred  
  } #those are necessary measures to the case 38 where there is no
  if (i>=38) { #estiamtion for GARCH
    stfgarch[,i+1] <- pred  
  }
  
  predts <- xts(pred, order.by = index(tm1))
  predts <- cbind(hf1[,i], predts)
  # plot(predts)
  
  actual <- hf1[,i]
  preds <- pred
  rss <- sum((preds - actual) ^ 2)
  tss <- sum((actual - mean(actual)) ^ 2)
  rsq <- 1 - rss/tss
  tbpredgarch[i,2] <- rsq
}
tbpredgarch
stfgarch
plot(stfgarch)
#solo prediction GARCH ----------------------------------------------------------
hf1 <- hf_data["20190401/20190527"]
hf1 <- hf1[,-38]
tm1 <-  tbsig["20190401/"]
tm1 <- tm1[,-38]
i <- 1
tbp2garch <- data.frame(matrix(NA, nrow=74,ncol=1))
names(tbp2garch) <- c("pred_garch")
i <- 1
for (i in 1:74) {
  actual <- hf1[,i]
  dif1 <- tm1[,i] - actual
  rss <- sum(dif1^2)
  tss <- sum((actual - mean(actual)) ^ 2)
  rsq <- 1 - rss/tss
  tbp2garch[i,1] <- rsq
}
tbp2garch
tbpredgarch2 <- cbind(tbpredgarch, tbp2garch)
names(tbpredgarch2) <- c('ticker','pred_garch_var','pred_garch')
tbpredgarch2
#visualize the difference

j <- 24
ts2 <- cbind(hf1[,j],tm1[,j])
plot(ts2)


#join the two tables
names(tbpred2)
names(tbpredgarch2)
tbf1 <- merge(tbpred2,tbpredgarch2, by.x = 'ticker' , sort = FALSE, all.x = TRUE)
tbpred2 %>% dim
tbf1 %>% dim
tbf1

#we need to input the 75th line into the position of 38
tbf2 <- rbind(tbf1[1:37,],tbf1[75,],tbf1[38:74,])
tbf2 %>% class
row.names(tbf2) <- as.character(1:75) 
#xtable(tbf2, digits = 4)

#analysis of performance of each method ---------------------------------------
tbc1 <- data.frame(matrix(NA,ncol=1,nrow=75)) #table column 1
names(tbc1) <- "best_model"
i <- 1
for (i in 1:75) {
  v1 <- as.numeric(tbf2[i,]) 
  m1 <- max(v1,na.rm = TRUE)
  pos1 <- which(m1==v1)
  if (pos1==2) {
    tbc1[i,] <- "non-par/VAR"
  }
  if (pos1==3) {
    tbc1[i,] <- "non-par"
  }
  if (pos1==4) {
    tbc1[i,] <- "GARCH/VAR"
  }
  if (pos1==5) {
    tbc1[i,] <- "GARCH"
  }
}
tbc1
dft1 <- as.data.frame(table(tbc1))   #data frame table 1
dft12 <- dft1[c("4","3","2","1"),]
rownames(dft12) <- as.character(1:4)
xtable(dft12)

tbf3 <- cbind(tbf2,tbc1)
xtable(tbf3, digits = 4)


#plots of series  --------------------------------------------------------------


1
24
53
71

path1 <- "C:/Users/willi/Desktop/working/projects_git/article_finance_2019/"

j <- 1
tbts1 <- cbind(basets[,j]^2,hf_data[,j],smoothed1[,j],tbsig[,j])
pdf( paste(path1,"whole1.pdf", sep = "/") ) 
plot(tbts1)
dev.off() 
pdf( paste(path1,"partial1.pdf", sep = "/") ) 
plot(tbts1["201807/20190527"])
dev.off() 
pdf( paste(path1,"partial1a.pdf", sep = "/") ) 
plot(tbts1["20190101/20190401"])
dev.off() 

j <- 24
tbts1 <- cbind(basets[,j]^2,hf_data[,j],smoothed1[,j],tbsig[,j])
pdf( paste(path1,"whole24.pdf", sep = "/") ) 
plot(tbts1)
dev.off() 
pdf( paste(path1,"partial24.pdf", sep = "/") ) 
plot(tbts1["201807/20190527"])
dev.off() 
pdf( paste(path1,"partial24a.pdf", sep = "/") ) 
plot(tbts1["20190101/20190401"])
dev.off() 

j <- 53
tbts1 <- cbind(basets[,j]^2,hf_data[,j],smoothed1[,j],tbsig[,j])
pdf( paste(path1,"whole53.pdf", sep = "/") ) 
plot(tbts1)
dev.off() 
pdf( paste(path1,"partial53.pdf", sep = "/") ) 
plot(tbts1["201807/20190527"])
dev.off() 
pdf( paste(path1,"partial53a.pdf", sep = "/") ) 
plot(tbts1["20190101/20190401"])
dev.off() 


j <- 71
tbts1 <- cbind(basets[,j]^2,hf_data[,j],smoothed1[,j],tbsig[,j])
pdf( paste(path1,"whole71.pdf", sep = "/") ) 
plot(tbts1)
dev.off() 
pdf( paste(path1,"partial71.pdf", sep = "/") ) 
plot(tbts1["201807/20190527"])
dev.off() 
pdf( paste(path1,"partial71a.pdf", sep = "/") ) 
plot(tbts1["20190101/20190401"])
dev.off() 


smoothed1
tbsig

#plots of prediction -----------------------------------------------

stfore1
smoothed1
stfgarch
tbsig

1
24
53
71
19
55

path1 <- "C:/Users/willi/Desktop/working/projects_git/article_finance_2019/"

j <- 1
tbts2 <- cbind(basets[,j]^2,hf_data[,j],smoothed1[,j],tbsig[,j],stfore1[,j],stfgarch[,j])
pdf( paste(path1,"prev1.pdf", sep = "/") ) 
plot(tbts2["20190320/20190527"])
dev.off() 
j <- 24
tbts2 <- cbind(basets[,j]^2,hf_data[,j],smoothed1[,j],tbsig[,j],stfore1[,j],stfgarch[,j])
pdf( paste(path1,"prev24.pdf", sep = "/") ) 
plot(tbts2["20190320/20190527"])
dev.off() 
j <- 53
tbts2 <- cbind(basets[,j]^2,hf_data[,j],smoothed1[,j],tbsig[,j],stfore1[,j],stfgarch[,j])
pdf( paste(path1,"prev53.pdf", sep = "/") ) 
plot(tbts2["20190320/20190527"])
dev.off() 
j <- 71
tbts2 <- cbind(basets[,j]^2,hf_data[,j],smoothed1[,j],tbsig[,j],stfore1[,j],stfgarch[,j])
pdf( paste(path1,"prev71.pdf", sep = "/") ) 
plot(tbts2["20190320/20190527"])
dev.off() 
j <- 19
tbts2 <- cbind(basets[,j]^2,hf_data[,j],smoothed1[,j],tbsig[,j],stfore1[,j],stfgarch[,j])
pdf( paste(path1,"prev19.pdf", sep = "/") ) 
plot(tbts2["20190320/20190527"])
dev.off() 
j <- 55
tbts2 <- cbind(basets[,j]^2,hf_data[,j],smoothed1[,j],tbsig[,j],stfore1[,j],stfgarch[,j])
pdf( paste(path1,"prev55.pdf", sep = "/") ) 
plot(tbts2["20190320/20190527"])
dev.off() 






#finis --------------------------------------------------------------------------








