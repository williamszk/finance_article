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

#GARCH  ------------------------------------------------------------------------

#question: how to export data frame to latex
#https://stackoverflow.com/questions/9274013/generating-latex-output-from-r-data-frame
# xtable(my.data.frame)

nlines1 <- dim(basets)[1]
ncols1 <- dim(basets)[2]
plot(basets[,1:5])

names1aux <- c('ticker','omega','alpha1','beta1','AIC','BIC')
len1aux <- length(names1aux)
df1aux <- data.frame(matrix(rep(0,ncols1*len1aux),ncol=len1aux))
names(df1aux) <- names1aux

model1GARCH <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                          mean.model = list(armaOrder=c(0,0)),
                          distribution.model = "std")
for (ii in 1:ncols1) {
  
  aux1names <- names(basets)[ii]
  df1aux[ii,1] <- aux1names
  
  m1 <- ugarchfit(spec = model1GARCH, data = basets[,ii])
  c1aux <- coef(m1) 
  if (!is.null(c1aux)) {
    
    ic1aux <- infocriteria(m1)
    
    df1aux[ii,2] <- c1aux[2]
    df1aux[ii,3] <- c1aux[3]
    df1aux[ii,4] <- c1aux[4]
    
    df1aux[ii,5] <- ic1aux[1,1]
    df1aux[ii,6] <- ic1aux[2,1]
  }
  print(ii)
}

#xtable(df1aux[1:10,],digits = 3)
xtable(df1aux,digits = 3)

#finis -------------------------------------


gdata::keep(basets,sure=TRUE)





























