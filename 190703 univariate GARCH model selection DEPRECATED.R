# 190703 GARCH tests
#preliminaries ----------------------------------------------------------------
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

#import data
path1 <- "C:/Users/willi/Desktop/working/RAW_DATA/saved_RData/"
load(paste(path1,"190626_finance_article.RData",sep=''))

hf_data <- fread(paste(path1,"hf_data.csv",sep=''))
hf_data <- as.data.frame(hf_data)



#the model ---------------------------------------------------------------------

#data frame to store best models for each ticker
lng2 <- dim(basets)[2] #length 2
dfbm1 <- data.frame(matrix(rep(0, lng2*5), ncol=5)) #data frame best models
names(dfbm1) <- c('AR','MA','ARCH','GARCH','AIC')
rownames(dfbm1) <- names(basets)

#the models to be tested:
garch_func <- 
  function(ar1=0,ma1=0,arch1=1,garch1=1, var1=var1){
    modelGARCH <- ugarchspec(variance.model = 
                               list(model="sGARCH",
                                    garchOrder=c(arch1,garch1)),
                                    mean.model = list(armaOrder=c(ar1,ma1)),
                                    distribution.model = "std")
    garch1 <- ugarchfit(spec = modelGARCH, data = var1)
    try(aic1 <- infocriteria(garch1)[1], silent=TRUE)
    if (exists('aic1')){
      aic1
    }
  }

#just an example
var1 <- basets[,1]
garch_func(var1=var1)

#create data frame to store informaion about AIC and model selected
mor <- 5 #maximum order
arv1 <- 0:mor
mav1 <- 0:mor
archv1 <- 0:mor
garchv1 <- 0:mor

aux_dep1 <- data.frame(matrix(rep(0,(mor+1)^4*5), ncol=5))
names(aux_dep1) <- c('AR','MA','ARCH','GARCH','AIC')
aux_dep1$GARCH <- rep(garchv1,(mor+1)^3)
#ii<-1
var1 <-c()
for (ii in 0:mor) {
  var1 <- c(var1,rep(ii,(mor+1)))
}
aux_dep1$ARCH <- rep(var1,(mor+1)^2)
var2 <-c()
for (ii in 0:mor) {
  var2 <- c(var2,rep(ii,(mor+1)^2))
}
aux_dep1$MA <- rep(var2,(mor+1))
var3 <-c()
for (ii in 0:mor) {
  var3 <- c(var3,rep(ii,(mor+1)^3))
}
aux_dep1$MA <- var3

#to the loop
#ticker <- 2
for (ticker in 1:lng2) {
  var1 <- basets[,ticker]
  jj <- 1
  for (ar1 in arv1) {
    for (ma1 in mav1) {
      for (arch1 in archv1) {
        for (garch1 in garchv1) {
          rm(garch_m)
          try(garch_m <- garch_func(ar1=ar1,
                                    ma1=ma1,
                                    arch1=arch1,
                                    garch1=garch1, 
                                    var1=var1),
              silent=TRUE)
          if (!is.null(garch_m)) {
            aux_dep1[jj,5] <- garch_m  
          }
          jj <- jj + 1
          print(paste(as.character(jj),as.character(ticker)))
        }
      }
    }
  }
  aux_dep1
  adarch <- aux_dep1$ARCH==0 #aux drop arch
  aux_dep2 <- aux_dep1[!adarch,]
  aux_dep3 <- aux_dep2[order(aux_dep2$AIC),  ]
  av1 <- aux_dep3[1,] #aux value 1
  
  dfbm1[ticker,] <- av1
}


#finis--------------------------------------------------------------------------

path1 <- "C:/Users/willi/Desktop/working/RAW_DATA/dir_finance_article_2019/"
name1 <- paste(path1,"ugarch_morders.csv",sep='')
write.csv(dfbm1, name1)




