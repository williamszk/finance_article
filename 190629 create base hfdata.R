#190629 create base hfdata

#preliminaries --------------------------------------------------------

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
library(GetHFData)
library(data.table)

path1 <- "C:/Users/willi/Desktop/working/RAW_DATA/saved_RData/"
load(paste(path1,"190626_finance_article.RData",sep=''))
tickers <- names(basets)

path <- "C:/Users/willi/Desktop/working/RAW_DATA/high_frequency/"

# csv from GetHFData -----------------------------------------------
files_import <- c("hfdata_day20180702.csv",
                  "hfdata_day20180703.csv",
                  "hfdata_day20180704.csv",
                  "hfdata_day20180705.csv",
                  "hfdata_day20180706.csv",
                  "hfdata_day20180710.csv",
                  "hfdata_day20180711.csv",
                  "hfdata_day20180712.csv",
                  "hfdata_day20180713.csv",
                  "hfdata_day20180716.csv",
                  "hfdata_day20180717.csv",
                  "hfdata_day20180718.csv",
                  "hfdata_day20180719.csv",
                  "hfdata_day20180720.csv",
                  "hfdata_day20180723.csv",
                  "hfdata_day20180724.csv",
                  "hfdata_day20180725.csv",
                  "hfdata_day20180726.csv",
                  "hfdata_day20180727.csv",
                  "hfdata_day20180730.csv",
                  "hfdata_day20180731.csv",
                  "hfdata_day20180801.csv",
                  "hfdata_day20180802.csv",
                  "hfdata_day20180803.csv",
                  "hfdata_day20180806.csv",
                  "hfdata_day20180807.csv",
                  "hfdata_day20180808.csv",
                  "hfdata_day20180809.csv",
                  "hfdata_day20180810.csv",
                  "hfdata_day20180813.csv",
                  "hfdata_day20180814.csv",
                  "hfdata_day20180815.csv",
                  "hfdata_day20180816.csv",
                  "hfdata_day20180817.csv",
                  "hfdata_day20180820.csv",
                  "hfdata_day20180821.csv",
                  "hfdata_day20180822.csv",
                  "hfdata_day20180823.csv",
                  "hfdata_day20180824.csv",
                  "hfdata_day20180827.csv",
                  "hfdata_day20180828.csv",
                  "hfdata_day20180829.csv",
                  "hfdata_day20180830.csv",
                  "hfdata_day20180831.csv",
                  "hfdata_day20180903.csv",
                  "hfdata_day20180904.csv",
                  "hfdata_day20180905.csv",
                  "hfdata_day20180906.csv",
                  "hfdata_day20180910.csv",
                  "hfdata_day20180911.csv",
                  "hfdata_day20180912.csv",
                  "hfdata_day20180913.csv",
                  "hfdata_day20180914.csv",
                  "hfdata_day20180917.csv",
                  "hfdata_day20180918.csv",
                  "hfdata_day20180919.csv",
                  "hfdata_day20180920.csv",
                  "hfdata_day20180921.csv",
                  "hfdata_day20180924.csv",
                  "hfdata_day20180925.csv",
                  "hfdata_day20180926.csv",
                  "hfdata_day20180927.csv",
                  "hfdata_day20180928.csv",
                  "hfdata_day20181001.csv",
                  "hfdata_day20181002.csv",
                  "hfdata_day20181003.csv",
                  "hfdata_day20181004.csv",
                  "hfdata_day20181005.csv",
                  "hfdata_day20181008.csv",
                  "hfdata_day20181009.csv",
                  "hfdata_day20181010.csv",
                  "hfdata_day20181011.csv",
                  "hfdata_day20181015.csv",
                  "hfdata_day20181016.csv",
                  "hfdata_day20181017.csv",
                  "hfdata_day20181018.csv",
                  "hfdata_day20181019.csv",
                  "hfdata_day20181022.csv",
                  "hfdata_day20181023.csv",
                  "hfdata_day20181024.csv",
                  "hfdata_day20181025.csv",
                  "hfdata_day20181026.csv",
                  "hfdata_day20181029.csv",
                  "hfdata_day20181105.csv",
                  "hfdata_day20181106.csv",
                  "hfdata_day20181107.csv",
                  "hfdata_day20181108.csv",
                  "hfdata_day20181109.csv",
                  "hfdata_day20181112.csv",
                  "hfdata_day20181113.csv",
                  "hfdata_day20181114.csv",
                  "hfdata_day20181116.csv",
                  "hfdata_day20181119.csv",
                  "hfdata_day20181121.csv",
                  "hfdata_day20181122.csv",
                  "hfdata_day20181123.csv",
                  "hfdata_day20181126.csv",
                  "hfdata_day20181127.csv",
                  "hfdata_day20181128.csv",
                  "hfdata_day20181129.csv",
                  "hfdata_day20181130.csv",
                  "hfdata_day20181203.csv",
                  "hfdata_day20181204.csv",
                  "hfdata_day20181205.csv",
                  "hfdata_day20181206.csv",
                  "hfdata_day20181207.csv",
                  "hfdata_day20181210.csv",
                  "hfdata_day20181211.csv",
                  "hfdata_day20181212.csv",
                  "hfdata_day20181213.csv",
                  "hfdata_day20181214.csv",
                  "hfdata_day20181217.csv",
                  "hfdata_day20181218.csv",
                  "hfdata_day20181219.csv",
                  "hfdata_day20181220.csv",
                  "hfdata_day20181221.csv",
                  "hfdata_day20181226.csv",
                  "hfdata_day20181227.csv",
                  "hfdata_day20181228.csv",
                  "hfdata_day20190102.csv",
                  "hfdata_day20190103.csv",
                  "hfdata_day20190104.csv",
                  "hfdata_day20190107.csv",
                  "hfdata_day20190108.csv",
                  "hfdata_day20190109.csv",
                  "hfdata_day20190110.csv",
                  "hfdata_day20190111.csv",
                  "hfdata_day20190114.csv",
                  "hfdata_day20190115.csv",
                  "hfdata_day20190116.csv",
                  "hfdata_day20190117.csv",
                  "hfdata_day20190118.csv",
                  "hfdata_day20190121.csv",
                  "hfdata_day20190122.csv",
                  "hfdata_day20190123.csv",
                  "hfdata_day20190124.csv",
                  "hfdata_day20190128.csv",
                  "hfdata_day20190129.csv",
                  "hfdata_day20190130.csv",
                  "hfdata_day20190131.csv",
                  "hfdata_day20190201.csv",
                  "hfdata_day20190204.csv",
                  "hfdata_day20190205.csv",
                  "hfdata_day20190206.csv",
                  "hfdata_day20190207.csv",
                  "hfdata_day20190208.csv",
                  "hfdata_day20190211.csv",
                  "hfdata_day20190212.csv",
                  "hfdata_day20190213.csv",
                  "hfdata_day20190214.csv",
                  "hfdata_day20190215.csv",
                  "hfdata_day20190218.csv",
                  "hfdata_day20190219.csv",
                  "hfdata_day20190220.csv",
                  "hfdata_day20190221.csv",
                  "hfdata_day20190222.csv",
                  "hfdata_day20190225.csv",
                  "hfdata_day20190226.csv",
                  "hfdata_day20190227.csv",
                  "hfdata_day20190228.csv",
                  "hfdata_day20190301.csv",
                  "hfdata_day20190306.csv",
                  "hfdata_day20190307.csv",
                  "hfdata_day20190308.csv",
                  "hfdata_day20190311.csv",
                  "hfdata_day20190312.csv",
                  "hfdata_day20190313.csv",
                  "hfdata_day20190314.csv",
                  "hfdata_day20190315.csv",
                  "hfdata_day20190318.csv",
                  "hfdata_day20190319.csv",
                  "hfdata_day20190320.csv",
                  "hfdata_day20190321.csv",
                  "hfdata_day20190322.csv",
                  "hfdata_day20190325.csv",
                  "hfdata_day20190326.csv",
                  "hfdata_day20190327.csv",
                  "hfdata_day20190328.csv",
                  "hfdata_day20190329.csv",
                  "hfdata_day20190401.csv",
                  "hfdata_day20190402.csv",
                  "hfdata_day20190403.csv",
                  "hfdata_day20190404.csv",
                  "hfdata_day20190405.csv",
                  "hfdata_day20190408.csv",
                  "hfdata_day20190409.csv",
                  "hfdata_day20190410.csv",
                  "hfdata_day20190411.csv",
                  "hfdata_day20190412.csv",
                  "hfdata_day20190415.csv",
                  "hfdata_day20190416.csv",
                  "hfdata_day20190417.csv",
                  "hfdata_day20190418.csv",
                  "hfdata_day20190422.csv",
                  "hfdata_day20190423.csv",
                  "hfdata_day20190424.csv",
                  "hfdata_day20190425.csv",
                  "hfdata_day20190426.csv",
                  "hfdata_day20190429.csv",
                  "hfdata_day20190430.csv",
                  "hfdata_day20190502.csv",
                  "hfdata_day20190503.csv",
                  "hfdata_day20190506.csv",
                  "hfdata_day20190507.csv",
                  "hfdata_day20190508.csv",
                  "hfdata_day20190509.csv",
                  "hfdata_day20190510.csv",
                  "hfdata_day20190513.csv",
                  "hfdata_day20190514.csv",
                  "hfdata_day20190515.csv",
                  "hfdata_day20190516.csv",
                  "hfdata_day20190517.csv",
                  "hfdata_day20190520.csv",
                  "hfdata_day20190521.csv",
                  "hfdata_day20190522.csv",
                  "hfdata_day20190523.csv",
                  "hfdata_day20190524.csv",
                  "hfdata_day20190527.csv",
                  "hfdata_day20190528.csv",
                  "hfdata_day20190529.csv",
                  "hfdata_day20190530.csv",
                  "hfdata_day20190531.csv",
                  "hfdata_day20190603.csv",
                  "hfdata_day20190604.csv",
                  "hfdata_day20190605.csv",
                  "hfdata_day20190606.csv",
                  "hfdata_day20190607.csv",
                  "hfdata_day20190610.csv",
                  "hfdata_day20190611.csv",
                  "hfdata_day20190612.csv",
                  "hfdata_day20190613.csv",
                  "hfdata_day20190614.csv",
                  "hfdata_day20190617.csv",
                  "hfdata_day20190618.csv",
                  "hfdata_day20190619.csv",
                  "hfdata_day20190621.csv",
                  "hfdata_day20190624.csv",
                  "hfdata_day20190625.csv",
                  "hfdata_day20190626.csv",
                  "hfdata_day20190627.csv")

#import data and chose the best time ----------------------------------
#create data frame to hold realized volatility variables
l1aux <- length(files_import)
l2aux <- length(tickers)
hf_data <- data.frame(matrix( rep(0,l1aux*(1+l2aux)),nrow=l1aux))
names(hf_data)[1] <- c('Date')
#import csv edited from package GetHFData
#and run the loop
#file1 <- files_import[1]
for (file1 in files_import) {
  pos2aux <- which(file1==files_import)
  aux1 <- fread(paste(path, file1,sep = ""))
  aux_tickers <- aux1$InstrumentSymbol
  price <- aux1$last.price
  #we can take only one of the dates because it is all the same
  date1 <- aux1$SessionDate[1]
  #input date
  hf_data[pos2aux,1] <- date1
  
  #ticker1 = tickers[1]
  for (ticker1 in tickers) {
    aux_loop1 <- ticker1 == aux_tickers
    price_aux1 <- price[aux_loop1]  
    price_log <- log(price_aux1)
    price_diff1 <- diff(price_log)
    price_diff2 <- price_diff1^2
    vol_f1 <- sum(price_diff2) 
    pos1aux <- which(ticker1==tickers)
    hf_data[pos2aux, pos1aux+1] <- vol_f1
    
    #give names to columns of hp_data once, on the first time
    if (pos2aux==1) {
      names(hf_data)[pos1aux+1] <- ticker1
    }
  }
}
#save as csv out
name1 <- paste(path1,"hf_data.csv",sep='')
write.csv(hf_data, name1)

#in another script import and transform it into a xts


#finis --------------------------------------------------------------------





