#190628 high frequency data
#preliminaries -----------------------------------------------------------------------------------
#this script uses the package GetHFData to download and export to csv the 
#high frequency code with 5 min frequency of the Brazilian stock exchange.

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

#code to download all the necessary data:--------------------------------------------------------------
#5 mins frequency

#get dates correctly:
#path2 <- "C:/Users/willi/Desktop/working/RAW_DATA/saved_RData/"
#load(paste(path2,"190626_finance_article.RData",sep=''))
#dates_vector <- index(basets["20180706/"])

#get the tickers
my.tickers <-ghfd_get_available_tickers_from_ftp(as.Date('2018-07-02'))
ticker_names <- as.character(my.tickers[,1]) 

#create objects to store information

dates_vector <- as.Date("2018-07-02") + 0:370

#create path of high frequency repository
path_high_frequency <- "C:/Users/willi/Desktop/working/RAW_DATA/high_frequency"


for (date_aux in dates_vector[-(1:6)]) {
  #date_aux <- dates_vector[358]
  try(out_df <- ghfd_get_HF_data(my.assets=ticker_names, 
                          type.market='equity' , 
                          first.date= as.Date(date_aux), 
                          last.date= as.Date(date_aux),
                          type.data = 'trades',
                          agg.diff = "5 mins", 
                          type.output="agg",
                          first.time = "10:00:00", 
                          last.time = "17:00:00"), silent=TRUE  )
  if (exists("out_df")) {
    date1 <- as.character(as.Date(date_aux))
    date2 <- paste(substr(date1,1,4),substr(date1,6,7),substr(date1,9,10),sep='')
    name1 <- paste(path_high_frequency,'/hfdata_day',date2,'.csv',sep = '')
    write.csv(out_df, name1)
    rm(out_df) #reset for the loop
  }
}


#to read the csv
#library(data.table)
#system.time(fread('../data/2008.csv', header = T, sep = ',')) 



#finis -------------------------------------------------------------------------------------------------


#path2 <- "C:/Users/willi/Desktop/working/RAW_DATA/saved_RData/"
#gdata::keep(basets,high_frequency,sure=TRUE)
##
#save.image(paste(path2,"190628_finance_article.RData",sep=''))
#how to load an environment in RStudio:
#path2 <- "C:/Users/willi/Desktop/working/RAW_DATA/saved_RData/"
#load(paste(path2,"190626_finance_article.RData",sep=''))
































