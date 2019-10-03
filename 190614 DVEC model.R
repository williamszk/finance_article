# 190614 DVEC model

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


#DVEC model---------------------------------------------------------------------
#DVEC(1,1)
k <- 75
(k^2+k)/2









#finis -------------------------------------------------------------------------
























