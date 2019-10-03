#190723 create base 2
#preliminaries ---------------------------------------------------------------------------------------
#import datasets from economatica

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
library(stats)
library(base)
library(matrixcalc)

path <- "C:/Users/willi/Desktop/working/RAW_DATA/Economatica/"
#file names ----------------------------------------------------------------------
files_economatica <- c("economatica br malls 190528.xlsx",
                       "economatica bradespar pn 190528.xlsx",
                       "economatica cemig on 190604.xlsx",
                       "economatica copel on 190604.xlsx",
                       "economatica cosan log 190604.xlsx",
                       "economatica crr sa 190604.xlsx",
                       "economatica cvc brasil 190604.xlsx",
                       "economatica cyrela realt 190604.xlsx",
                       "economatica eletrobras on 190604.xlsx",
                       "economatica embraer on 190604.xlsx",
                       "economatica energias br 190604.xlsx",
                       "economatica engie brasil 190604.xlsx",
                       "economatica estacio part 190604.xlsx",
                       "economatica fibria on 190604.xlsx",
                       "economatica gerdau met 190604.xlsx",
                       "economatica gerdau on 190604.xlsx",
                       "economatica gol pn 190604.xlsx",
                       "economatica itausa on 190604.xlsx",
                       "economatica itauunibanco on 190604.xlsx",
                       "economatica jbs on 190604.xlsx",
                       "economatica klabin on 190604.xlsx",
                       "economatica kroton on 190604.xlsx",
                       "economatica localiza on 190604.xlsx",
                       "economatica mrv on 190604.xlsx",
                       "economatica multiplan on 190604.xlsx",
                       "economatica natura on 190604.xlsx",
                       "economatica petrobras on 190604.xlsx",
                       "economatica rumo on 190604.xlsx",
                       "economatica sabesp on 190604.xlsx",
                       "economatica suzano sa 190604.xlsx",
                       "economatica aes tiete 190618.xlsx",
                       "economatica alupar on 190618.xlsx",
                       "economatica arezzo on 190618.xlsx",
                       "economatica azul on 190618.xlsx",
                       "economatica barisul on 190618.xlsx",
                       "economatica carrefour on 190618.xlsx",
                       "economatica cesp on 190618.xlsx",
                       "economatica cia hering 190618.xlsx",
                       "economatica duratex on 190618.xlsx",
                       "economatica iochp maxion 190618.xlsx",
                       "economatica irbbrasil on 190618.xlsx",
                       "economatica light sa 190618.xlsx",
                       "economatica m diasbranco 190618.xlsx",
                       "economatica minerva sa 190618.xlsx",
                       "economatica petrobras on 190618.xlsx",
                       "economatica random part 190618.xlsx",
                       "economatica sao martinho 190618.xlsx",
                       "economatica ser educa 190618.xlsx",
                       "economatica SLC agricola 190618.xlsx",
                       "economatica taesa on 190618.xlsx",
                       "economatica tran paulist 190618.xlsx",
                       "economatica ultrapar on 190618.xlsx",
                       "economatica usiminas on 190618.xlsx",
                       "economatica vale on 190618.xlsx",
                       "economatica weg on 190618.xlsx",
                       "economatica lojas americ on 190604.xlsx",
                       "economatica lojas renner on 190604.xlsx",
                       "economatica magaz luiza on 190604.xlsx",
                       "economatica raia drogasil on 190604.xlsx",
                       "economatica santander br on 190604.xlsx",
                       "economatica sid nacional on 190604.xlsx",
                       "economatica telef brasil on 190618.xlsx",
                       "economatica tim part on 190618.xlsx",
                       "economatica p acucar pn 190604.xlsx",
                       "economatica ambev 190528.xlsx",
                       "economatica b2w 190528.xlsx",
                       "economatica b3 190528.xlsx",
                       "economatica BBAS3 190528.xlsx",
                       "economatica BBAS4 190528.xlsx",
                       "economatica bbseguridade 190528.xlsx",
                       "economatica bradesco 190528.xlsx",
                       "economatica braskem 190604.xlsx",
                       "economatica brf 190604.xlsx",
                       "economatica cielo 190604.xlsx",
                       "economatica copasa 190618.xlsx",
                       "economatica ecorodovias 190604.xlsx",
                       "economatica energisa 190618.xlsx",
                       "economatica equatorial 190604.xlsx",
                       "economatica eztec 190618.xlsx",
                       "economatica fleury 190604.xlsx",
                       "economatica gafisa 190618.xlsx",
                       "economatica hypera 190604.xlsx",
                       "economatica iguatemi 190604.xlsx",
                       "economatica linx 190618.xlsx",
                       "economatica marcopolo 190618.xlsx",
                       "economatica marfrig 190604.xlsx",
                       "economatica odontoprev 190618.xlsx",
                       "economatica portoseguro 190618.xlsx",
                       "economatica qualicorp 190604.xlsx",
                       "economatica sanepar 190618.xlsx",
                       "economatica smiles 190604.xlsx",
                       "economatica sulamerica 190618.xlsx",
                       "economatica totvs 190618.xlsx",
                       "economatica valid 190618.xlsx",
                       "economatica viavarejo 190618.xlsx",
                       "economatica wiz 190618.xlsx")

files_hf <- c("hfdata_day20180702.csv",
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


#create base economatica-------------------------------------------------------------------------
#date2 <- as.Date(date, origin = "1899-12-30")
auxdate1 <- as.Date(format(seq(as.Date("1980-01-01"), as.Date("2019-06-19"), by = "1 day")), "%Y-%m-%d") 
len1 <- length(auxdate1)
len2 <- length(files_economatica) 
matr1 <- matrix(NA,nrow=len1,ncol=len2)
basets <-  xts(matr1,order.by = auxdate1)
names(basets) <- rep('replace',len2)
auxfile1 = files_economatica[1]
for (auxfile1 in files_economatica) {
  auxdf1 <- read_xlsx(paste(path,auxfile1,sep=''), 
                      col_names = TRUE, col_types = c("date", rep('text',8) ), na='', skip = 3)
  pos1aux <- which(files_economatica==auxfile1)
  #class(auxdf1)
  names(auxdf1) <- c('date','close','open','min','max','average','quantity','thousand_titles','volume')
  #take only the close variable
  #auxdf1$date %>% class
  auxts1 <- xts( as.numeric(auxdf1$close), order.by = auxdf1$date)
  #plot(auxts1)
  hol1 <- xts(order.by = auxdate1)
  hol2 <- cbind(hol1,auxts1)
  #plot(hol2)
  #hol2 %>% dim
  #basets %>% dim
  basets[,pos1aux] <- as.numeric(hol2) 
  
  #plot(basets)
  sheet1aux <- excel_sheets(path = paste(path,auxfile1,sep=''))
  names(basets)[pos1aux] <- sheet1aux
  #basets[1:10,1:20]
  print(auxfile1)
}
names(basets) 
basets[1000:1100,1:10]
dim(basets)
plot(basets[,1:10])
plot(basets[,1:10]["1995/"])
head(basets[,1:10]["1995/"])
#cut the base in 1995, the first cut
base2 <- basets["1995/"]
#plot(base2[,1:3])
base3 <- log(base2)
names(base3)
#drop all weekends and holidays
base3core <- coredata(base3)
dim(base3core)
all2 <- dim(base3core)[2]
aux2 <- apply(base3core,1,function(x){sum(is.na(x))==all2})
sum(aux2)
base3_1 <- base3[!aux2,]
dim(base3_1)
#take just after 2013, second cut
base3_2 <- base3_1["20130101/"]
dim(base3_2)
anyNA(base3_2)
plot(base3_2[,1:10])
base3_2[1:10,1:10]
#base3_2[1:100,1:90] %>% View
aux1_na <- apply(coredata(base3_2),2,anyNA)
aux1_na; sum(aux1_na); sum(aux1_na)/length(aux1_na)
#linear interpolation
base4 <- na.approx(base3_2)
base4[50:100,50:60] %>% View
anyNA(base4)
class(base4)
dim(base4)
#create base with log difference
base5 <- diff(base4)
#plot(base5[,10:15])
anyNA(base5)
#see how many stocks are present in all time period between "20130101/"
aux3 <- apply(coredata(base5),1,function(x){sum(!is.na(x))})
class(aux3)
plot(aux3)
aux3[1:10]
#see which tickers are present from the beginning:
#base5[1:100,1:50] %>% View
base6 <- base5[-(1:2),] #drop the first periods because they are missing
aux4_0 <- !is.na(coredata(base6)[1,])
sum(aux4_0) #all periods
#filter for the stocks that have information for all periods
base7 <- base6[,aux4_0] 
dim(base7)
base7[1:5,10:20]
plot(base7[,10:20])
anyNA(base7)
#drop the missing which are the last in the table
tail(base7,20)
dim(base7)
ho1 <- apply(base7,2,function(x){sum(is.na(x))})
ho1
#the last 15 missings seems to be due to difference in gathering data
#use the ticker 1
dr1 <- is.na(base7[,1])   #drop 1
base8 <- base7[!dr1,] #the first ticker have all 
dim(base8) #there is 15 of difference
tail(base8,20)
#count the number of na in each variable
nna <- apply(base8,2,function(x) sum(is.na(x)))
nna
nna>0
nna[nna>0]
#some tickers still have missings
pos1 <- which(nna!=0) #which do not have zero missings
pos1
#drop tickers that have any missing
base10 <- base8[,-pos1]
#verify the number of missings in each variable, again
nna3 <- apply(base10,2,function(x) sum(is.na(x)))
nna3 #no more missings
sum(nna3)
#rename the final base
basets <- base10
plot(basets[,1])
basets %>% class
#just leave the basets 

#realized volatility ----------------------------------
tickers <- names(basets)
path2 <- "C:/Users/willi/Desktop/working/RAW_DATA/high_frequency/"
#create data frame to hold realized volatility variables
l1aux <- length(files_hf)
l2aux <- length(tickers)
hf_data <- data.frame(matrix( rep(NA,l1aux*(1+l2aux)),nrow=l1aux))
dim(hf_data)
names(hf_data)[1] <- c('date')
#import csv edited from package GetHFData and run the loop
file1 <- files_hf[1]
for (file1 in files_hf) {
  pos2aux <- which(file1==files_hf)
  aux1 <- fread(paste(path2, file1,sep = ""))
  #aux1[1:10,1:10]
  aux_tickers <- aux1$InstrumentSymbol
  price <- aux1$last.price
  #we can take only one of the dates because it is all the same
  date1 <- aux1$SessionDate[1]
  #input date
  hf_data[pos2aux,1] <- date1
  #hf_data[1:10,1:20]
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
    #give names to columns of hf_data once, on the first time
    if (pos2aux==1) {
      names(hf_data)[pos1aux+1] <- ticker1
    }
  }
  print(file1)
}
class(hf_data)
hf_data[1:10,1:5]
#transform into a xts
hf1 <- hf_data[,-1]
date2 <- as.Date(hf_data$date, format = "%Y-%m-%d") 
hf_data2 <- xts(hf1,order.by = date2)
hf_data <- hf_data2 #final assign 
#class(hf_data)
gdata::keep(basets, hf_data, daily_cov, hf_cov, vech_daily,vech_hf,sure=TRUE)
#preparing datasets to match tickers-----------------------------------------------------
#find the lines that are only zero in hf_data
anyNA(hf_data)
#no missing, but ENGI3 only have zeros inside
lng1 <- dim(hf_data)[1]
aux1_zeros <- !apply(hf_data, 2, function(x){sum(x==0)==lng1})
hf_data$ENGI3
which(aux1_zeros==FALSE)
sum(aux1_zeros)
dim(hf_data)
names(hf_data)
names_aux1 <- names(aux1_zeros)[aux1_zeros]
names_aux1 %>% length
aux1_zeros %>% length
#keep only those variables that have all information
hf_data2 <- hf_data[,names_aux1]
hf_data2 %>% dim
hf_data %>% dim
hf_data %>% class
#clean the basets too from the variables that are all zero in hf_data
class(names_aux1)
names2 <- names(basets)
names3 <- intersect(names2,names_aux1)
length(names3)
basets2 <- basets[,names3] 
hf_data3 <- hf_data2[,names3] 
identical(names(hf_data3),names(basets2)) #the names match 
hf_data <- hf_data3 #reassing
basets <- basets2 #reassing

#list of cov matrices of daily returns -------------------------------
#create a list to input the covariance matrices
daily_cov <- list()
dim1 <- dim(basets2)
ii <- 1
for (ii in 1:dim1[1]) {
  vecc1 <- as.numeric(basets2[ii,]) 
  mat1 <- vecc1 %*% t(vecc1)
  daily_cov[[ii]] <- mat1
}
#just to see if it is ok
ii <- 1 ; jj <- 2
vv1 <- sapply(daily_cov,function(x){x[ii,jj]})
plot(vv1, type='l')
#some tests with the covariances of daily returns
versym1 <- sapply(daily_cov, function(x){isSymmetric(x)})
versym1 %>% sum
dim1[1] #all matrices are symmetric
anyNA(daily_cov) #there are not missings

#list for realized covariance matrices -------------------------------------
#create list for realized covariance matrices, create hf_dis
basets %>% dim
tickers <- names(basets) #take the tickers from basets2 which are the same of hf
l1 <- length(tickers)+2
hf_dis <- data.frame(matrix(rep(NA,l1), ncol=l1)) #data frame for high frequency disaggregated
hf_dis %>% dim
names(hf_dis) <- c('date','time',tickers)
names(hf_dis)
file1 <- files_hf[84] #specially 84 has more time periods inside the day
aux0 <- fread(paste(path2, file1,sep = ""))
t1 <- tickers[1]
aux2 <- aux0[t1==aux0$InstrumentSymbol,]
sedate0 <- aux2$TradeDateTime #session date
sedate <- substr(sedate0,12,16)
leng2 <- length(sedate)
file1 <- files_hf[1]
for (file1 in files_hf) {
  aux1 <- fread(paste(path2, file1,sep = ""))
  date1 <- aux1$SessionDate[1]
  prices <- aux1$last.price
  #create data frame to store high frequency information
  taux1 <- data.frame(matrix(rep(NA,l1*leng2),nrow=leng2)) #table auxiliary 1
  #taux1 %>% dim
  names(taux1) <- names(hf_dis)
  taux1$time <- sedate
  taux1$date <- date1
  name1 <- tickers[1]
  for (name1 in tickers) {
    p1 <- which(name1==tickers) #position 
    ver1 <- sum(name1==aux1$InstrumentSymbol) #verifier
    if (ver1!=0) {
      prices2 <- prices[name1==aux1$InstrumentSymbol]
      lenp <- length(prices2) #length price
      vaux1 <- leng2-lenp + 1
      taux1[vaux1:leng2, p1+2] <- prices2
    }
  }
  hf_dis <- rbind(hf_dis,taux1)
  print(file1)
}
hf_dis <- hf_dis[-1,]
dim(hf_dis)
names(hf_dis)
#transform hf_dis into a xts
hf_dis2 <- hf_dis[,-c(1,2)]
dim(hf_dis2)
#View(hf_dis2)
datetime <- paste(hf_dis$date,hf_dis$time)
datet1 <- as.POSIXct(datetime)
datet1 %>% class
hf_dis2 %>% dim
length(datet1)
hf_dis3 <- xts(hf_dis2, order.by=datet1)
hf_dis3 %>% class
hf_dis3 %>% dim
plot(hf_dis3[,4])
plot(hf_dis3[,"TRPL3"])
#there are some missings... specially in TRPL3
#interpolate missings, linear interpolation
hf_dis4 <- na.approx(hf_dis3)
hf_dis4[1:100,1:40] %>% View
plot(hf_dis3[,"TRPL3"])
plot(hf_dis4[,"TRPL3"])
plot(hf_dis3[,"CPLE3"])
plot(hf_dis4[,"CPLE3"])
#interpolation, use the last observed value
hf_dis5 <- na.locf(hf_dis4 , fromLast = TRUE) 
plot(hf_dis5[,"TRPL3"])
plot(hf_dis4[,"CPLE3"])
plot(hf_dis5[,"CPLE3"])
#reassign
hf_dis <- hf_dis5



#high frequency covariance matrices -----------------------------------------------
#create hf_cov
#list with the realized covariances, here we use hf_dis from the previous step
hf_cov <- list()
date_time <- index(hf_data) #use hf_data which is by day, not 5min
date <- as.character(date_time)
dd <- 1
for (dd in 1:length(date)) {
  d1 <- date[dd]
  hf_hold1 <- hf_dis[d1]
  #dim(hf_hold1)
  d2<-dim(hf_hold1)[2]
  ii <- 1
  jj <- 1
  matrix1 <- matrix(rep(NA,d2^2),nrow=d2)
  for (ii in 1:d2) {
    vec1 <-  diff(log(hf_hold1[,ii]))
    for (jj in 1:d2) {
      vec2 <- diff(log(hf_hold1[,jj])) 
      sum1 <- vec1*vec2
      if (sum(is.na(sum1))==d2) {print("there are missings")}
      sum2 = sum1[!is.na(sum1)]
      matrix1[ii,jj] <- sum(sum2)
    }
  }
  #matrix1[1:20,1:5]
  hf_cov[[dd]] <- matrix1
  print(dd)
}
length(hf_cov)




#tests on hf_cov list of matrices for high frequency ------------------------

#some tests with the list of realized covariances 
#verify if all matrices are symmetric
versym <- sapply(hf_cov, function(x){isSymmetric(x)})
versym
#they are all symmetric

#build each series of realized covariances
ii <- 1
jj <- 10
dim2 <- dim(hf_cov[[1]])[1]
dim2
ser1 <- sapply(hf_cov, function(x){x[ii,jj]})
plot(ser1, type='l')
holm <- hf_cov[[1]]

#verify in each series if there are any missings besides he initial value
holdna1 <-c()
ii <- 1; j <- 1 
for (ii in 1:dim2 ) {
  for (jj in 1:dim2 ) {
    ser1 <- sapply(hf_cov, function(x){x[ii,jj]})
    v1 <- anyNA(ser1[-1])    
    holdna1 <- append(holdna1,v1) 
  }}
holdna1
holdna1 %>% length
75*75
#verify if there are any TRUE values
any(holdna1, na.rm = FALSE)
any(holdna1, na.rm = FALSE)
#there are none
####
#interpolate missing and substitute in the series
#date1 <- index(hf_data)
#hf_cov2 <- hf_cov #build another list to substitute the interpolated values
#len1 <- length(hf_cov2)
#ii <- 1
#jj <- 40
#for (ii in 1:dim2 ) {
#  for (jj in 1:dim2 ) {
#    ser1 <- sapply(hf_cov, function(x){x[ii,jj]})
#    naloc <- is.na(ser1) #na location
#    serts <- xts(ser1, date1)
#    #plot(serts)
#    serts2 <- na.approx(serts)
#    #plot(serts2)
#    nsts <- as.numeric(coredata(serts2))
#    vec1 <- 1:len1
#    poskk <- vec1[naloc]
#    #kk <- 8
#    for (kk in poskk) {
#      hf_cov2[[kk]][ii,jj] <- nsts[kk]
#    }
#    print(paste(as.character(ii),as.character(jj)))
#  }
#}
#anyNA(hf_cov2)
#there are no more NA
#just an example, does it worked?
#ii <- 1
#jj <- 40
#ser1_2 <- sapply(hf_cov2, function(x){x[ii,jj]})
#serts3 <- xts(ser1_2, date1)
#plot(serts3)
#anyNA(hf_cov2)
##reassign the list
#hf_cov <- hf_cov2
#clean objects
gdata::keep(basets, hf_data, daily_cov, hf_cov,sure=TRUE)




#dataset with vech of daily and high frequency data ------------------------
#we use here hf_cov
dim2 <- dim(hf_cov[[1]])[1]
len2 <-  length(hf_cov)
vech_hf <- data.frame(matrix(rep(NA,(dim2*(dim2+1)/2)*len2),nrow=len2))
vech_hf %>% dim
 #create names for columns of vech matrices ===============================
names1 <- names(basets)
d1 <- length(names1)
#just an example
nam1 <- c("A","B","C","D")
paste("A",nam1,sep='_')
paste("B",nam1[-1],sep='_')
paste("C",nam1[-(1:2)],sep='_')

nvech <- c()
kk <- 2
for (kk in 1:length(nam1)) {
  n1 <- nam1[kk]
  if (kk==1) {
    nvech <- append(nvech,paste(n1,nam1,sep='_')) 
  }
  if (kk!=1) {
    nvech <- append(nvech,paste(n1,nam1[-(1:(kk-1))],sep='_'))   
  }
}
nvech
#create vector to hold names
nvech <- c()
kk <- 1
for (kk in 1:length(names1)) {
  n1 <- names1[kk]
  if (kk==1) {
    nvech <- append(nvech,paste(n1,names1,sep='_')) 
  }
  if (kk!=1) {
    nvech <- append(nvech,paste(n1,names1[-(1:(kk-1))],sep='_'))   
  }
}
length(nvech)
tail(nvech)
tail(names1)
nvech
names(basets)



 #continue =============================================================
names(vech_hf) <- nvech
ex1 <- vech(hf_cov[[1]]) 
ex1 %>% length
kk <- 1
for (kk in 1:len2) {
  vech_hf[kk,] <- vech(hf_cov[[kk]]) 
  print(kk)
}
vech_hf %>% class
#transform from data frame to xts
vech_hf <- xts(vech_hf, order.by = index(hf_data))
vech_hf %>% class
jj <- 10
plot(vech_hf[,jj])
#View(vech_hf)
 
#create vech_daily for daily returns ------------------------------------------
dim3 <- dim(daily_cov[[1]])[1] 
len3 <- length(daily_cov)
vech_daily <- data.frame(matrix( rep(NA,(dim3*(dim3+1)/2)*len3)  , nrow=len3) )
vech_daily %>% dim
vech_daily %>% class
#View(vech_daily)
names(vech_daily) <- nvech
kk <- 1
vech(daily_cov[[kk]]) %>% length
for (kk in 1:len3) {
  vech_daily[kk,] <- vech(daily_cov[[kk]]) 
  print(kk)
}
vech_daily %>% class
#transform into a xts
vech_daily <- xts(vech_daily, order.by = index(basets))
vech_daily %>% class
plot(vech_daily[,1]["2017/"])

#verify if there are missings in vech_daily
anyNA(vech_daily)

#standardize dates in vech_daily and vech_hf ---------------------
vech_daily %>% dim
vech_hf %>% dim
hts1 <- cbind(vech_daily,vech_hf) #hole time series
dim(hts1)
nts2 <- xts(order.by = index(hts1))
dim(nts2)
identical(index(vech_daily), index(hst2) )
#they are not the same
vec1 <- cbind(nts2, vech_daily)
vec2 <- cbind(nts2, vech_hf)
anyNA(vec1)
anyNA(vec2)
dim(vec1)
dim(vec2)
#make linear interpolation and then drop observation withou any observation
vi1 <- na.approx(vec1)
vi2 <- na.approx(vec2)
vi1 %>% dim
vi2 %>% dim
anyNA(vi1)
anyNA(vi2)
plot(vi2[,1])
#assign
vech_daily <- vi1
vech_hf <- vi2


#save out ---------------------------------------------------------
gdata::keep(basets, 
            hf_data, 
            daily_cov, 
            hf_cov, 
            vech_daily,
            vech_hf,
            sure=TRUE)
save(list = ls(all=TRUE), file = "finance.RData")


#finis -----------------------------------------------------------------

























