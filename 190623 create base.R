#190623 import economatica

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

path <- "C:/Users/willi/Desktop/working/RAW_DATA/Economatica/"

#vector of excels###################
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

#create base -------------------------------------------------------------------------
#date2 <- as.Date(date, origin = "1899-12-30")
auxdate1 <- as.Date(format(seq(as.Date("1995-01-01"), as.Date("2019-06-19"), by = "1 day")), "%Y-%m-%d") 
basets <- xts(order.by = auxdate1)
#auxfile1 = files_economatica[2]
for (auxfile1 in files_economatica) {
  auxdf1 <- read_xlsx(paste(path,auxfile1,sep=''), 
                      col_names = TRUE, col_types = c("date", rep('text',8) ), na='', skip = 3)
  names(auxdf1) <- c('date','close','open','min','max','average','quantity','thousand_titles','volume')
  #take only the close variable
  auxts1 <- xts( as.numeric(auxdf1$close), order.by = auxdf1$date)
  basets <- cbind(basets,auxts1)  
  
  sheet1aux <- excel_sheets(path = paste(path,auxfile1,sep=''))
  pos1aux <- which(files_economatica==auxfile1)
  names(basets)[pos1aux] <- sheet1aux
}
#basets[11183:11283,1:10]
#dim(basets)
#plot(basets["1995/"])
#head(basets["1995/"])
base2 <- basets["1995/"]
#analysis1
base3 <- log(base2)
#drop all weekends and holidays
base3core <- coredata(base3)
aux2 <- apply(base3core,1,function(x){sum(is.na(x))==96})
base3_1 <- base3[!aux2,]
#dim(base3_1)
#linear interpolation
base4 <- na.approx(base3_1)
base5 <- diff(base4)
#plot(base5[,10:15])
#anyNA(base5)

base7 <- base5["20080101/20190430"]
plot(base7[,1:5])
aux3 <- apply(coredata(base7),1,function(x){sum(!is.na(x))})
class(aux3)
plot(aux3)
aux4 <- apply(coredata(base7),2,function(x){sum(is.na(x))==0})
plot(aux4)
base8 <- base7[,aux4] #filter for the stocks that have information for all periods
base8[1:5,10:20]
dim(base8)
anyNA(base8)
basets <- base8

#finis -------------------------------------



gdata::keep(basets,sure=TRUE)




