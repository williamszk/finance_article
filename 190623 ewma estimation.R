# 190623 ewma estimation
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

#calculate EWMA by hand ------------------------------------------------------------------------

# a standard specification
spec1 = ugarchspec()
spec1
# an example which keep the ar1 and ma1 coefficients fixed:
spec2 = ugarchspec(mean.model=list(armaOrder=c(2,2),
                                   fixed.pars=list(ar1=0.3,ma1=0.3)))
spec2
# an example of the EWMA Model
spec3 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                   distribution.model="norm", fixed.pars=list(omega=0))



















#finis -------------------------------------



gdata::keep(basets,sure=TRUE)








