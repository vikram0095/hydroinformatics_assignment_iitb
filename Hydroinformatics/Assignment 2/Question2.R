rm(list=ls())
#install.packages("xlsx")
require("xlsx")
setwd("/home/de/Dropbox/Hydroinformatics/Assignment 2/")
raw_data=read.xlsx("Temp_173040020_26.5_78.5.xls",sheetIndex = 1,header = FALSE)
summary(raw_data)
YearRain=raw_data[ ,c(1,4)]
annav=matrix(c(0),nrow = 34,ncol = 2)

for(i in 1:34){
  ind = which((raw_data[ ,1]==1979+i))
  ind2= which(raw_data[ind,4]>33)
  annav[i,1]=1979+i
  annav[i,2]=length(ind2)
}

#par(mfrow=c(2,2))
dataF=data.frame("Year" = annav[ ,1],"ANMTEMP" = annav[ ,2])
#plot(dataF$Year,dataF$ANMTEMP,xlim = c(1980,2013),ylim = c(0,35))

gfit1=glm(ANMTEMP~1,dataF,family=poisson(link=log))
gfit2=glm(ANMTEMP~1+Year,dataF,family=poisson(link=log))
summary(gfit1)
summary(gfit2)

conf=predict( gfit1, newdata=data.frame( Year= 1980:2013 ) )
#points(1980:2013,exp(conf),pch=19,type='l',col="red")
conf=predict( gfit2, newdata=data.frame( Year= 1980:2013 ) )
#points(1980:2013,exp(conf),pch=19,type='l',col="blue")

logLik(gfit1)
logLik(gfit2)

AIC(gfit1)
AIC(gfit2)
BIC(gfit1)
BIC(gfit2)

qchisq(.99, df=1)
D01=2*(logLik(gfit2)-logLik(gfit1))