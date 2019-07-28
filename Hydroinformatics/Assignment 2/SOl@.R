#rm(list=ls())
#install.packages("xlsx")
#require("xlsx")
setwd("/home/de/Dropbox/Hydroinformatics/Assignment 2/")
raw_data=read.xlsx("Temp_173040020_26.5_78.5.xls",sheetIndex = 1,header = FALSE)
summary(raw_data)
YearRain=raw_data[ ,c(1,4)]
annav=matrix(c(0),nrow = 34,ncol = 2)

for(i in 1:34){
  ind = which(raw_data[ ,1]==1979+i)
  annav[i,1]=1979+i
  annav[i,2]=mean(YearRain[ind,2])
}

#par(mfrow=c(2,2))
plot(annav[ ,1],annav[ ,2],xlim = c(1980,2013),ylim = c(25,35))
dataF=data.frame("Year" = annav[ ,1],"ANMTEMP" = annav[ ,2])

mfit=lm(ANMTEMP~Year,dataF)
summary(mfit)
summary(mfit)$r.squared 
names(summary(mfit))
abline ( mfit , lwd =3)


conf=predict( mfit, data.frame( Year= 1980:2013 ) ,interval ="confidence" , level = 0.99 )
pred=predict( mfit, data.frame( Year= 1980:2013 ) ,interval ="prediction" , level = 0.99 )

lines(1980:2013,conf[ ,2],col="red",lwd=3)
lines(1980:2013,conf[ ,3],col="red",lwd=3)

lines(1980:2013,pred[ ,2],col="blue",lwd=3)
lines(1980:2013,pred[ ,3],col="blue",lwd=3)

plot(pred[ ,1],summary(mfit)$residuals)

gfit=glm(ANMTEMP~1+Year,dataF,family=gaussian)

