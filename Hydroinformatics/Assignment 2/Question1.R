rm(list=ls())
#install.packages("xlsx")
require("xlsx")
setwd("/home/de/Dropbox/Hydroinformatics/Assignment 2/")
raw_data=read.xlsx("Temp_173040020_26.5_78.5.xls",sheetIndex = 1,header = FALSE)
dev.new()

summary(raw_data)
YearRain=raw_data[ ,c(1,4)]
annav=matrix(c(0),nrow = 34,ncol = 2)

for(i in 1:34){
  ind = which(raw_data[ ,1]==1979+i)
  annav[i,1]=1979+i
  annav[i,2]=mean(YearRain[ind,2])
}
dataF=data.frame("Year" = annav[ ,1],"ANMTEMP" = annav[ ,2])
mfit=lm(ANMTEMP~Year,dataF)
summary(mfit)
summary(mfit)$r.squared 
names(summary(mfit))
conf=predict( mfit, data.frame( Year= 1980:2013 ) ,interval ="confidence" , level = 0.99 )
pred=predict( mfit, data.frame( Year= 1980:2013 ) ,interval ="prediction" , level = 0.99 )


plot(annav[ ,1],annav[ ,2],xlim = c(1980,2013),ylim = c(25,40),xlab="Years",ylab = "Annual Mean Temperatures(°C)",main="Regression Plot")


#plot  fit line
abline (mfit , lwd =1,col="red")

#plot confidence interval
lines(1980:2013,conf[ ,2],col="black",lwd=1)
lines(1980:2013,conf[ ,3],col="black",lwd=1)

#plot prediction interval
lines(1980:2013,pred[ ,2],col="black",lwd=1,lty=4)
lines(1980:2013,pred[ ,3],col="black",lwd=1,lty=4)
legend("topright",c("Prediction interval(99%)","Confidence interval(99%)","Fitted Line"),lty=c(4,1,1),lwd=c(1,1,1),col=c("black","black","red")) 

dev.new()
x=seq(-4,4,0.1)
plot(annav[ ,1],annav[ ,2],xlim = c(1980,2013),ylim = c(25,35),xlab="Years",ylab = "Annual Mean Temperatures(°C)",main="Regression Plot")
#plot(x,dnorm(x,0,summary(mfit)$sigma),type = 'l',xlim=c(-4,4),ylim=c(0,0.4))
polygon(1982+5*c(0,dnorm(x,0,summary(mfit)$sigma),0),pred[2,1]+c(-4, x, 4), col='gray95')
polygon(1990+5*c(0,dnorm(x,0,summary(mfit)$sigma),0),pred[10,1]+c(-4, x, 4), col='gray95')
polygon(2000+5*c(0,dnorm(x,0,summary(mfit)$sigma),0),pred[20,1]+c(-4, x, 4), col='gray95')
abline (mfit , lwd =1,col="red")
dev.new()
plot(pred[ ,1],summary(mfit)$residuals,xlab="Predicted Values",ylab = "Residual Error",main="Residual Error")

gfit=glm(ANMTEMP~1+Year,dataF,family=gaussian(link=identity))
summary(gfit)



