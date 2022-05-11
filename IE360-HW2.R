library(xlsx)
library(lubridate)
library(zoo)
library(ggplot2)
library(RcppRoll)
library(GGally)
library(skimr)
library(forecast)
library(dplyr)
library(readxl)
library(data.table)

salesdata=data.table(read.csv("Documents/R /IE360_Spring22_HW2_data.csv",colClasses=c("character", rep("numeric",10))))
colnames(salesdata) <- c("Quarters", "UGS", "RNUV","NLPG","PU","PG","NUGV","NDGV","GNPA","GNPC","GNPT")
salesdata$Quarters <- as.yearqtr(salesdata$Quarters, format = "%Y_Q%q")
str(salesdata)

ggplot(salesdata, aes(x=Quarters, y=UGS, group=1)) + 
  geom_line()+
  geom_point()+
  ggtitle('Unleaded Gasoline Sales Time Series Plot')

meanUGS=roll_mean(salesdata$UGS,4,align='left')
varUGS=roll_var(salesdata$UGS,4,align='left')
plot(meanUGS,
     type='l',col='magenta',
     xlab = "time (t)",
     ylab = "Rolling Mean",
     main = "Mean series")

plot(varUGS,
     type='l',col='green',
     xlab = "time (t)",
     ylab = "Rolling Variance",
     main = "Variance series")

acf(salesdata$UGS,na.action = na.pass,lag.max = 12)


salesdata <- salesdata[, trend:=(1:.N)]
salesdata <- salesdata[,Lag1:=c(NA, salesdata$UGS[1:31])]

salesdata <- salesdata[,seasonality:=(1:.N)%%4]
salesdata <- salesdata[seasonality==0,seasonality:=4]
salesdata <- salesdata[,Lag4:=c(rep(NA,4),salesdata$UGS[1:28])]

str(salesdata)

ggpairs(salesdata[,2:15])

GNPAadded <- lm(UGS~GNPA, salesdata)  
summary(GNPAadded)

GNPA_NUGVadded <- lm(UGS~GNPA+NUGV, salesdata)  
summary(GNPA_NUGVadded)

GNPA_NUGV_NDGVadded <- lm(UGS~GNPA+NUGV+NDGV, salesdata)  
summary(GNPA_NUGV_NDGVadded)

GNPA_NUGV_NDGV_PGadded <- lm(UGS~GNPA+NUGV+NDGV+PG, salesdata)  
summary(GNPA_NUGV_NDGV_PGadded)

GNPA_NUGV_NDGV_PUadded <- lm(UGS~GNPA+NUGV+NDGV+PU, salesdata)  
summary(GNPA_NUGV_NDGV_PUadded)

TrSeasAdded <- lm(UGS ~ trend+seasonality, salesdata)
summary(TrSeasAdded)

Reg_Tr_Seas_Added <- lm(UGS ~ GNPA+NUGV+NDGV+trend+seasonality, salesdata)
summary(Reg_Tr_Seas_Added)

Reg_Tr_Seas_Lag1_Added <- lm(UGS ~ GNPA+NUGV+NDGV+trend+seasonality+Lag1, salesdata)
summary(Reg_Tr_Seas_Lag1_Added)

Reg_Tr_Seas_Lags_Added <- lm(UGS ~ GNPA+NUGV+NDGV+trend+seasonality+Lag1+Lag4, salesdata)
summary(Reg_Tr_Seas_Lags_Added)


finalmodel<- Reg_Tr_Seas_Lag1_Added
summary(finalmodel)

checkresiduals(finalmodel)

to_be_predicted <- salesdata[29:32,c("GNPA","NUGV","NDGV","trend","seasonality","Lag1")]
prediction = c(0,0,0,0)

prediction[1] = predict(finalmodel,newdata = to_be_predicted[1,])
to_be_predicted[2,"Lag1"] = prediction[1]

prediction[2] = predict(finalmodel,newdata = to_be_predicted[2,])
to_be_predicted[3,"Lag1"] = prediction[2]

prediction[3] = predict(finalmodel,newdata = to_be_predicted[3,])
to_be_predicted[4,"Lag1"] = prediction[3]

prediction[4] = predict(finalmodel,newdata = to_be_predicted[4,])

prediction

finaldata=copy(salesdata)
finaldata$actual=salesdata$UGS
finaldata$predicted=predict(finalmodel,finaldata)
ggplot(finaldata ,aes(x=Quarters))+
  geom_line(aes(y=actual,color='real')) + 
  geom_line(aes(y=predicted,color='predicted'))
