# Wed Apr 10 23:29:09 2019 ------------------------------
#zadanie domowe 2
library(openxlsx)
library(dplyr)
library(quantmod)
library(xts)
library(tseries)
data=read.xlsx("Zaj2_Regresje_poz.xlsx", startRow = 14)%>%select(3)
data$t=1:dim(data)[1]
colnames(data)=c("y", "t")
data$dy=lead(data$y)-(data$y)
model1=lm(data$dy~data$t+data$y)
summary(model1)

data2=get.hist.quote("AAPL")%>%as.data.frame%>%select(Close)
data2
