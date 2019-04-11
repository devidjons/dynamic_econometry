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

data2 = get.hist.quote("AAPL", start = "2009-01-01", compression = "m") %>% as.data.frame %>%
    mutate(y = log(Close)) %>% select(y)
data2
rownames(data2) = 1:dim(data2)[1]
data2$t = 1:dim(data2)[1]
head(data2)
data2$dy = lead(data2$y) - data2$y
model2 = lm(dy ~ y + t, data2)
summary(model2)
plot(data2$y, type="l")
