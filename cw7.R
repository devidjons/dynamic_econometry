library(openxlsx)
library(dplyr)
library(CADFtest)
library(lmtest)
# take data
data=read.xlsx("Zaj7_Kointegracja.xlsx",sheet=4, startRow = 13)
head(data)
colnames(data)=c("t", "EUR/PLN", "y_t", "dy_t", "y_t1", "dy_t1", "d2y_t", "dy_t1")
#

# model1
model1=lm(dy_t~y_t1+dy_t1,data)
summary(model1)
dwtest(model1)
#


#model2
model2=lm(d2y_t~dy_t1, data)
summary(model2)
dwtest(model2)
#


data2=read.xlsx("Zaj7_Kointegracja.xlsx",sheet=5, startRow = 11)
model3=lm(X2~X3+X4-1,data2)
eps=model3$residuals
data_eps=data.frame(eps)
data3=data_eps
colnames(data3)="e"
data3$de=lead(data3$e)-data3$e
model4=lm(de~e-1, data3)
summary(model4)
dwtest(model4)
