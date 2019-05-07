library(openxlsx)
library(CADFtest)
library(dplyr)
data=read.xlsx("gr1.xlsx", sheet=3, startRow = 2)
head(data)
wig20=data[,c(1,5)]
pko=data[,c(7,11)]
head(wig20)
head(pko)

#check I(1) dla wig
y=log(wig20$Zamkniecie)
dy=diff(y)
lm(y~lag(y))

model=lm(dy~head(y,-1))

dwtest(model, alternative = "two.sided") #przyjmujemy ze nie ma autokorelacji

DF_stat=summary(model)$coefficients[2,3]
print(paste(DF_stat,"> -2,88"))
#brak podstaw do odrzucenia, czyli I(1)

#check I(1) dla pko

y=log(pko$Zamkniecie)
dy=diff(y)
lm(y~lag(y))

model=lm(dy~head(y,-1))

dwtest(model, alternative = "two.sided") #przyjmujemy ze nie ma autokorelacji
DF_stat=summary(model)$coefficients[2,3]
print(paste(DF_stat,"> -2,88"))
#brak podstaw do odrzucenia, czyli I(1)

x=log(pko$Zamkniecie)
y=log(wig20$Zamkniecie)
model2=lm(x~y-1)

eps=model2$residuals
data_eps=data.frame(eps)
data3=data_eps
colnames(data3)="e"
data3$de=lead(data3$e)-data3$e
model4=lm(de~e-1, data3)
summary(model4)
dwtest(model4, alternative = "two.sided")#brak autokorelacji
DF_stat=summary(model4)$coefficients[3]

print("nie ma stacjonarnosci reszt, wiec procesy nie sa kointegrowane")
