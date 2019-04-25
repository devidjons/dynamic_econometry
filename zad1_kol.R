library(openxlsx)
data=read.xlsx("gr1.xlsx", sheet=1,startRow = 2)

y=data$Szereg.1



model1=lm(diff(y)~y[-length(y)]-1)
t1=summary(model1)$coefficients[3]
dwtest(model1, alternative = "two.sided")


y=data$Szereg.2

model1=lm(diff(y)~y[-length(y)]-1)
t2=summary(model1)$coefficients[3]
dwtest(model1, alternative = "two.sided")

print(paste(t1,t2))
print("dla obu procesow DF test odrzuca pierwiastek jednostkowy")
