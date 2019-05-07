library(openxlsx)
library(lmtest)
data=read.xlsx("gr1.xlsx", sheet = 2, startRow = 2)
colnames(data)=c("x",'y')
data$t=1:dim(data)[1]
head(data)
model=lm(y~x,data)

dwtest(model, alternative = "two.sided")
print("nie mamy podstaw do odrzucenia 0 autokorelacji")
summary(model)
print("model ma dobre statystyki istotności, więc regresja nie wygląda pozorną")
############

print("dla pewności można skonstruować model bez trendu deterministycznego")
res1=lm(x~t, data)$residuals
res2=lm(y~t, data)$residuals
model2=lm(res2~res1)
summary(model2)
dwtest(model2, alternative = "two.sided")
print("model nadal ma sens, więc jesteśmy jeszcze bardziej pewni że regresja nie jest pozorna")
print("dla jeszcze większej pewności można byłoby zbadać kointegrację")