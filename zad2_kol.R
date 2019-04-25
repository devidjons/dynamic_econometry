library(openxlsx)
data=read.xlsx("gr1.xlsx", sheet = 2, startRow = 2)
colnames(data)=c("x",'y')
data$t=1:dim(data)[1]
head(data)
model=lm(y~x,data)
summary(model)
model2=lm(y~t,data)
model3=lm(x~t,data)
summary(model2)
summary(model3)
##w obu modelach widac mocny trent stacjonarny, wiec regresja pomiedzy nimi nie ma sensu 
