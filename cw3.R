#cw3
library(dplyr)
library(xlsx)
data = read.xlsx("data/Zaj2_Regresje_poz.xlsx", 1, colIndex = 1:3, stringsAsFactors=F) %>%
    slice(-1)
colnames(data) = c("rok", "las", "dlugosc")
head(data)
data$las%>%as.character%>%as.numeric->data$las
data$dlugosc%>%as.numeric->data$dlugosc
model = lm(las ~ rok, data)
data$las_star=model$residuals
head(data)
model2=lm(dlugosc~rok+las_star, data)
summary(model2)
#####################

N=100
sigma=1
phi=0.9
x=0
szum=rnorm(N,0,sigma)
szum[10]=szum[10]+1
for (i in 2:N)
{
    x[i]=phi*x[i-1]+szum[i]
}

plot(x, type="l", col="black")
lines(szum, col="red")
# legend("topleft", legend=c("Line 1", "Line 2"), col=c("black", "red"), cex=0.8)
