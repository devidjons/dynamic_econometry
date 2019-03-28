library(openxlsx)
w1=read.xlsx("Zaj4_AR.xlsx")[,3]
x = as.numeric(w1[5780:length(w1)])
plot(x)
acf(x)
res=residuals(arima(x, c(1,0,0)))


# Thu Mar 28 11:08:22 2019 ------------------------------
library(CADFtest)
