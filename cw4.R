library(openxlsx)
w1=read.xlsx("Zaj4_AR.xlsx")[,3]
x = as.numeric(w1[5780:length(w1)])
plot(x)
acf(x)
res=residuals(arima(x, c(1,0,0)))


# Thu Mar 28 11:08:22 2019 ------------------------------
library(CADFtest)
w2=read.xlsx("Zaj4_AR.xlsx", 2, startRow = 4)

# Thu Mar 28 11:37:52 2019 ------------------------------
w3=read.xlsx("Zaj5_infl.xlsx", startRow = 3)
