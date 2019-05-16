library(dplyr)
library(quadprog)
library(xts)
library(tseries)
library(quantmod)
#16.05.19


#initialization
N=200
a0=0.2
a1=0.3
a2=0.3
start_date="1995-01-01"
end_date = "2015-01-01"
alpha=0.95
#
data = get.hist.quote("ibm", start = start_date, end = end_date, quote="Close", compression = "d")
data = as.matrix(data)
returns = diff(log(data))
rownames(returns)=NULL

z=lm(lead(returns)~returns)$residuals
x1=lag(z)^2
x2=lag(z,2)^2
model  = lm(z~x1+x2)
stat = summary(model)$r.squared*length(z)
qchisq(alpha, 3)
stat
