library(dplyr)
library(quadprog)
library(xts)
library(tseries)
library(quantmod)
#16.05.19
start_date = "1995-01-01"
end_date = "2015-01-01"


data = get.hist.quote(
    "ibm",
    start = start_date,
    end = end_date,
    quote = "Close",
    compression = "d"
)
data = as.matrix(data)
returns = diff(log(data))
rownames(returns) = NULL


#initialization
N = 200
a0 = 0.2
a1 = 0.3
a2 = 0.3
a3 = 0.2
b0 = 0.4
b1 = 0.35
gamma=0.3
alpha = 0.95
#

# z = lm(lead(returns) ~ returns)$residuals
arch = function(a0=0,a1=0,a2=0,b0=0,b1=0)
{
    w = returns - b0 + b1 * lag(returns)
    z = w
    x1 = lag(z) ^ 2
    x2 = lag(z, 2) ^ 2
    x3 = lag(z, 3) ^ 2
    h = a0 + a1 * x1 + a2 * x2 + a3 * x3
    indices = complete.cases(h) & complete.cases(w)
    h = h[indices]
    w = w[indices]
    t = length(w)
    L = -t / 2 * log(2 * pi) - 1 / 2 * sum(log(h)) - 1 / 2 * sum((w ^ 2) / h)
    return(L)
}

answer = optim(c(a0, a1, a2, b0, b1), function(x)
    - arch(x[1], x[2], x[3], x[4], x[5]), lower= rep(0.00001,5), upper = rep(1,5), method = "L-BFGS-B")$par

answer

garch = function(a0=0,a1=0,b0=0,b1=0, gamma1=0)
{
    w = returns - b0 + b1 * lag(returns)
    z = w
    x1 = lag(z) ^ 2
    #warunek poczatkowy
    h = 0.001    #
    
    for (i in 2:length(x1))
    {
        h[i] = a0 + a1 * x1[i] + a2 * h[i-1]   
    }
    
    indices = complete.cases(h) & complete.cases(w)
    h = h[indices]
    w = w[indices]
    t = length(w)
    L = -t / 2 * log(2 * pi) - 1 / 2 * sum(log(h)) - 1 / 2 * sum((w ^ 2) / h)
    return(L)
}

answer2 = optim(c(a0, a1, b0, b1), function(x)
    - arch(x[1], x[2], x[3], x[4]), lower= rep(0.00001,4), upper = rep(1,4), method = "L-BFGS-B")$par

answer2


