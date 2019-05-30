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
v=3
gamma = 0.3
alpha = 0.95
#

# z = lm(lead(returns) ~ returns)$residuals
arch = function(a0 = 0,
                a1 = 0,
                a2 = 0,
                a3 = 0,
                b0 = 0,
                b1 = 0)
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

answer = optim(c(a0, a1, a2, a3, b0, b1), function(x)
    - arch(x[1], x[2], x[3], x[4], x[5], x[6]), lower = rep(0.00001, 6), upper = rep(1, 6), method = "L-BFGS-B")$par

answer

garch = function(a0 = 0,
                 a1 = 0,
                 b0 = 0,
                 b1 = 0,
                 gamma1 = 0)
{
    w = returns - b0 + b1 * lag(returns)
    w=w[-1]
    z = w
    x1 = lag(z) ^ 2
    #warunek poczatkowy
    h = 0.001    #
    
    for (i in 2:length(x1))
    {
        h[i] = a0 + a1 * x1[i] + gamma1 * h[i - 1]
    }
    
    indices = complete.cases(h) & complete.cases(w)
    h = h[indices]
    w = w[indices]
    t = length(w)
    L = -t / 2 * log(2 * pi) - 1 / 2 * sum(log(h)) - 1 / 2 * sum((w ^ 2) / h)
    return(L)
}

answer2 = optim(c(a0, a1, b0, b1, gamma), function(x)
    - garch(x[1], x[2], x[3], x[4], x[5]), lower = rep(0.00001, 5), upper = rep(1, 5), method = "L-BFGS-B")

answer2

# Thu May 30 10:53:46 2019 ------------------------------


garch_t = function(a0 = 0,
                 a1 = 0,
                 b0 = 0,
                 b1 = 0,
                 gamma1 = 0,
                 v=3
                 )
{
    w = returns - b0 + b1 * lag(returns)
    w=w[-1]
    z = w
    x1 = lag(z) ^ 2
    #warunek poczatkowy
    h = 0.001    #
    
    for (i in 2:length(x1))
    {
        h[i] = a0 + a1 * x1[i] + gamma1 * h[i - 1]
    }
    
    indices = complete.cases(h) & complete.cases(w)
    h = h[indices]
    w = w[indices]
    t = length(w)
    L = t * log((gamma((v + 1) / 2)) / (gamma(v / 2) * sqrt(v * pi)))  -
        1 / 2 * sum(log(h)) -
        (v + 1) / 2 * sum(log(1 + w ^ 2 / (v * h)))
    return(L)
}



answer3 = optim(c(a0, a1, b0, b1, gamma, v),
                function(x) - garch_t(x[1], x[2], x[3], x[4], x[5], x[6]),
                lower = c(rep(0.00001, 5), 2.01),
                upper = c(rep(1, 5), 10 ^ 7),
                method = "L-BFGS-B"
            )

answer3




