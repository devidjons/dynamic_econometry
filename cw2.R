n <- 5000
k <- 100
Stat_t1 <- numeric(n)
for (i in 1:n){
    X <- rnorm(k,2,2)
    Y <- rnorm(k,1,1)
    Stat_t1[i] <- summary(lm(Y~X))$coefficients[2,3]
}

hist(Stat_t1, breaks=42, freq=F)
t <- (-40:40)/10
lines(t, dnorm(t))


x=rnorm(k)
y=0.7*x+rnorm(k)
x_star=cumsum(x)
y_star=cumsum(y)
model1=lm(y_star~x_star)
summary(model1)
for (e in 1:3)
{
    i=0:3
    j=0:3
    m1=matrix(rep(e*i,4),4,4)
    m2=t(matrix(rep(j,4),4,4))
    print((m1+m2) %%4)
}

