library(lattice)

#
N=100
alpha=1
phi=c(0.7,-0.2)
beta=c(-0.1,0.3)
sigma=0.2
eps=rnorm(N,0,sigma)
x=c(0,0)
#

phi_n=length(phi)
beta_n=length(beta)

t=1:N

for (i in (length(x)+1):N)
{
    x[i]=alpha+sum( x[i - 1:phi_n]*phi)+sum( eps[i+1-1:beta_n])
}

E_warunk=alpha*(1-phi^t)/(1-phi)
E_est=cumsum(x)/t

E=alpha/(1-sum(phi))
V=sigma^2*(1+sum(beta^2))/(1-sum(phi^2))

range=E+c(-1,1)*V^(1/2)*1.96
plot(t,x, type="l")
abline(h=range[1], col="red")
abline(h=range[2], col="red")
plot(t,E_warunk, type="l", ylim = c(-0.5,5))
lines(t,E_est, col="red")




#praca domowa

#
N=15
A=0.5
sigma=0.2
sample_N=1000
#
get_A=function()
{
    eps=rnorm(N,0,sigma)
    x=0
    for (i in (length(x)+1):N)
    {
        x[i]=A*x[i-1]+eps[i]
    }
    n=length(x)
    
    lm(x[-1]~x[-n]+0)$coefficients[1]
}

A_sample=sapply(1:sample_N, function(x) get_A())
names(A_sample)=NULL
hist(A_sample)
print(t.test(A_sample)$conf.int[1:2])