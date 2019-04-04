#hyper parameters
N=1000
ksi=0
phi=0.9
m=0.5
beta=0.7
#
for(i in 2:N)
{
    ksi[i]=phi*ksi[i-1]+rnorm(1)
}
t=1:N

y=m+beta*t+ksi