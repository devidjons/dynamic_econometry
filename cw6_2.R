###################
#generate process
# y_t=m+b*t+ksi_t
# where
# ksi_t=phi*ksi_(t-1)+eps
#####################


#hyper parameters
N=500
ksi=0
phi=1
m=1
beta=0.2
#
for(i in 2:N)
{
    ksi[i]=phi*ksi[i-1]+rnorm(1)
}
t=1:N

y=m+beta*t+ksi
plot(t,y, type="l")
abline(m,beta)
