library(openxlsx)

#zad 3

N=50+1*20
samp_size=10000
alpha=0.05

gen_process=function()
{
    y=0
    for (i in 2:N)
    {
        y[i]=y[i-1]+rnorm(1,0,0.1)
    }
    return(y)
}


get_t_stat=function()
{
    y=gen_process()
    # browser()
    x=lag(y)[-1]
    dy=diff(y)
    model1=lm(dy~x)
    return(summary(model1)$coefficients[2,3])
}

t_sample=sapply(1:samp_size, function(x) get_t_stat())
answer=t_sample[round((1-alpha)*samp_size)]

print(paste("critic value =", answer))

#end zad 3

