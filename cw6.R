library(openxlsx)
library(CADFtest)
library(dplyr)
###hyper parameters
year=2002
#


data=read.xlsx("Zaj5_infl.xlsx", startRow = 3)
data$x=log(data$inflacja.GUS/100)*100
data$dx=lead(data$x)-data$x
head(data)
data$dx1=lag(data$dx)
data_filtered=data%>%filter(r>=year)
data_filtered
model=lm(dx~x+dx1, data_filtered)
summary(model)
CADFpvalues(-2.135)
dwtest(model)
# Thu Apr  4 11:36:00 2019 ------------------------------

data$d2x=lead(data$dx)-data$dx
data_filtered2=data%>%filter(r>=year)
model2=lm(d2x~dx-1, data_filtered2)
model2=lm(data_filtered2$d2x~data_filtered2$dx-1)
summary(model2)
CADFpvalues(-10.69)
dwtest(model2)
# Thu Apr  4 11:43:54 2019 ------------------------------

