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
