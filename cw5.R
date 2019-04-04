library(openxlsx)
library(CADFtest)
library(dplyr)
library(lmtest)
data=read.xlsx("Zaj5_infl.xlsx", startRow = 3)
# Test DF dla danych o inflacji. One są w formie GUS wyrażonej jako GUS_inf=100*CPI(t)/CPI(t-12). Weźmy logarytmiczne stopy czyli 100*ln(GUS_inf/100). Test robimy w Excelu na danych od 2000 roku. Wypisujemy wnioski, patrzymy na założenia i staramy się oszacować ich poprawność.
data$y=log(data$inflacja.GUS/100)*100
data$ret=lead(data$y)-data$y
data_2000=data%>%filter(r>=2000)
model=lm(ret~y-1, data_2000)
summary(model)
CADFtest(data$ret)
dwtest(model)
