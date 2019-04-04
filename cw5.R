library(openxlsx)
library(CADFtest)
data=read.xlsx("Zaj5_infl.xlsx", startRow = 3)
# Test DF dla danych o inflacji. One są w formie GUS wyrażonej jako GUS_inf=100*CPI(t)/CPI(t-12). Weźmy logarytmiczne stopy czyli 100*ln(GUS_inf/100). Test robimy w Excelu na danych od 2000 roku. Wypisujemy wnioski, patrzymy na założenia i staramy się oszacować ich poprawność.
plot(data[data$r>2000,3])
data$ret=log(lead(data$inflacja.GUS,12)/data$inflacja.GUS)
data_2000=data%>%filter(r>=2000)
plot(data_2000$ret)
CADFtest(data_2000$ret)
