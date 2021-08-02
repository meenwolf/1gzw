library(tidyverse)
data<- read.csv(file.choose(), sep=",")
view(data)
data.goed<- data%>%
  filter(!is.na(V12_6))
view(data.goed)
nrow(data.goed)
# geen.zin<- filter(data.goed, V12_1 == 1)
# nrow(geen.zin)

data.goed<- data %>%
  filter(! is.na(V12_1))

data.g<- data.goed%>%
  select(V12_1, V12_2, V12_3, V12_4, V12_5, V12_6, V12_7)

een <-data.g[[1]]
twee<- data.g[[2]]
drie<- data.g[[3]]
vier<- data.g[[4]]
vijf<- data.g[[5]]
zes<- data.g[[6]]
zeven<- data.g[[7]]
tabel<- cbind(een, twee, drie, vier, vijf, zes, zeven)
namen<- c("Geen zin", "Geen tijd", "Vervoersproblemen", "Sport niet beschikbaar",
          "Lichamelijke klachten", "Niet gewend", "Sport te duur")
palette <- colorRampPalette(c('red', "yellow", 'darkgreen'))
barplot(tabel, col= "darkgrey", names.arg = namen, xlab = "Reden om niet te sporten",
        cex.lab= 1.4, ylab = "Frequentie",
        main = "De redenen van inactiviteit onder de desbetreffende deelnemers",
        cex.main=1.5, ylim = c(0, 250))
waarde.voor.lk<- data.g%>%
  filter(V12_5 == 1)
nrow(waarde.voor.lk)
hoogste.v7<- data.g %>%
  filter(V7 == 3)
