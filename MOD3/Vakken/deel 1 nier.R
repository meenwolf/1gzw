library(tidyverse)

file.choose()
data<-read.csv("C:\\Users\\User\\OneDrive\\Documenten\\MOD3\\cvs bestanden\\Data Niertransplantatie College 2.csv", sep = ",")
view(data)
data<- data%>%
  filter(! ID %in% c(16,25,33,14,22,30))

table(data$geslacht)



data %>%
  summarise( uitschieter = max(leeftijd))

data$leeftijd[data$leeftijd== 550]<- NA

data %>%
  ggplot(aes( x=leeftijd))+
  geom_histogram()

summarise(data, laagste = min(leeftijd, na.rm = T))

data$leeftijd[data$leeftijd== 3.2]<- NA
data %>%
  ggplot(aes( x=leeftijd))+
  geom_histogram()

table(data$aantal_transplantaties)

data%>%
  group_by(aantal_transplantaties)%>%
  summarise( n=n())

data%>%
  ggplot(aes(x=tijd_wachtlijst))+
  geom_histogram()
data%>%
  summarise(aanta_NA= sum(is.na(tijd_wachtlijst)))

data%>%
  summarise(sdtrans= sd(aantal_transplantaties, na.rm=T))

data%>%
  summarise(gemtrans= mean(aantal_transplantaties, na.rm=T))

data%>%
  summarise(sum(geslacht == "2"))
library(tidyverse)
data1<- data%>%
  mutate(hads7_rec = abs(hads7 - 3), 
         hads10_rec =  abs(hads10 - 3))

view(data2)
data2 <- data1%>% 
  mutate(depressiescore = (hads1+ hads3+ hads5+ hads7_rec+ hads9+ hads10_rec+ hads13))%>%
  mutate(angstscore = (hads2+ hads4+ hads6+ hads8+ hads11+ hads12+ hads14))
  
data3<- mutate(data2, depcat= cut(depressiescore,
                     breaks= c(0, 7, 10, 21),
                     labels= c("geen_depressie", "mogelijke_depressie", "vermoedelijke_depressie")))
view(data3)
md<- data3 %>%
filter(depcat == "mogelijke_depressie")
nrow(md)
vd<- data3%>%
  filter(depcat =="vermoedelijke_depressie")
nrow(vd)
# van de mensen die op de wachtlijst voor een niertranspantatie zijn 
# ervaren 41 mensen mogelijk een depressie en 20 mensen
# vermoedelijk een depressie
# er kan niet worden gesproken over definitieve depressieklachten, enkel een vermoeden van
# er kan dus ook niet gesteld worden hoeveel mensen depressieklachten ervaren die op de wachtlijst staan.
# de vragen die gesteld zijn in het onderzoek kunnen enkel een indicatie geven.
# hieruit blijkt dat mogelijk de helft van alle ondervraagden depressieklachten ervaren en een vierde vermoedelijk een depressie heeft
#
deel1<- data3%>%
  filter(leeftijd<=55)%>%
  select( tijd_wachtlijst, depcat)
view(deel1)

deel4<-data3%>%
  filter(leeftijd<=55)

deel5<- sort(deel4$tijd_wachtlijst, decreasing= T)
select(deel5, tijd_wachtlijst, depcat)
view(deel4)
deel2<- sort(deel1$tijd_wachtlijst, decreasing= TRUE)
view(deel2)
# bij mensen van 55 jaar of jonger zien we dat dat mensen die het kortste
# op de wachtlijst staan over het algemeen een hogere depressiescore hebben dan
# de mensen die langer op de wachtlijst staan. ik heb in de tabel op de pijltjes
# bij tijd_wachtlijst geklikt, hoe langer de mensen op de wachtlijst staan,
# hoe vaker er "geen depressie"geconstateerd wordt(ahv het stukje code hieboven)

del1<- data3 %>%
  filter(leeftijd >= 55)%>%
  select(tijd_wachtlijst, depcat)
view(del1)
# bij de mensen die 55 jaar of ouder zijn heb ik aan de hand van het stukje code
#hierboven, kunnen constateren, door op de pijltjes in de tabel te klikken, 
# zodat de leeftijd gesorteerd staat(deed het niet met group_by, wel in de
# console, maar niet in de tabel) dat de mensen met deze leeftijd vaker 
# "geen depressie" geconstateerd kregen( ahv code opg.5) als ze korter op de
# wachtlijst stonden. Dit betekent dus dat ouderen vaker een depressie kunnen
# ervaren als zij langer op de wachtlijst staan dan mensen die korter op de 
# wachtlijst staan
nier1 <-data3 %>%
  filter(leefijd <= "55")
