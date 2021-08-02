library(tidyverse)
data<- read.csv(file.choose(), sep=",")
datag<- data%>%
  mutate(V1 = factor(V1)) %>%
  mutate(V1= fct_recode(V1,
                        "0-0,5" = "1",
                        "0,5-1" = "2",
                        "1-1,5"= "3",
                        "1,5-2" = "4",
                        "2-2,5" = "5",
                        "2,5-3" = "6",
                        "3-3,5" = "7",
                        ">3,5"= "8"))
onorm<- datag %>%
  group_by(Inkomen)%>%
  summarise(V1, aantal.inkomen= n())%>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))%>%
  group_by(Inkomen)%>%
  summarise(V1, n= n()/first(aantal.inkomen)*100)%>%
  group_by(Inkomen, n)%>%
  filter(!is.na(Inkomen))%>%
  summarise()
view(onorm)
deel.tabel<-datag%>%
  group_by(Inkomen) %>%
  summarise(aantal.per.ink =n(), V1)%>%
  filter(V1 %in% c( "0-0,5", "2-2,5"))%>%
  group_by(V1, Inkomen)%>%
  summarise(V1, Inkomen, n=100*n()/first(aantal.per.ink))%>%
  group_by(V1, Inkomen, n)%>%
  summarise()
view(deel.tabel)
# tabel maken
niet<- deel.tabel[1:6,2:3]
bijna<- deel.tabel[7:12,2:3]
amper<- niet[[2]]
net.niet<-bijna[[2]]
ondernorm<-onorm[[2]]
data.voor.grafiek<- cbind(amper, net.niet, ondernorm)
naam<- c("0-0,5", "2-2,5", "0-2,5")
palette <- colorRampPalette(c('red', "yellow", 'darkgreen'))
barplot(data.voor.grafiek, beside = T, names.arg = naam, 
        col=palette(6),
        xlab = "Uren beweging* per week",
        ylab = "Percentage",
        legend.text = c("<???26.500,-", "???26.500,- tot ???33.000,-", 
                        "???33.000,- tot ???39.500,-", "???39.500,- tot ???66.000,-", ">???66.000,-", "Geen antwoord"),
        args.legend = c(x=6, y=35),
        main= "Inkomen en beweging")
view(data.voor.grafiek)
#prop.toets uitvoeren
hoogste.onorm<- data%>%
  filter(Inkomen ==5) %>%
  filter(V1 %in% c(1,2,3,4,5))%>%
  select(V1, Inkomen)
een.na.hoogste.onorm<- data%>%
  filter(Inkomen == 4)%>%
  filter(V1 %in% c(1,2,3,4,5))%>%
  select(V1, Inkomen)
mod.onorm <- data %>%
  filter(Inkomen ==3)%>%
  filter(V1 %in% c(1,2,3,4,5))%>%
  select(V1, Inkomen)
ondermod.onorm<-data%>%
  filter(Inkomen == 2)%>%
  filter(V1 %in% c(1,2,3,4,5))%>%
  select(V1, Inkomen)
laagst.onorm<- data%>%
  filter(Inkomen == 1)%>%
  filter(V1 %in% c(1,2,3,4,5))%>%
  select(V1, Inkomen)
# proptest:laagste tegen een na laagste cat:

deel.tabel<-data%>%
  group_by(Inkomen) %>%
  summarise(totaal =n(), V1)%>%
  filter(V1 %in% c(1,2,3,4,5))%>%
  group_by(Inkomen)%>%
  summarise(onorm.ink=n(), Inkomen, totaal)%>%
  group_by(Inkomen, onorm.ink, totaal)%>%
  summarise()
view(deel.tabel)
laagste<- deel.tabel[1,]
een.na.laagste<- deel.tabel[2,]
modaal<-deel.tabel[3,]
hoger<- deel.tabel[4,]
hoogst<- deel.tabel[5,]
prop.test(c(laagste[[2]], een.na.laagste[[2]]), c(laagste[[3]], een.na.laagste[[3]]))
# p waarde te hoog-> geen significant verschil
prop.test(c(laagste[[2]], modaal[[2]]), c(laagste[[3]], modaal[[3]]))
# p waarde te hoog-> geen significant verschil
prop.test(c(modaal[[2]], hoger[[2]]), c(modaal[[3]], hoger[[3]]))
# p waarde te hoog-> geen significant verschil
prop.test(c(hoogst[[2]], modaal[[2]]), c(hoogst[[3]], modaal[[3]]))
# pwaarde is ook hier te hoog om een significant verschil aan te tonen
#verschil aantonen stijging naarmate inkomen stijgt: uiterste nemen, laagste en hoogste inkomenscategorie
minder<- data %>%
  filter(! is.na(V1)| ! is.na(Inkomen))%>%
  filter(Inkomen==1)%>%
  mutate(totaal=n())%>%
  filter(V1 == 1)%>%
  mutate(aantal=n())%>%
  select(aantal, totaal)%>%
  summarise(first(aantal), first(totaal))
view(minder)
hoogsteink<- data %>%
  filter(! is.na(V1)| ! is.na(Inkomen))%>%
  filter(Inkomen==5)%>%
  mutate(totaal=n())%>%
  filter(V1 == 1)%>%
  mutate(aantal=n())%>%
  select(aantal, totaal)%>%
  summarise(first(aantal), first(totaal))
view(hoogst)
prop.test(c(minder[[1]], hoogsteink[[1]]), c(minder[[2]], hoogsteink[[2]]))
