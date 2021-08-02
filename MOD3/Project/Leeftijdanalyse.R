library(tidyverse)
data<- read.csv(file.choose(), sep= ",")
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
datag%>%
  ggplot(aes(x=V26))+
  geom_histogram(col="darkblue")+
  labs(x= "leeftijd",
       y= "frequentie")
# we zien geen rare waarden, dus we kunnen buckets maken
# van logische leeftijdscategorie verdelingen, rekening
# houdend met de grootte van de buckets. zodat we geen 300
# tegenover 2000 gaan vergelijken.

clean<- datag %>%
  mutate(Leeftijd = cut(V26,
                       breaks= c(17, 30, 40, 50,
                                 60, 65, 70,
                                 103)))%>%
  group_by(Leeftijd)%>%
  summarise(nleef=n(), V1)%>%
  group_by(Leeftijd, V1)%>%
  summarise(n=n()/first(nleef)*100, nleef=first(nleef))
# kolom 1 en 2 generen
clean1<- clean %>%
  filter( V1 == "0-0,5")
col1<-clean1[[3]]
clean2<- clean %>%
  filter( V1== "2-2,5")
col2<-clean2[[3]]
view(col1)
# datafiltering om percentage te berekenen dat niet aan de norm voldoet
jong<- filter(datag, V26 %in% c(17:30))
jongste<- jong%>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))%>%
  nrow()/nrow(jong)*100
midden<- filter(datag, V26 %in% c(31:40))
middelste<- midden%>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))%>%
  nrow()/nrow(midden)*100
drie<- filter(datag, V26 %in% c(41:50))
derde<- drie%>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))%>%
  nrow()/nrow(drie)*100
vier<- filter(datag, V26 %in% c(51:60))
vierde<- vier%>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))%>%
  nrow()/nrow(vier)*100
voorpen<- filter(datag, V26 %in% c(61:65))
voorp<- voorpen%>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))%>%
  nrow()/nrow(voorpen)*100
napen<- filter(datag, V26 %in% c(66:70))
nap<- napen%>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))%>%
  nrow()/nrow(napen)*100
oud<- filter(datag, V26 %in% c(71:103))
oudste<- oud%>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))%>%
  nrow()/nrow(oud)*100
# tabel maken en transposen
col3<- c(jongste, middelste, derde, vierde, voorp, nap, oudste)
view(col3)
tabel<- data.frame(col1, col2, col3, row.names = c("18-30 jaar",
           "31-40 jaar", "41-50 jaar", "51-60 jaar",
           "61-65 jaar", "66-70 jaar", "71-103 jaar"))
colnames(tabel)<- c("0-0,5 uur", "2-2,5 uur", "0-2,5 uur")
transtabel<- as.data.frame(t(tabel))
view(transtabel)
cbind(col1, col2, col3)
ncol1<- transtabel[[1]]
ncol2<- transtabel[[2]]
ncol3<- transtabel[[3]]
ncol4<- transtabel[[4]]
ncol5<- transtabel[[5]]
ncol6<- transtabel[[6]]
ncol7<- transtabel[[7]]

binding<- rbind(ncol1, ncol2, ncol3, ncol4, ncol5, ncol6, ncol7)
# staafdiagram maken
naam<- c("0-0,5", "2-2,5", "0-2,5")
palette <- colorRampPalette(c('red', "yellow", 'darkgreen'))
barplot(binding, beside = T, names.arg = naam, 
        col=palette(7),
        xlab = "Uren beweging* per week",
        ylab = "Percentage",
        legend.text = c("18-30 jaar", "31-40 jaar", 
        "41-50 jaar", "51-60 jaar", "61-65 jaar",
        "66-70 jaar", "71-103 jaar"),
        main= "Beweging per leeftijdscategorie",
        args.legend = c(x=6, y=50))
view(binding)
#significantie hoogste en laagste percentage 0-0,5
view(col1)
l18.30<- jong%>%
  mutate(totaal=n())%>%
  select(V1, totaal)%>%
  filter(! V1 %in% c("2,5-3", "3-3,5", ">3,5"))%>%
  summarise(aantal=n(), first(totaal))
view(l18.30)  
l41.50<- drie%>%
  mutate(totaal=n())%>%
  select(V1, totaal)%>%
  filter(! V1 %in% c("2,5-3", "3-3,5", ">3,5"))%>%
  summarise(aantal=n(), first(totaal))
view(l41.50)
prop.test(c(l18.30[[1]], l41.50[[1]]),c(l18.30[[2]], l41.50[[2]]))
# geen significant verschil tussen de 18-30 jarigen en de 41-50 jarigen
# kijken of dit wel is met 18-30 en 31-40 jarigen
l31.40<- midden%>%
  mutate(totaal=n())%>%
  select(V1, totaal)%>%
  filter(! V1 %in% c("2,5-3", "3-3,5", ">3,5"))%>%
  summarise(aantal=n(), first(totaal))
view(l31.40)
prop.test(c(l18.30[[1]], l31.40[[1]]),c(l18.30[[2]], l31.40[[2]]))
# geen significant verschil tussen de 18-30 jarigen en de 31-40 jarigen
# dan zijn we beniewd naar de groep die het meeste beweegt, de 51-60 jarigen en de groep
# die het minste beweegt, de 31-40 jarigen
l31.40<- midden%>%
  mutate(totaal=n())%>%
  select(V1, totaal)%>%
  filter(! V1 %in% c("2,5-3", "3-3,5", ">3,5"))%>%
  summarise(aantal=n(), first(totaal))
view(l31.40)
l51.60<- vier%>%
  mutate(totaal=n())%>%
  select(V1, totaal)%>%
  filter(! V1 %in% c("2,5-3", "3-3,5", ">3,5"))%>%
  summarise(aantal=n(), first(totaal))
view(l51.60)
prop.test(c(l31.40[[1]], l51.60[[1]]),c(l31.40[[2]], l51.60[[2]]))
# dit is zeker significant
# percentage lang niet aan de norm voldoen 0-0,5 uur beweging
n18.30<- jong%>%
  filter(!is.na(V1))%>%
  mutate(totaal=n())%>%
  select(V1, totaal)%>%
  filter( V1 == "0-0,5")%>%
  summarise(aantal=n(), first(totaal))
view(n18.30)  
n41.50<- drie%>%
  filter(! is.na(V1))%>%
  mutate(totaal= n())%>%
  select(V1, totaal)%>%
  filter( V1 == "0-0,5")%>%
  summarise(aantal=n(), totaal)%>%
  group_by(aantal, totaal)%>%
  summarise()
prop.test(c(n18.30[[1]], n41.50[[1]]),c(n18.30[[2]], n41.50[[2]]))
# dit is een significant verschil
b51.60<- vier%>%
  filter(!is.na(V1))%>%
  mutate(totaal=n())%>%
  select(V1, totaal)%>%
  filter( V1 == "2-2,5")%>%
  summarise(aantal=n(), first(totaal))
view(n51.60)  
b66.70<- napen%>%
  filter(! is.na(V1))%>%
  mutate(totaal= n())%>%
  select(V1, totaal)%>%
  filter( V1 == "2-2,5")%>%
  summarise(aantal=n(), totaal)%>%
  group_by(aantal, totaal)%>%
  summarise()
prop.test(c(b51.60[[1]], b66.70[[1]]), c(b51.60[[2]], b66.70[[2]]))
# dit is geen significant verschil
prop.test(c(l18.30[[1]], l51.60[[1]]), c(l18.30[[2]], l51.60[[2]]))
# ook geen significant verschil