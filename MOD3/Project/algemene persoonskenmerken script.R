library(tidyverse)
data<- read.csv(file.choose(), sep= ",")
totaal<- nrow(data)
# informatie over het geslacht
geslacht<- data%>%
  select(Geslacht)%>%
  mutate(totaal=n())%>%
  group_by(Geslacht)%>%
  mutate(aantal= n())%>%
  mutate(Percentage= aantal/ totaal*100)%>%
  mutate(Geslacht= factor(Geslacht))%>%
  mutate(Geslacht=fct_recode(Geslacht,
                             "Mannen"= "1",
                             "Vrouwen"= "2"))%>%
  group_by(Geslacht)%>%
  summarise(first(Geslacht), first(aantal), first(Percentage))
# informatie over opleidingsniveau
opleiding<- data%>%
  mutate(totaal=n())%>%
  group_by(Opleiding_her)%>%
  summarise(aantal= n(), percentage= aantal/first(totaal)*100)%>%
  mutate(Opleiding_her= factor(Opleiding_her))%>%
  mutate(Opleiding_her= fct_recode(Opleiding_her,
                                   "Laagopgeleid" = "1",
                                   "Middelbaar opgeleid"= "2",
                                   "Hoogopgeleid"= "3"))
#informatie over de leeftijd van de respondenten  
leeftijd<- data%>%
  filter(! V26>= 104)%>%
  summarise(gem.leef= mean(V26, na.rm=T), sd.leef= sd(V26, na.rm=T))
# BMI van de respondenten, gezien het over beweging en sport gaat
bmi<- data %>%
  mutate( BMI = V27/(V28/100)^2)
bmin<- bmi %>%
  mutate(BMIN =cut(BMI,
                   breaks =c(0, 18.5, 24.9,34.9, 50, Inf),
                   labels =c("ondergewicht", "normaal", 
                             "overgewicht","obesitas", 
                             "extreme obesitas"))) %>%
  filter(! is.na(V1) | ! is.na(BMIN))
gewicht<- bmin%>%
  select(BMIN)%>%
  mutate(totaal=n())%>%
  group_by(BMIN)%>%
  mutate(aantal= n())%>%
  mutate(percentage= aantal/totaal*100)%>%
  group_by(BMIN)%>%
  summarise(first(aantal), first(percentage))
#totaal aantal mensen dat niet aan de beweegnorm voldoet
niet<- data%>%
  filter(! is.na(V1))%>%
  mutate(totaal=n())%>%
  filter(V1 %in% c(1,2,3,4,5))%>%
  mutate(aantal= n())%>%
  summarise(first(totaal), first(aantal), perc= first(aantal)/first(totaal)*100)

4375*0.107
317+376+468+1062+799+625+726  
470/4375
