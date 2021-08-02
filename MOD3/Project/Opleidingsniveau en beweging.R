library(tidyverse)
data<- read.csv(file.choose(), sep=",")
view(data)
max(data$Opleiding)
max(data$Opleiding_her)
her1<- data %>%
  filter( Opleiding_her == 1)%>%
  select(Opleiding, Opleiding_her, V1)%>%
  group_by(Opleiding, V1)%>%
  summarise(n=n())
view(her1)
# hercodering: 1= Geen onderwijs / basisonderwijs / cursus inburgering / cursus Nederlandse taal
#LBO / VBO / VMBO (kader- of beroepsgerichte leerweg) / MBO 1 (assistentenopleiding)
# MAVO / HAVO of VWO (eerste drie jaar) / ULO / MULO / VMBO (theoretische of gemengde leerweg) /
#  voortgezet speciaal onderwijs
#eens met deze codering-> je kan nog geen ander werk doen met optie 1,2 of 3

her2 <- data %>%
  filter( Opleiding_her == 2)%>%
  select(Opleiding, Opleiding_her, V1)%>%
  group_by(Opleiding, V1)%>%
  summarise(n=n())
view(her2)
# hercodering is 4&5 MBO 2, 3, 4 (basisberoeps-, vak-, middenkader- of specialistenopleiding) of MBO oude structuur (vóór 1998)
# HAVO of VWO (overgegaan naar de 4e klas) / HBS / MMS / HBO propedeuse of WO Propedeuse
# enigsinds mee eens, jammer dat ze een middelbaarschool diploma samen rekenen met MBO 2-3 of 4 of HBS.
# de hercodering veranderdert hier niks aan, gezien het geen losse antwoordoptie was
# wel eerlijk dat ze HBS of oude MBO structuur samen met de nieuwe niveau 2,3 en 4 nemen
# aanhouden hercodering hier

her3 <- data %>%
  filter( Opleiding_her == 3)%>%
  select(Opleiding, Opleiding_her, V1)%>%
  group_by(Opleiding, V1)%>%
  summarise(n=n())
view(her3)
# hercodering drie neemt antwoordopties 6 en 7 samen. dit zijn  HBO (behalve HBO-master) / WO-kandidaats- of WO-bachelor
# WO-doctoraal of WO-master of HBO-master / postdoctoraal onderwijs
# hier ben ik het wel mee eens, gezien de antwoordopties HBO en WO sowieso al combineren
# een verschil met bachelor of master op HBO of WO niveau. Alleen in het begin zal het inkomen
# van een WO student gemiddeld hoger liggen. Na een aantal jaar zal dit naar elkaar toe 
# convergeren, gezien werkervaring ook geld waard is, en er een limiet is. 
# het overgrote deel van de mensen in onze dataset is 50+. Wij achten het verschil klein
# en gezien de hercodering er niet voor niets is, zullen wij deze ook voor deze groep behouden.

# opleiding niet meer van belang, alleen opleiding_her
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
  group_by(Opleiding_her)%>%
  summarise(V1, aantal.opl= n())%>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))%>%
  group_by(Opleiding_her)%>%
  summarise(V1, n= n()/first(aantal.opl)*100)%>%
  group_by(Opleiding_her, n)%>%
  filter(!is.na(Opleiding_her))%>%
  summarise()
view(onorm)
#mensen bepalen die niet-amper bewegen en die de norm net niet halen
deel.tabel<-datag%>%
  group_by(Opleiding_her) %>%
  summarise(aantal.per.opl =n(), V1)%>%
  filter(V1 %in% c( "0-0,5", "2-2,5"))%>%
  group_by(V1, Opleiding_her)%>%
  summarise(V1, Opleiding_her, n=100*n()/first(aantal.per.opl))%>%
  group_by(V1, Opleiding_her, n)%>%
  summarise()
view(deel.tabel)
# tabel maken
niet<- deel.tabel[1:3,1:3]
bijna<- deel.tabel[4:6,1:3]
amper<- niet[[3]]
net.niet<-bijna[[3]]
ondernorm<-onorm[[2]]
data.voor.grafiek<- cbind(amper, net.niet, ondernorm)
naam<- c("0-0,5", "2-2,5", "0-2,5")
palette <- colorRampPalette(c('red', "yellow", 'darkgreen'))
barplot(data.voor.grafiek, beside = T, names.arg = naam, 
        col=palette(3),
        xlab = "Uren beweging* per week",
        ylab = "Percentage",
        legend.text = c("Laagopgeleiden", "Middelbaaropgeleiden", 
                        "Hoogopgeleiden"),
        args.legend = c(x=4.5, y=35),
        main= "Opleidingsniveau en beweging")
view(data.voor.grafiek)
# z-toets uitvoeren voor de laag- en hoog opgeleiden
laagopl<- data %>%
  filter(! is.na(V1)| ! is.na(Opleiding_her))%>%
  filter(Opleiding_her==1)%>%
  mutate(totaal=n())%>%
  filter(V1 == 1)%>%
  mutate(aantal=n())%>%
  select(aantal, totaal)%>%
  summarise(first(aantal), first(totaal))
view(laagopl)
hoogopl<- data %>%
  filter(! is.na(V1)| ! is.na(Opleiding_her))%>%
  filter(Opleiding_her==3)%>%
  mutate(totaal=n())%>%
  filter(V1 ==1)%>%
  mutate(aantal=n())%>%
  select(aantal, totaal)%>%
  summarise(first(aantal), first(totaal))
view(hoogopl)
prop.test(c(laagopl[[1]], hoogopl[[1]]), c(laagopl[[2]], hoogopl[[2]]))
# de p- waarde tussen de laag en hoogopgeleiden is zeker significant.
middenopl<- data %>%
  filter(! is.na(V1)| ! is.na(Opleiding_her))%>%
  filter(Opleiding_her==2)%>%
  mutate(totaal=n())%>%
  filter(V1 ==1)%>%
  mutate(aantal=n())%>%
  select(aantal, totaal)%>%
  summarise(first(aantal), first(totaal))
view(middenopl)
prop.test(c(middenopl[[1]], hoogopl[[1]]), c(middenopl[[2]], hoogopl[[2]]))
hoogst.niet<- data %>%
  filter(! is.na(V1)| ! is.na(Opleiding_her))%>%
  filter(Opleiding_her==3)%>%
  mutate(totaal=n())%>%
  filter(V1 == 4)%>%
  mutate(aantal=n())%>%
  select(aantal, totaal)%>%
  summarise(first(aantal), first(totaal))
view(minder)
prop.test(c(hoogst.niet[[1]], hoogopl[[1]]), c(hoogst.niet[[2]], hoogopl[[2]]))
# duidelijk significant
