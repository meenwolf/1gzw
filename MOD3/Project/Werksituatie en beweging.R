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
  group_by(Werksituatie)%>%
  summarise(V1, aantal.werk= n())%>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))%>%
  group_by(Werksituatie)%>%
  summarise(V1, n= n()/first(aantal.werk)*100)%>%
  group_by(Werksituatie, n)%>%
  filter(!is.na(Werksituatie))%>%
  summarise()
view(onorm)
#mensen bepalen die niet-amper bewegen en die de norm net niet halen
deel.tabel<-datag%>%
  group_by(Werksituatie) %>%
  summarise(aantal.per.werk =n(), V1)%>%
  filter(V1 %in% c( "0-0,5", "2-2,5"))%>%
  group_by(V1, Werksituatie)%>%
  summarise(V1, Werksituatie, n=100*n()/first(aantal.per.werk))%>%
  group_by(V1, Werksituatie, n)%>%
  summarise()
view(deel.tabel)
# tabel maken
niet<- deel.tabel[1:11,2:3]
bijna<- deel.tabel[12:22,2:3]
amper<- niet[[2]]
net.niet<-bijna[[2]]
ondernorm<-onorm[[2]]
data.voor.grafiek<- cbind(amper, net.niet, ondernorm)
naam<- c("0-0,5", "2-2,5", "0-2,5")
palette <- colorRampPalette(c('red', "yellow", 'darkgreen'))
barplot(data.voor.grafiek, beside = T, names.arg = naam, 
        col=palette(11),
        xlab = "Uren beweging* per week",
        ylab = "Percentage",
        legend.text = c("Ondernemer met personeel"
                        , "ZZP'er/freelancer"
                        , "Werkzaam in loondienst (bedrijfsleven)"
                        , "Werkzaam bij de overheid"
                        , "Werkzaam bij de semi-overheid"
                        , "Arbeidsongeschikt", "Werkloos / werkzoekend / bijstand"
                        , "Gepensioneerd of VUT"
                        , "Studerend / schoolgaand"
                        , "Huisvrouw / huisman"
                        , "Anders"),
        args.legend = c(x=10.6, y=50,cex= 0.7),
        ylim=c(0,50),
        main = "Werksituatie en beweging")
view(data.voor.grafiek)
#prop.toets data prepareren
aantalloon<- data%>%
  filter(Werksituatie ==3)
aantal.loon<-nrow(aantalloon)

loondienst<- data %>%
  filter(Werksituatie == 3)%>%
  filter(! V1 %in% c(6,7,8))
onorm.loon<-nrow(loondienst)

aantalsemi<- data%>%
  filter(Werksituatie ==5)
aantal.semi<-nrow(aantalsemi)

semioverheid<- data %>%
  filter(Werksituatie == 5)%>%
  filter( ! V1 %in% c(6,7,8))
onorm.semi<- nrow(semioverheid)

aantaloverheid<- data%>%
  filter(Werksituatie==4)
aantal.overheid<- nrow(aantaloverheid)
overheid<- data %>%
  filter(Werksituatie == 4)%>%
  filter(! V1 %in% c( 6,7,8))
onorm.overheid<- nrow(overheid)

prop.test(c(onorm.loon, onorm.overheid), c(aantal.loon, aantal.overheid))
prop.test(c(onorm.loon, onorm.semi), c(aantal.loon, aantal.semi))
