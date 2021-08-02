library(tidyverse)
data<-read.csv(file.choose(), sep=",")
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
# BMI berekenen
bmi<- datag %>%
  mutate( BMI = V27/(V28/100)^2)
bmin<- bmi %>%
  mutate(BMIN =cut(BMI,
                   breaks =c(0, 18.5, 24.9,34.9, 50, Inf),
                   labels =c("ondergewicht", "normaal", 
                             "overgewicht","obesitas", 
                             "extreme obesitas"))) %>%
  filter(! is.na(V1) | ! is.na(BMIN))
onorm<- bmin %>%
  group_by(BMIN)%>%
  summarise(V1, aantal.bmi= n())%>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))%>%
  group_by(BMIN)%>%
  summarise(V1, n= n()/first(aantal.bmi)*100)%>%
  group_by(BMIN, n)%>%
  filter(!is.na(BMIN))%>%
  summarise()
view(onorm)
deel.tabel<-bmin%>%
  group_by(BMIN) %>%
  summarise(aantal.per.bmi =n(), V1)%>%
  filter(V1 %in% c( "0-0,5", "2-2,5"))%>%
  group_by(V1, BMIN)%>%
  summarise(V1, BMIN, n=100*n()/first(aantal.per.bmi))%>%
  group_by(V1, BMIN, n)%>%
  summarise()
view(deel.tabel)
# tabel maken
view(deel.tabel)
niet<- deel.tabel[1:5,2:3]
bijna<- deel.tabel[7:11,2:3]
amper<- niet[[2]]
net.niet<-bijna[[2]]
ondernorm<-onorm[[2]]
data.frame(row)
data.voor.grafiek<- cbind(amper, net.niet, ondernorm)
naam<- c("0-0,5", "2-2,5", "0-2,5")
palette <- colorRampPalette(c('red', "yellow", 'darkgreen'))
barplot(data.voor.grafiek, beside = T, names.arg = naam, 
        col=palette(5),
        xlab = "Uren beweging* per week",
        ylab = "Percentage",
        legend.text = c("Ondergewicht", "Normaal gewicht", 
                        "Overgewicht", "Obesitas", "Extreme obesitas"),
        args.legend = c(x=6, y=65),
        main= "BMI en beweging")
view(data.voor.grafiek)
# duidelijk toenemende lijn in beweging.
# kijken vanuit normaal gewicht naar overgewicht
normaal<- bmin%>%
  filter(!is.na(V1)| !is.na(BMIN))%>%
  filter(BMIN == "normaal")%>%
  mutate(totaal=n())%>%
  filter(!V1 %in% c( "2,5-3", "3-3,5", "<3,5"))%>%
  mutate(aantal=n())%>%
  summarise(first(aantal), first(totaal))
view(normaal)
overgewicht<- bmin%>%
  filter(!is.na(V1)| !is.na(BMIN))%>%
  filter(BMIN == "overgewicht")%>%
  mutate(totaal=n())%>%
  filter(!V1 %in% c( "2,5-3", "3-3,5", "<3,5"))%>%
  mutate(aantal=n())%>%
  summarise(first(aantal), first(totaal))
view(normaal)
prop.test(c(normaal[[1]], overgewicht[[1]]), c(normaal[[2]], overgewicht[[2]]))
# dit is geen significant verschil
obesitas<- bmin%>%
  filter(!is.na(V1)| !is.na(BMIN))%>%
  filter(BMIN == "obesitas")%>%
  mutate(totaal=n())%>%
  filter(!V1 %in% c( "2,5-3", "3-3,5", "<3,5"))%>%
  mutate(aantal=n())%>%
  summarise(first(aantal), first(totaal))
prop.test(c(obesitas[[1]], normaal[[1]]), c(obesitas[[2]], normaal[[2]]))
# dit is wel een significant verschil
extreme<-bmin%>%
  filter(!is.na(V1)| !is.na(BMIN))%>%
  filter(BMIN == "extreme obesitas")%>%
  mutate(totaal=n())%>%
  filter(!V1 %in% c( "2,5-3", "3-3,5", "<3,5"))%>%
  mutate(aantal=n())%>%
  summarise(first(aantal), first(totaal))
prop.test(c(obesitas[[1]], extreme[[1]]), c(obesitas[[2]], extreme[[2]]))
# geen significant verschil
onder<-bmin%>%
  filter(!is.na(V1)| !is.na(BMIN))%>%
  filter(BMIN == "ondergewicht")%>%
  mutate(totaal=n())%>%
  filter(!V1 %in% c( "2,5-3", "3-3,5", "<3,5"))%>%
  mutate(aantal=n())%>%
  summarise(first(aantal), first(totaal))
prop.test(c(onder[[1]], normaal[[1]]), c(onder[[2]], normaal[[2]]))
#geen significant verschil
normaaln<- bmin%>%
  filter(!is.na(V1)| !is.na(BMIN))%>%
  filter(BMIN == "normaal")%>%
  mutate(totaal=n())%>%
  filter(V1 %in% c( "0-0,5"))%>%
  mutate(aantal=n())%>%
  summarise(first(aantal), first(totaal))
view(normaaln)
overgewichtn<- bmin%>%
  filter(!is.na(V1)| !is.na(BMIN))%>%
  filter(BMIN == "overgewicht")%>%
  mutate(totaal=n())%>%
  filter(V1 %in% c( "0-0,5"))%>%
  mutate(aantal=n())%>%
  summarise(first(aantal), first(totaal))
prop.test(c(normaaln[[1]], overgewichtn[[1]]), c(normaaln[[2]], overgewichtn[[2]]))
# wel significant verschil
obesitasn<- bmin%>%
  filter(!is.na(V1)| !is.na(BMIN))%>%
  filter(BMIN == "obesitas")%>%
  mutate(totaal=n())%>%
  filter(V1 %in% c( "0-0,5"))%>%
  mutate(aantal=n())%>%
  summarise(first(aantal), first(totaal))
prop.test(c(overgewichtn[[1]], obesitasn[[1]]), c(overgewichtn[[2]], obesitasn[[2]]))
# geen significant verschil
extremen<- bmin%>%
  filter(!is.na(V1)| !is.na(BMIN))%>%
  filter(BMIN == "extreme obesitas")%>%
  mutate(totaal=n())%>%
  filter(V1 %in% c( "0-0,5"))%>%
  mutate(aantal=n())%>%
  summarise(first(aantal), first(totaal))
prop.test(c(obesitasn[[1]], extremen[[1]]), c(obesitasn[[2]], extremen[[2]]))
view(extremen)
df<-data.frame(prov=met.naam$provincie, g.bmi=met.naam$BMIN)
p <- ggplot(df, aes(prov,g.bmi))
p + stat_bin2d(bins=20) + scale_fill_gradientn(colours=palette(20), trans="log")

