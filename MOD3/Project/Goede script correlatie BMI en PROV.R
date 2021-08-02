library(tidyverse)
data<- read.csv(file.choose(),sep=",")
bmi<- data %>%
  mutate( BMI = V27/(V28/100)^2)
bmin<- bmi %>%
  mutate(BMIN =cut(BMI,
                   breaks =c(0, 18.5, 24.9,34.9, 50, Inf),
                   labels =c("ondergewicht", "normaal", 
                             "overgewicht","obesitas", 
                             "extreme obesitas"))) %>%
  filter(! is.na(V1) | ! is.na(BMIN))%>%
  select(provincie, BMIN)

met.naam<- bmin %>%
  mutate(provincie= factor(provincie))%>%
  mutate(provincie= fct_recode(provincie,
                               "Groningen" = "20",
                               "Friesland" = "21",
                               "Drente" = "22",
                               "Overijssel" = "23",
                               "Flevoland" = "24",
                               "Gelderland" = "25",
                               "Utrecht" = "26",
                               "Noord-Holland" = "27",
                               "Zuid-Holland" = "28",
                               "Zeeland" = "29",
                               "Noord-Brabant" = "30",
                               "Limburg" = "31"))%>%
  filter(! is.na(provincie))
dataset<- bmin%>%
  group_by(provincie)%>%
  mutate(totaal.prov= n())%>%
  group_by(provincie, BMI)%>%
  mutate(aantal=n())%>%
  group_by(provincie, BMI)%>%
  select(provincie, BMI, totaal.prov, aantal)%>%
  summarise(perc= aantal/totaal.prov*100)%>%
  group_by(provincie, BMI, perc)%>%
  filter(! is.na(BMI))%>%
  filter(! is.na(provincie))%>%
  summarise()
df<-data.frame(GR, FR)

perfect<- bmi%>%
  select(provincie, BMI)

boxplot(BMI~provincie, data=perfect,outline=F)

groningen<- dataset[1:4,]
Groningen<- groningen[[3]]
GR<- append(Groningen, values = 0, after=4)
friesland<- dataset[5:8,]
Friesland<- friesland[[3]]
FR<- append(Friesland, values= 0, after=4)
drenth<- dataset[9:12,]
Drenth<- drenth[[3]]
DR<- append(Drenth, values= 0, after=0)
over<- dataset[13:17,]
OV<- over[[3]]
flevo<- dataset[18:21,]
Flevo<- flevo[[3]]
FL<- append(Flevo, values= 0, after=4)
gel<- dataset[22:26,]
GL<- gel[[3]]
Utrecht<- dataset[27:31,]
UT<- Utrecht[[3]]
"Noord-Holland"<- dataset[32:36,]
NH<- `Noord-Holland`[[3]]
"Zuid-Holland"<- dataset[37:41,]
ZH<- `Zuid-Holland`[[3]]
zeeland<- dataset[42:45,]
Zeeland<-zeeland[[3]]
ZL<- append(Zeeland, values= 0, after=4)
"Noord-Brabant"<- dataset[46:50,]
NB<- `Noord-Brabant`[[3]]
limburg<- dataset[51:55,]
LB<- limburg[[3]]
dataprep<- data.frame(GR, FR, DR,OV, FL, GL, UT, NH,ZH, ZL,NB, LB )
barplot(dataprep, beside=T)
heatmap(dataprep, labCol=c("Extreme obesitas", "ondergewicht",
                           "Obesitas", "Normaal", "Overgewicht"),
        cexCol = 0.7)


df<-data.frame(Provincie=met.naam$provincie, BMI.categorie=met.naam$BMIN, )
p <- ggplot(df, aes(Provincie, BMI.categorie))
p+ stat_bin2d(aes(fill = after_stat(density), bins=(20)) + scale_fill_gradient(name = "Percent", 
                                                                               labels = scales::percent))) 
p + stat_bin2d(bins=(20)) + scale_fill_gradient(name = "Percent", 
                                           labels = scales::percent))

ggplot(df, aes(Provincie, BMI.categorie)) +
  stat_bin2d(aes(fill = after_stat(ndensity))) +
  scale_fill_gradient(name = "Percent", 
                      labels = scales::percent)

ggplot(prep.data, aes(BMI, provincie)) + 
  geom_boxplot()

# voor t test
drente<- bmi%>%
  filter(provincie==22)

ut<- bmi%>%
  filter(provincie==26)
view(gron)
t.test(drente$BMI,ut$BMI)
