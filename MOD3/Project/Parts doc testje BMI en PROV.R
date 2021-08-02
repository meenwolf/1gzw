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
  filter(! is.na(V1) | ! is.na(BMIN))
prep.data<- bmin%>%
  group_by(provincie)%>%
  mutate(totaal.prov= n())%>%
  group_by(provincie, BMIN)%>%
  mutate(aantal=n())%>%
  group_by(provincie, BMIN)%>%
  select(provincie, BMIN, totaal.prov, aantal)%>%
  summarise(perc= aantal/totaal.prov*100)%>%
  group_by(provincie, BMIN, perc)%>%
  filter(! is.na(BMIN))%>%
  filter(! is.na(provincie))%>%
  summarise()

df<-data.frame(Groningen=prep.data[1:4,3], Friesland= prep.data[5:8,3], row.names = c("ondergewicht") )
p <- ggplot(df, aes(value))
p + stat_bin2d(bins=2) + scale_fill_gradientn(colours=palette(5), trans="log")
Drenthe= prep.data[11:15,3],
Overijssel= prep.data[16:20,3], Flevoland= prep.data[21:25,3], Gelderland= prep.data[26:30,3], 
Utrecht=prep.data[31:35,3], "Noord-Holland"= prep.data[36:40,3], 
"Zuid-Holland"=prep.data[41:45,3], Zeeland= prep.data[46:50,3], "Noord-Brabant"= prep.data[51:55,3],
Limburg=prep.data[:15,3])
