library(tidyverse)
data<- read.csv(file.choose(), sep=",")


met.naam<- data %>%
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


met.naam%>%
  ggplot(aes(x=provincie))+
  geom_bar()

totalen<- c(583467,647470,492134,1153966,414108,2066038, 1301187, 
  2842271,3695103, 382668,2536546,1116668)

goede.data<- met.naam%>%
  select(provincie)%>%
  group_by(provincie)%>%
  mutate(aandeel= n())%>%
  summarise(aantal= first(aandeel))%>%
  mutate(totaal= totalen)%>%
  mutate(percentage= aantal/totaal*100)

tabel<- goede.data%>%
  select(provincie, percentage)
tabel%>%
  ggplot(aes(x=provincie, y= percentage))+
  geom_boxplot()
view(goede.data)
fl<- goede.data[5,2:3]
nh<-goede.data[8,2:3]
prop.test(c(fl[[1]],nh[[1]]), c(fl[[2]], nh[[2]]))
