library(tidyverse)
data<- read.csv( "C:\\Users\\User\\Downloads\\BWEBPR18LEEFSTIJL_eindbestand.csv", sep=",")
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
jong<- filter(datag, V26 %in% c(18:30))
midden<- filter(datag, V26 %in% c(31:40))
derde<- filter(datag, V26 %in% c(41:50))
vierde<- filter(datag, V26 %in% c(51:60))
voorpen<-filter(datag, V26 %in% c(61:65))
napen<- filter(datag, V26 %in% c(66:70))
oud<- filter(datag, V26 %in% c(71:103))
view(jong)
jongste<- jong%>%
  group_by(V26) %>%
  filter(V1 == "0-0,5")%>%
  summarise(n= n()/nrow(jong) *100)%>%
  summarise("onder" = sum(n))
middelste<- midden%>%
  group_by(V26) %>%
  filter(V1 == "0-0,5")%>%
  summarise(n= n()/nrow(midden) *100)%>%
  summarise("onder" = sum(n))
drie<- derde%>%
  group_by(V26) %>%
  filter(V1 == "0-0,5")%>%
  summarise(n= n()/nrow(derde) *100)%>%
  summarise("onder" = sum(n))
vier<- vierde%>%
  group_by(V1) %>%
  filter(V1 == "0-0,5")%>%
  summarise(n= n()/nrow(vierde) *100)%>%
  summarise("onder" = sum(n))
voorpenste<- voorpen%>%
  group_by(V1) %>%
  filter(V1 == "0-0,5")%>%
  summarise(n= n()/nrow(voorpen) *100)%>%
  summarise("onder" = sum(n))
napenste<- napen%>%
  group_by(V1) %>%
  filter(V1 == "0-0,5")%>%
  summarise(n= n()/nrow(napen) *100)%>%
  summarise("onder" = sum(n))
view(napenste)
oudste<- oud%>%
  group_by(V1) %>%
  filter(V1 == "0-0,5")%>%
  summarise(n= n()/nrow(oud) *100)%>%
  summarise("onder" = sum(n))
nauwelijks<- c(jongste, middelste, drie, vier, 
               voorpenste, napenste, oudste)
nauwelijks <- rbind(jongste, middelste, drie, 
                    vier, voorpenste, 
                    napenste, oudste)
jongsteb<- jong%>%
  group_by(V26) %>%
  filter(V1 == "2-2,5")%>%
  summarise(n= n()/nrow(jong) *100)%>%
  summarise("bijna" = sum(n))
middelsteb<- midden%>%
  group_by(V26) %>%
  filter(V1 == "2-2,5")%>%
  summarise(n= n()/nrow(midden) *100)%>%
  summarise("bijna" = sum(n))
drieb<- derde%>%
  group_by(V26) %>%
  filter(V1 == "2-2,5")%>%
  summarise(n= n()/nrow(derde) *100)%>%
  summarise("bijna" = sum(n))
vierb<- vierde%>%
  group_by(V1) %>%
  filter(V1 == "2-2,5")%>%
  summarise(n= n()/nrow(vierde) *100)%>%
  summarise("bijna" = sum(n))
voorpensteb<- voorpen%>%
  group_by(V1) %>%
  filter(V1 == "2-2,5")%>%
  summarise(n= n()/nrow(voorpen) *100)%>%
  summarise("bijna" = sum(n))
napensteb<- napen%>%
  group_by(V1) %>%
  filter(V1 == "2-2,5")%>%
  summarise(n= n()/nrow(napen) *100)%>%
  summarise("bijna" = sum(n))
view(napenste)
oudsteb<- oud%>%
  group_by(V1) %>%
  filter(V1 == "2-2,5")%>%
  summarise(n= n()/nrow(oud) *100)%>%
  summarise("bijna" = sum(n))

bijna<- c(jongsteb, middelsteb, drieb, vierb, voorpensteb, napensteb, oudsteb)
view(bijna)
filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5")) %>%
  summarise(n= n()/nrow(napen)*100) %>%
  summarise("onorm" = sum(n))
jongsten<- jong%>%
  group_by(V26) %>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5",
                    "1,5-2", "2-2,5")) %>%  
  summarise(n= n()/nrow(jong) *100)%>%
  summarise("onorm" = sum(n))
middelsten<- midden%>%
  group_by(V26) %>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5",
                    "1,5-2", "2-2,5")) %>%
  summarise(n= n()/nrow(midden) *100)%>%
  summarise("onorm" = sum(n))
drien<- derde%>%
  group_by(V26) %>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5",
                    "1,5-2", "2-2,5")) %>%
  summarise(n= n()/nrow(derde) *100)%>%
  summarise("onorm" = sum(n))
viern<- vierde%>%
  group_by(V1) %>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5",
                    "1,5-2", "2-2,5")) %>%
  summarise(n= n()/nrow(vierde) *100)%>%
  summarise("onorm" = sum(n))
voorpensten<- voorpen%>%
  group_by(V1) %>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5",
                    "1,5-2", "2-2,5")) %>%  
  summarise(n= n()/nrow(voorpen) *100)%>%
  summarise("onorm" = sum(n))
napensten<- napen%>%
  group_by(V1) %>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5",
                    "1,5-2", "2-2,5")) %>%
  summarise(n= n()/nrow(napen) *100)%>%
  summarise("onorm" = sum(n))
oudsten<- oud%>%
  group_by(V1) %>%
  filter( V1 %in% c("0-0,5", "0,5-1", "1-1,5",
                    "1,5-2", "2-2,5")) %>%
  summarise(n= n()/nrow(oud) *100)%>%
  summarise("onorm" = sum(n))
onderdenorm<- c(jongsten, middelsten, drien, viern, voorpensten, napensten, oudsten)
data<- rbind(nauwelijks, bijna, onderdenorm)
view(nauwelijks)
tabel<- matrix(nauwelijks, bijna, onderdenorm)
view(leeftijdt)
leeftijdt <- matrix(tabel, ncol=7,byrow=TRUE)
colnames(leeftijdt) <- c("18-30","31-40","41-50",
                       "51-60", "61-64", "65-70",
                       "71-103")
rownames(leeftijdt) <- c("0-0,5 uur",
                           "2-2,5 uur",
                           "0-2,5 uur")
leeftijd <- as.table(leeftijdt)
view(leeftijdt)
plot(leeftijdt, x == ("18-30", "30-40"))
