library(tidyverse)
data<- read.csv( file.choose(), sep=",")
view(data)
datagoed<- data %>%
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
view(datagoed)
alleen<- filter(datagoed, V8_1 == 1)
nrow(alleen)
onderbeweegnorm<- filter(alleen, V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))
nrow(datagoed)
eindalleen <- alleen%>%
  group_by(V1) %>%
  summarise(n=n()) %>%
  mutate(percentage= n/sum(n)*100)
#nu met een partner of vriend
tweetal<- filter(datagoed, V8_2 == 1)
nrow(tweetal)
onderbeweegnorm.tweetal<- filter(tweetal, V1 %in% c("0-0,5",
                                                    "0,5-1",
                                                    "1-1,5",
                                                    "1,5-2",
                                                    "2-2,5"))
eindtweetal <- tweetal%>%
  group_by(V1) %>%
  summarise(n=n()) %>%
  mutate(percentage= n/sum(n)*100)
view(eindtweetal)
# nu met 3 of meer, dus een groep
groep<- filter(datagoed, V8_3 == 1)
nrow(groep)
onderbeweegnorm.groep<- filter(groep, V1 %in% c("0-0,5",
                                                    "0,5-1",
                                                    "1-1,5",
                                                    "1,5-2",
                                                    "2-2,5"))
eindgroep <- groep%>%
  group_by(V1) %>%
  summarise(n=n()) %>%
  mutate(percentage= n/sum(n)*100)
view(eindgroep)
# preparatie voor tabel alleen
gefilterd.alleen<- filter(eindalleen, V1 == "0-0,5" | V1=="2-2,5")
view(gefilterd.alleen)
deel.alleen<-gefilterd.alleen[[3]]
onorm<- nrow(onderbeweegnorm)/nrow(alleen)*100
view(onorm)
view(deel.alleen)
# preparatie voor tabel tweetal
gefilterd.tweetal<- filter(eindtweetal, V1 == "0-0,5" | V1=="2-2,5")
view(gefilterd.tweetal)
deel.tweetal<-gefilterd.tweetal[[3]]
view(deel.tweetal)
onorm.tweetal<- nrow(onderbeweegnorm.tweetal)/nrow(tweetal)*100
view(onorm.tweetal)
 # preparatie voor tabel groep
gefilterd.groep<- filter(eindgroep, V1 == "0-0,5" | V1=="2-2,5")
view(gefilterd.groep)
deel.groep<-gefilterd.groep[[3]]
view(deel.groep)
onorm.groep<- nrow(onderbeweegnorm.groep)/nrow(groep)*100
view(onorm.groep)       
#hieronder tabel maken

col1<- c(deel.alleen, onorm)
view(col1)

col2<- c(deel.tweetal, onorm.tweetal)
view(col2)

col3<- c(deel.groep, onorm.groep)
view(col3)
# daadwerkelijke tabel maken
tabel<- data.frame(col1, col2, col3, row.names = c("0-0,5 uur bewegen*(%)", "2-2,5 uur bewegen* (%)", "0-2,5 uur bewegen* (%)"))
colnames(tabel)<-c("alleen sporten", " met <3 personen sporten", "met >3 personen sporten")
# je kan zelf hierboven de kolommen een andere naam geven, net als de rijen
view(tabel)
namen<- c("0-0,5 uur", "2-2,5 uur", "0-2,5 uur")
tabel.samenvoeging<- rbind(col1, col2, col3)
barplot(tabel.samenvoeging, beside = T, names.arg = namen, 
        col=c('lightblue','blue', "darkblue"), 
        xlab = "aantal uren beweging* per week",
        ylab = "Percentage",
        main = "sportgezelschap en aantal uren beweging* per week",
        legend.text = c("alleen sporten", "met <3 personen sporten", "met >3 personen sporten"),
        args.legend = c(x=5),
        density = c(NA,30, 20),
        angle = c(54))
# alles tussen aanhalingstekens kan je uiteraard zelf vervangen door andere
# tekst