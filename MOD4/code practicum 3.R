library(tidyverse)
geenoutpatient<-read.csv(choose.files(),sep=",")
welmetoutpatient<-read.csv(choose.files(),sep=",")

view(geenoutpatient)
view(welmetoutpatient)
otijd<-450
okpersoort<- function(slack){
  welmetoutpatient%>%
    group_by(Specialty.nr.)%>%
    summarise(Duur=sqrt(sum(variability.per.dag))*slack+sum(verwachtte.duur.met.no.shows))%>%
    mutate(oks_nodig= Duur/otijd,Dedicated= trunc(oks_nodig),Rest= oks_nodig-Dedicated)

}
slack_seq=seq(0,3, by=0.2)
Aantal_OKs<-c()
for(slack in slack_seq) {
  tussen<- okpersoort(slack)
  Aantal_OKs<-append(Aantal_OKs, c(sum(tussen$oks_nodig)))
}
 kans_op_uitloop<-1-pnorm(slack_seq)
 #Dit grafiekje hebben we gebruikt om de bijbehoorende slackfactor 
 # en kans op uitloop bij een x aantal ok's te zien.
 # Het is een erg mooi lijntje, een parametrische functie tov slack.
plot(Aantal_OKs, kans_op_uitloop, type="o",xlab="Aantal Operatiekamers zonder emergency OR's",
     ylab= "Kans op uitloop", main= "Aantal elective OK's en de kans op uitloop",
     col="red")
grid()
text(Aantal_OKs, kans_op_uitloop, labels=slack_seq,pos=3)
#de getallen die je ziet zijn de bijbehorende slack factoren. Wij hebben aan de hand van 
#dit grafiekje bepaald hoeveel elective OK's onze opties kunnen bevatten. Wij willen
#uiteraard uitgaan van een slackfactor die voor een aantal ok's zorgt dat bijna een rond getal is
#,9876 bijvoorbeeld. Wij hebben daarom gekozen voor de volgende opties:

minste_wachttijd<- okpersoort(1.69)

sum(minste_wachttijd$oks_nodig)
#slack factor van 1.74 geeft een benodigd aantal OKs(minus emergency) van 21.9874(22 dus)
#dit betekend dat er 1-0.962=0.038 dus minder dan 4% kans op uitloop is
# Met 1 outpatient OK wordt bij hetzelfde totaal aantal oks van 22 de slack 1.69, 
#dus 1-0,9545=0,0455 is iets minder dan 5% kans op uitloop

acceptabele_middenweg<- okpersoort(0.39)
sum(acceptabele_middenweg$oks_nodig)
#slack factor van 0.4 geeft een benodigd aantal OKs(minus emergency) van 17.9891 (dus18)
#dit betekend dat er 1-0.6554=0.3446 dus minder dan 35% kans op uitloop is
#Met 1 outpatient OK wordt er bij hetzelfde aantal OK's een uitloop van 1-0.6517=0,3483 
#dus iets minder dan dan 35% kans op uitloop gehanteerd. Hier maakt het geen verschil in wachttijd.

hoogste_bezetting<- okpersoort(0.06)
sum(hoogste_bezetting$oks_nodig)
# wanneer er 17(16.9448)OK's komen, is de kans op uitloop 1-0.5279=0.4721. dus iets minder dan 48% kans 
# op uitloop. Dit is net iets patientvriendelijker dan het
#percentage wat gehanteerd wordt bij sommige ziekenhuizen in nederland tijdens de corona crisis
# met 1 outpatient OK wordt de kans op uitloop 1-0,5239= 0,4761 
#dus iets minder dan 48% kans op uitloop. Dit maakt wederom geen verschil in uitlooptijd.
view(minste_wachttijd)
view(acceptabele_middenweg)
view(hoogste_bezetting)
#al met al kunnen we dus concluderen dat er altijd 1 outpatient OK aanwezig is samen met 3 emergency OK's.
#De overige OK's worden per preformance indicator voor onze ideeen ingedeeld.