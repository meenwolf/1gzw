library(tidyverse)
data<- read.csv(file.choose(), sep=",")
view(data)
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
mannen<- filter(datag, Geslacht == 1)
vrouwen<- filter(datag, Geslacht==2)
m1<- mannen%>%
  group_by(V1) %>%
  summarise(n=n()/nrow(mannen)*100)
v1<- vrouwen %>%
  group_by(V1) %>%
  summarise(n=n()/nrow(vrouwen)*100)
view(m1)
m2<-m1[[2]]
v2<-v1[[2]]
namen<-m1[[1]]
view(namen)
voor.tabel<- rbind(m2, v2)

barplot(voor.tabel, beside = T, names.arg = namen, 
        col=c('lightblue','pink'), 
        xlab = "Uren beweging* per week",
        ylab = "Percentage",
        legend.text = c("Mannen", "Vrouwen"),
        args.legend = c(x=6),
        main= "Uren beweging* van mannen en vrouwen",
        density = c(NA,30),
        angle = c(NA, 54))
view(voor.tabel)
norm.niet<- voor.tabel[,1:5]
view(norm.niet)
norm.n.gehaald<- norm.niet %>%
  rowSums()
view(norm.n.gehaald)
test.mannen<- mannen%>%
  mutate(totaal= n())%>%
  filter(!V1 %in% c("2,5-3", "3-3,5", "<3,5"))%>%
  mutate(aantal=n())%>%
  summarise(first(aantal), first(totaal))
view(test.mannen)  
test.vrouwen<- vrouwen%>%
  mutate(totaal= n())%>%
  filter(!V1 %in% c("2,5-3", "3-3,5", "<3,5"))%>%
  mutate(aantal=n())%>%
  summarise(first(aantal), first(totaal))
view(test.vrouwen)  
prop.test(c(test.mannen[[1]], test.vrouwen[[1]]), c(test.mannen[[2]], test.vrouwen[[2]]))
# dit is geen significant verschil. 