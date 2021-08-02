library(tidyverse)
data<- read.csv(file.choose())
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

met.naam<- datag %>%
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
onderbeweegnorm.provincie<- filter(datag, V1 %in% c("0-0,5", "0,5-1", "1-1,5", "1,5-2", "2-2,5"))

onorm.provincie<- onderbeweegnorm.provincie%>%
  group_by(provincie)%>%
  summarise("aantal" =n())%>%
  mutate(percentage = aantal /nrow(onderbeweegnorm.provincie)*100)
view(onorm.provincie)

view(met.naam) 
provincies.afk<-c("GR", "FR", "DR", "OV", "FL", "GD", "UT", "NH", "ZH", "ZL", "NB", "LB")
col1<- met.naam[[3]]
view(col1)
barplot(col1, beside = T, names.arg = provincies.afk, 
        col=c("lightblue"),
        main = "Inwoners per provincie die niet aan de beweegnorm voldoen",
        xlab = "Provincies",
        ylab = "Percentage" )
