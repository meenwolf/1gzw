aantal<-20
1:20
x<-seq(from=0, to=10, length.out=aantal+1)
x
y<-c(aantal,5,3,17)
y
y+3
sqrt(x)
mean(y)
sd(y)
z<-y-mean(y)
z^2
v<-sum(z^2)
v
sqrt(v/length(y))
length(y)
SD<-sqrt(sum((y-mean(y))^2)/(length(y)-1))
SD
sd(y)
View(women)
temp<- women
temp
view(men)
temp$gewicht <- temp$weight / 2.2046
temp
temp$lengte <- temp$height*2.45/100
temp
temp$BMI <- temp$gewicht/temp$lengte^2
temp
view(temp)
mean(temp$BMI)
sd(temp$BMI)
median(temp$BMI)
view(titanic.cvs)
view(Titanic)
titanic<- titanic.cvs
4 %>% sin()
temp %>% 
  mutate(bla2=lengte) %>%
  mutate(foo=bla2+lengte)
titanic<- read.csv(file="titaaanic.csv")
view(Titanic)
view(titanic)
head(titanic)
view(titanic)
filter(titanic$Survived==1)
blabla<- titanic %>%
  filter(Survived ==1)%>%
  nrow() %>%

filter(titanic, Survived==1)
blablabla<- nrow(filter(titanic, Survived==1))
blablabla
nrow(filter(titanic, Sex== "female" & Pclass==1))



titanic %>%
  filter(Sex == "female" & Pclass==1) -> selectie

sum(selectie$Age, na.rm=T)
metleef<- filter(selectie, is.na(Age)==F)
nrow(metleef)
sum(metleef$Age)
sum(metleef$Age)/nrow(metleef)
dit<- summarise(metleef$Age)
dit
  
  
summarise(metleef, totleef = sum(Age), aantal= n(),gemleef = sum(Age)/n())


titanic %>%
  filter(is.na(Age)==F) %>%
  group_by(Sex, Pclass) -> bla

view(bla)
temp<- select(titanic, Survived: Sex)
temp
view(temp)

temp<- women

new<-temp%>%
  mutate(gewicht= weight/2.2046, lengte = height*2.45)%>%
  mutate(BMI= gewicht/lengte^2*10000)

view(new)

women2<- select(new, gewicht, lengte, BMI)

view(women2)

chol1 <- read.csv("voorbeeld7_1.csv", sep =";")
view(chol1)



boxplot(chol1$leeftijd, chol1$chol, xlab = "leeftijd", ylab = "cholesterol", col= "blue", pch=3 )

chol1%>%
  mutate(alcohol= cut(alcohol, 
                      breaks = c(0,1,2),
                      labels = c("geen", "matig","veel")))

rlang::last_error()




         
library(forcats)
                          
chol1 %>%
  mutate(alcohol = factor(alcohol))%>%
  mutate(alcohol = fct_recode(alcohol, geen="0", matig="1", veel="2"))
               


boxplot(chol1$alcohol, chol1$bmi)


view(chol1)


new<- chol1 %>%
  mutate(sekse = factor(sekse)) %>%
  mutate(sekse  = fct_recode(sekse, vrouw="0", man="1"))


view(new)

library(tidyverse)
        
twee<- new%>%
  rename(geslacht = sekse)
view(twee)

twee%>%
  filter(geslacht=="vrouw")%>%
  ggplot(aes(x= chol))+
  geom_histogram()+
  labs(x="cholesterol",
       y= "aantal")

  labs(x)
  
  filter(twee, sekse)
  
 
  
  
vrouwen<- filter(twee, geslacht=="vrouw")
mannen<- filter(twee, geslacht=="man")
vrouwen$geslacht<- "vrouw"
mannen$geslacht <- "man"
choles<- rbind(vrouwen, mannen)


  ggplot(choles, aes(chol, fill = geslacht)) + 
  geom_histogram(alpha = 0.3, aes(), position = 'identity')     

twee%>%
  ggplot(aes(x=)))+
  geom_histogram()+
  labs(x="cholesterol",
       y="aantal")
