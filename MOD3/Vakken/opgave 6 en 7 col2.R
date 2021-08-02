library(tidyverse)
file.choose()
nier<- read.csv("C:\\Users\\User\\OneDrive\\Documenten\\MOD3\\cvs bestanden\\Data Niertransplantatie College 2.csv", sep= ",")
view(nier)
# opgave 7
nier1<- nier%>%
  filter(! ID %in% c(16,25,33,14,22,30))%>%
  filter(tijd_wachtlijst >= 24)
nrow(nier1)
nier2<- nier1%>%
  mutate(hads7_rec = abs(hads7 - 3), 
         hads10_rec =  abs(hads10 - 3))

  nier3 <- nier2%>% 
  mutate(depressiescore = (hads1+ hads3+ hads5+ hads7_rec+ hads9+ hads10_rec+ hads13))%>%
  mutate(angstscore = (hads2+ hads4+ hads6+ hads8+ hads11+ hads12+ hads14))

nier4<- mutate(nier3, depcat= cut(depressiescore,
                                  breaks= c(0, 7, 10, 21),
                                  labels= c("geen_depressie", "mogelijke_depressie", "vermoedelijke_depressie")))
view(nier4)

nier4.1<- nier4%>%
  select(depcat)%>%
  filter(depcat== "geen_depressie")
nrow(nier4.1)  
# van de mensen die 55 jaar of jonger zijn, eraren 35 mensen geen depressie
# 24 mensen mogelijke een depressie en 12 mensen vermoedelijk een depressie.
#
# van de mensen die ouder zijn dan 55 jaar, ervaren 19 mensen geen depressie,
# 19 mensen mogelijk een depressie en 8 mensen vermoedelijk een depressie.

# hieruit kan geconcludeerd worden dat er bijna 50% van de 55 jaar of jongeren
# geen depressie vertonen, tegenover de ongeveer 40% van de ouderen. Dit geldt
# respectievelijk voor mogelijk een depressie ook. 33% voor de jongeren en 40% 
# voor de ouderen. een vermoedelijke depressie is bij ongeveer 20% van de
# ouderen het geval en bij 16% van de jongeren het geval.

# overall hebben de jongeren minder last van depressieklachten gegeven dat zij 
# op de wachtlijst voor een niertransplantatie staan dan de oudere generatie

# opgave 7

# bij de mensen die korter dan 2 jaar op de wachtlijst staan hebben er 27 geen
# depressie(51%), 34% een mogelijke depressie en 15% een vermoedelijk depressie
# bij de mensen die langer dan 2 jaar op de wachtlijst staan vertonen 24 mensen 
# geen depressie(43%), 39% mogelijk een depressie en 18% vermoedelijk een depressie
# dit geeft aan dat mensen die langer op de wachtlijst staan ook meer depressieklachten
# vertonen. Het percentage geen depressie is lager, de mogelijke en vermoedelijke 
# depressie percentages zijn beide hoger dan die van de mensen met de kortere wachttijd


10/(24+22+10)
8/(18+8+27)
12/72
19/46
24/72
19/46

