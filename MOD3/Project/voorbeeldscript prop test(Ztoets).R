# z-toets uitvoeren voor de laag- en hoog opgeleiden
# een tabelletje maken met twee waarden: succes aantal en totaal
# succes is hier aantal wat niet aan de norm voldoet
#(dit kan ook iets anders zijn als je een andere vraag hebt)
laagopl<- data %>%
  filter(! is.na(V1)| ! is.na(Opleiding_her))%>%
  filter(Opleiding_her==1)%>%
  mutate(totaal=n())%>%
  filter(V1 == 1)%>%
  mutate(aantal=n())%>%
  select(aantal, totaal)%>%
  summarise(first(aantal), first(totaal))
view(laagopl)
# hetzelfde doen we voor de categorie waarvoor we willen kijken of er een
#significant verschil tussen zit
# ik gebruik hier V1==1 omdat dit een deelvraag was. gebruik voor onorm : V1 %in%c(1,2,3,4,5)
hoogopl<- data %>%
  filter(! is.na(V1)| ! is.na(Opleiding_her))%>%
  filter(Opleiding_her==3)%>%
  mutate(totaal=n())%>%
  filter(V1 ==1)%>%
  mutate(aantal=n())%>%
  select(aantal, totaal)%>%
  summarise(first(aantal), first(totaal))
view(hoogopl)

# nu gebruiken we prop.test() dit is een functie waarin je twee kollomen opgeeft
# prop.test(c(succes1, succes2), c(totaal1, totaal2)) de dubbele blokhaken zorgen
# ervoor dat r een kolom als numeric noteerd. In ons geval heeft het betrekking op 
# objecten van 1 rij en twee kolommen, dubble blokhaken geeft dus een getal,
# een argument van de kolom. 
prop.test(c(laagopl[[1]], hoogopl[[1]]), c(laagopl[[2]], hoogopl[[2]]))
# de p- waarde tussen de laag en hoogopgeleiden is kleinder dan 0.05: is zeker significant.
# lageropgeleiden hebben dus inderdaad een hoger percentage mensen dat niet aan 
# de beweegnorm voldoet

# om dit werkend te krijgen op je eigen stukje kan je spelen met de filterfuncties.
# noem de objecten anders, zodat het voor jou duidelijk is welke je moet kiezen in prop.test()
# succes!

27/103
