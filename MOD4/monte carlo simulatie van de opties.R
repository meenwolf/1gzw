score_berekenen<- replicate(100000, {
weging<- sample(1:10,6, replace=T)
hoofdidee<- c(5,1,1,5,1,5)
alt1<- c(3,2,2,3,3,2)
alt2<- c(2,4,4,2,4,3)
alt3<- c(1,5,5,1,5,1)
scorehoofd<- sum(weging*hoofdidee)
score1<- sum(weging*alt1)
score2<- sum(weging*alt2)
score3<- sum(weging*alt3)
maximum<-(c(scorehoofd,score1,score2,score3))
which.max(maximum)
})
waarden<- hist(score_berekenen, main="Hoevaak welk idee gekozen werd bij simulatie 1000 keer", xlab="Optie 1,2,3 en 4",
     ylab="frequenty", breaks=c(0,1,2,3,4))
barplot(waarden$counts, names.arg = c(1,2,3,4), ylab="Frequentie",
        xlab="Optie", main="Monte carlo simulatie hoevaak de opties als beste naar voren komen")
