library(SDSFoundations)
film <- FilmData

#--------------------------------------------------------------------
#Days in Theatre
#--------------------------------------------------------------------

studios <- table(film$Studio)

studios_days <- aggregate(Days~Studio, film, mean)

days_model <- aov(Days~Studio,data=film)
summary(days_model)

TukeyHSD(days_model)

#--------------------------------------------------------------------
#Percent Income Domestic
#--------------------------------------------------------------------

domestic_mu <- aggregate(Pct.Dom~Studio, film, mean)

doms_model <- aov(Pct.Dom~Studio,data=film)
summary(doms_model)
TukeyHSD(doms_model)





