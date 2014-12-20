library(SDSFoundations)
world <- WorldBankData 

den <- world[world$Country == "Denmark",]
bel <- world[world$Country == "Belarus",]

den$internet.prop <- den$internet.users / den$population
bel$internet.prop <- bel$internet.users / bel$population

startYear <- 1990
den1990 <- den[den$year >=  startYear,]
bel1990 <- bel[bel$year >=  startYear,]

den1990$years.since.1990 <- den1990$year - startYear
bel1990$years.since.1990 <- bel1990$year - startYear

d.1990.fit.exp <- expFit(den1990$years.since.1990,den1990$internet.prop)
b.1990.fit.exp <- expFit(bel1990$years.since.1990,bel1990$internet.prop)

d.1990.fit.log <- logisticFit(den1990$years.since.1990,den1990$internet.prop)
b.1990.fit.log <- logisticFit(bel1990$years.since.1990,bel1990$internet.prop)

#------------------------

q1a <- round(d.1990.fit.exp$r_sq,4)
q1b <- round(d.1990.fit.log$r_sq,4)
q1d <- round(b.1990.fit.exp$r_sq,4)
q1e <- round(b.1990.fit.log$r_sq,4)

q2a <- round(d.1990.fit.log$C,4)
q2b <- round(d.1990.fit.log$b,2)
q2c <- round(b.1990.fit.log$b,2)
q2d <- round(b.1990.fit.log$C,4)

logisticTime <- function (prop, a, b, C) {
    log (a / ((C / prop) - 1)) / log(b) 
}

q3a <- with(d.1990.fit.log, logisticTime(.1, a, b, C))
q3b <- with(b.1990.fit.log, logisticTime(.1, a, b, C))
q3c <- with(d.1990.fit.log, logisticTime(.8, a, b, C))
q3d <- with(b.1990.fit.log, logisticTime(.8, a, b, C))


