library(SDSFoundations)

#------------------------------------------------------
# part 1
#------------------------------------------------------
world <- WorldBankData 
startYear <- 1995

brazil <- world[world$Country == "Brazil",]
brazil1995 <- brazil[brazil$year >= startYear,]

brazil1995$year <- brazil1995$year - startYear
brazil1995$mobile.users <- brazil1995$mobile.users / 1000000

mu2000 <- brazil1995[brazil1995$year == (2000-startYear),"mobile.users"]
mu1m <- head(brazil1995[brazil1995$mobile.users > 100,"year"],1) + startYear

tfit <- tripleFit(brazil1995$year,brazil1995$mobile.users)
lpred <- logisticFitPred(brazil1995$year,brazil1995$mobile.users, 2025-startYear)

#------------------------------------------------------
# part 2
#------------------------------------------------------
a <- 76.64
b <- 1.46
r <- b - 1

q1 <- round((367-257)/257,2)
q2 <- r

t <- 14
q3 <- round(a*b^t)

aa <- 43.59
bb <- 1.57
c <- 3273.31

q4 <- round(c/(1+aa*bb^-t))

actual <- 4379
q5 <- actual-q3
q6 <- actual-q4

#------------------------------------------------------
# part 3
#------------------------------------------------------

year <- c(1996,1998)
year0 <- year - min(year) 
wolves <- c(25,45)

lfit <- linFit(year0,wolves)

efit <- expFit(year0, wolves)
q2 <- round(efit$b - 1,2)
q3 <- efit$a

lp <- linFitPred(year0,wolves,2002-min(year))
ep <- expFitPred(year0,wolves,2002-min(year))

lr <- 147-lp
er <- 147-ep

y <- round(log(325/efit$a)/log(efit$b))
