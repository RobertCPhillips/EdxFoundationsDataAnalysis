#Invoke the SDSFoundataions package
library(SDSFoundations)
WR <- WorldRecords

#Subset the data
menspv <- WR[WR$Event=='Mens Polevault' & WR$Year >= 1970,]

plot(menspv$Year,menspv$Record,main='Mens P.V. World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)
linFit(menspv$Year, menspv$Record)

#---------------------------------------------------

C <- c(140,280,420,560)
h <- c(0,2,4,6)

plot(h,C)

#---------------------------------------------------

a1 <- 2.84 + .04*970/100
a2 <- 3.71-2.84
a3 <- 2.91 - (2.84 + .04*1450/100)
