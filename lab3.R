library(SDSFoundations)
bull <- BullRiders

#Which variable has the strongest linear relationship with Earnings:  
#  Ride Percentage or Cup Points?
head(bull)

hist(bull$Earnings)
fivenum(bull$Earnings)

par(mfrow=(c(2,1)))
plot(bull$Earnings~bull$RidePer)
plot(bull$Earnings~bull$CupPoints)
cor(bull[,c("Earnings","RidePer","CupPoints")])

i <- which(bull$Earnings == max(bull$Earnings))
nooutlier <-bull[-i,]

par(mfrow=(c(2,1)))
plot(nooutlier$Earnings~nooutlier$RidePer)
plot(nooutlier$Earnings~nooutlier$CupPoints)
cor(nooutlier[,c("Earnings","RidePer","CupPoints")])
