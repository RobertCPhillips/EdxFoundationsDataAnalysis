#prelab3
library(SDSFoundations)
bull <- BullRiders

q1b <- sum(bull[1:10,]$YearsPro > 10)
q1c <- bull[bull$BuckOuts == min(bull$BuckOuts), "Rides"]

bull$RidesPerEvent <- bull$Rides/bull$Events
hist(bull$RidesPerEvent)
fivenum(bull$RidesPerEvent)

cor(bull$RidesPerEvent,bull$Place)
#################

# Visualize and describe the first variable of interest
hist(bull$RidePer)
fivenum(bull$RidePer)
mean(bull$RidePer)
sd(bull$RidePer)

# Visualize and describe the second variable of interest 
hist(bull$Top10)
fivenum(bull$Top10)
mean(bull$Top10)
sd(bull$Top10)

# Create a scatterplot
plot(bull$RidePer,bull$Top10)

# Add line of best fit
fit <- lm(bull$Top10~bull$RidePer)
abline(fit)
fit$coef[1] + .53*fit$coef[2]

# Calculate the correlation coefficient
cor(bull$RidePer,bull$Top10)

# Create a correlation matrix  
vars <- c("Top10", "RidePer")
cor(bull[,vars])
