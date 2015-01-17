library(SDSFoundations)
bull <- BullRiders

ho <- 190

mu <- mean(bull$Weight)
sig <- sd(bull$Weight)

n <- length(bull$Weight)
se <-  sig/sqrt(n)
                                                       
hist(bull$Weight, main='Histogram of Bull Rider Weights',xlab='Weight (lbs)')

tt <- t.test(bull$Weight, mu=ho)
