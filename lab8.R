library(SDSFoundations)
bull <- BullRiders

h0 <- .5
rp <- bull$RidePer

mu <- mean(rp)
sig <- sd(rp)

n <- length(rp)
se <-  sig/sqrt(n)

hist(rp, main='Histogram of Bull Rider RidePer',xlab='Per')

tt <- t.test(rp, mu=h0, alternative=c("two.sided"))

