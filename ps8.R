library(SDSFoundations)
bull <- BullRiders

#-------------------------------------------------------
# q1
#-------------------------------------------------------
bull$EE <- bull$Earnings / bull$Events
bull$LogEE <- log(bull$Earnings / bull$Events)

hist(bull$EE, main='Histogram of Bull Rider EE',xlab='EE')
hist(bull$LogEE, main='Histogram of Bull Rider EE',xlab='LogEE')

ee <- bull$LogEE

mu <- mean(ee)
sig <- sd(ee)

n <- length(ee)
se <-  sig/sqrt(n)

tt <- t.test(bull$LogEE)
lb <- tt$conf.int[1]
ub <- tt$conf.int[2]

lb2 <- exp(round(lb,2))
ub2 <- exp(round(ub,2))

#-------------------------------------------------------
# q2
#-------------------------------------------------------
h0 <- 28.5
samples <- c(29.4,29.0,28.4,28.8,28.9,29.3,28.5,28.2)

mu2 <- mean(samples)
sig2 <- sd(samples)

n2 <- length(samples)
se2 <-  sig2/sqrt(n2)

tt2 <- t.test(samples, mu=h0)

#-------------------------------------------------------
# q3
#-------------------------------------------------------
h0 <- 91

n3 <- 25
mu3 <- 93.6
sig3 <- 7.8

se3 <- sig3/sqrt(n3)
t3 <- (mu3-h0)/se3
c3 <- qt(.95, df=n3-1)

mu3b <- 95
t3b <- (mu3b-h0)/se3

#-------------------------------------------------------
# q4
#-------------------------------------------------------

n4 <- 12
c4 <- round(qt(.95,df=n4-1),3)

mu4 <- 42.6
sig4 <- 5.3
se4 <- sig4/sqrt(n4)

ci4 <- round(mu4 + c(-1,1)*se4*c4,2)

c4b <- round(qt(.975,df=n4-1),3)
ci4b <- round(mu4 + c(-1,1)*se4*c4b,2)





