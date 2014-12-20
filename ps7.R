library(SDSFoundations)
survey <- StudentSurvey

#---------------------------------------
#q1
#---------------------------------------
hist(survey$austin)
pop_mn <- mean(survey$austin)
pop_sd <- sd(survey$austin)

s10_se <- pop_sd/sqrt(10)

#Samples of size n=10
xbar10 <- rep(NA, 1000)
for (i in 1:1000) {
  x <- sample(survey$austin, size=10)
  xbar10[i] <- mean(x)
}

hist(xbar10,xlim=c(2,10))
s10_mn <- mean(xbar10)
s10_sd <- sd(xbar10)

#---------------------------------------
#q2
#---------------------------------------
p3.2 <- pnorm(3.2,3.08,.4,lower.tail=F)
z3.2 <- (3.2-3.08)/.4

s25_se <- .4 / sqrt(25)

p2.9_to3.2 <- round(pnorm(3.2,3.08,s25_se,lower.tail=T) - 
                    pnorm(2.9,3.08,s25_se,lower.tail=T),3)

#---------------------------------------
#q3
#---------------------------------------
s23_se <- 11 / sqrt(23) 
z35.1 <- round((35.1-28)/s23_se,1)
p35.1 <- pnorm(z35.1,lower.tail=F)

#---------------------------------------
#q4
#---------------------------------------
q4_sd <- 1.5
q4_n <- 15
s15_mn <- 471.46

s15_se <- round(q4_sd / sqrt(q4_n),3)
s15_me <- round(1.96*s15_se,3)

s15_ci <- s15_mn + s15_me*c(-1,1)







