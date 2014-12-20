library(SDSFoundations)
survey <- StudentSurvey

happy <- survey$happy
hist(happy)

pop_mn <- mean(happy)
pop_sd <- sd(happy)

#---------------------------
# n = 5
#---------------------------
n5 <- 5

xbar5 <- rep(NA, 1000)
for (i in 1:1000) {
  x <- sample(happy, size=n5)
  xbar5[i] <- mean(x)
}

hist(xbar5)

s5_mn <- mean(xbar5)
s5_sd <- sd(xbar5)
s5_se <- pop_sd/sqrt(n5)

#---------------------------
# n = 15
#---------------------------
n15 <- 15

xbar15 <- rep(NA, 1000)
for (i in 1:1000) {
  x <- sample(happy, size=n15)
  xbar15[i] <- mean(x)
}

hist(xbar15)

s15_mn <- mean(xbar15)
s15_sd <- sd(xbar15)
s15_se <- pop_sd/sqrt(n15)

#---------------------------
# n = 25
#---------------------------
n25 <- 25

xbar25 <- rep(NA, 1000)
for (i in 1:1000) {
  x <- sample(happy, size=n25)
  xbar25[i] <- mean(x)
}

hist(xbar25)

s25_mn <- mean(xbar25)
s25_sd <- sd(xbar25)
s25_se <- pop_sd/sqrt(n25)















