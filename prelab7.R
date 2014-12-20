library(SDSFoundations)
survey <- StudentSurvey

#----------------------------------------------
# part 1
#----------------------------------------------
a <- head(survey,10)
sum(a$name_letters > 5)

happy40 <- survey[survey$happy < 40,]
happy40[1,"name_letters"]

#----------------------------------------------
# part 2
#----------------------------------------------
pop_hist <- hist(survey$name_letters)
pop_fivenum <- fivenum(survey$name_letters)
pop_mn <- mean(survey$name_letters)
pop_sd <- sd(survey$name_letters)

# Draw 1,000 samples of n=5 and find the mean of each sample.
xbar5 <- rep(NA, 1000)
for (i in 1:1000) {
  x <-sample(survey$name_letters, size =5)
  xbar5[i] <-  mean(x)
}

# Graph the histogram of 1,000 sample means.
hist(xbar5,xlim=c(2,10))


# Calculate the mean and sd of the sampling distribution.
s5_mn <- mean(xbar5)
s5_sd <- sd(xbar5)
# Compare to the std dev predicted by the CTL.
s5_se <- sd(survey$name_letters)/sqrt(5)

#Repeat for samples of size n=15
xbar15 <- rep(NA, 1000)
for (i in 1:1000) {
  x <-sample(survey$name_letters, size =15)
  xbar15[i] <- mean(x)
}

hist(xbar15,xlim=c(2,10))
s15_mn <- mean(xbar15)
s15_sd <- sd(xbar15)
s15_se <- sd(survey$name_letters)/sqrt(15)

#Repeat for samples of size n=25
xbar25 <- rep(NA, 1000)
for (i in 1:1000) {
  x <-sample(survey$name_letters, size =25)
  xbar25[i] <- mean(x)
}

hist(xbar25,xlim=c(2,10))
s25_mn <- mean(xbar25)
s25_sd <- sd(xbar25)
s25_se <- sd(survey$name_letters)/sqrt(25)








