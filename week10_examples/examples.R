library(SDSFoundations)

#Observed = actual count values in each category 
#Expected = the predicted (expected) counts in each category 
#           if the null hypothesis were true 
# df =  number of levels in our categorical variable and subtracting one

#H0 :The population distribution of the variable is the same as the proposed distribution
#HA :The distributions are different

#1 a survey in 2011 and determined that 60% of dog owners have only 
#  one dog, 28% have two dogs, and 12% have three or more. Data 
#  collected: Out of 129 dog owners, 73 had one dog and 38 had two dogs.

total_observed <- 129
expected <- c(.60*total_observed, .28*total_observed, .12*total_observed)
observed <- c(73, 38, 18)

df <- length(observed)-1
chi_squared <- sum(((observed-expected)^2)/expected)
qchi <- qchisq(.95, df=df)
reject <- chi_squared > qchi

#2 She says that male drivers are held responsible in 65% of accidents involving
# driversunder 23. If Eric does some research of his own and discovers that 
# 46 out of the 85 accidents he investigates involve male drivers

total_observed2 <- 85
expected2 <- c(.65*total_observed2, .35*total_observed2)
observed2 <- c(46, 85-46)

df2 <- length(observed2)-1
chi_squared2 <- sum(((observed2-expected2)^2)/expected2)
qchi2 <- qchisq(.95, df=df2)
reject2 <- chi_squared2 > qchi2

#3 Given a choice between a Ford Mustang or Chevy Camaro, 
#  51% of readers will choose a Camaro. Data collected: 
#  Mustang owners: 28, Camaro owners: 34

total_observed3 <- 28 + 34
expected3 <- c(.51*total_observed3, .49*total_observed3)
observed3 <- c(34, 28)

df3 <- length(observed3)-1
chi_squared3 <- sum(((observed3-expected3)^2)/expected3)
qchi3 <- qchisq(.95, df=df3)
reject3 <- chi_squared3 > qchi3

#4 Tuscany claims that 70% of local pet owners own a dog, 
#  and 30% own a cat. Sayber decides to test her claim and 
#  learns that 23 of the 40 people he asks own dogs, and 17 own cats.

total_observed4 <- 40
expected4 <- c(.70*total_observed4, .30*total_observed4)
observed4 <- c(23, 17)

df4 <- length(observed4)-1 
chi_squared4 <- sum(((observed4-expected4)^2)/expected4)
qchi4 <- qchisq(.90, df=df4)
reject4 <- chi_squared4 > qchi4

#----------------------------------------------------------
#--part 2 - independence

#H0 :There is no association between the two categorical variables 
#HA :There is an association (the two variables are not independent)

#df = (rows-1)(columns-1)

#1
data1 <- data.frame(rbind(c(30, 145, 95),c(10, 44, 12)), row.names=c("Yes","No"))
colnames(data1) <- c("Not Happy","Pretty Happy","Very Happy")

df1 <- (dim(data1)[1]-1) * (dim(data1)[2]-1)
chit1 <- chisq.test(data1)
qchi1 <- qchisq(.95, df=df1)
reject1 <- as.numeric(chit1$statistic) > qchi1

#2
data2 <- data.frame(rbind(c(72, 489),c(48, 530)), row.names=c("Female","Male"))
colnames(data2) <- c("Black-White","Color")

df2 <- (dim(data2)[1]-1) * (dim(data2)[2]-1)
chit2 <- chisq.test(data2)
qchi2 <- qchisq(.95, df=df2)
stat2 <- as.numeric(chit2$statistic)
reject2 <- stat2 > qchi2

#3
data3 <- data.frame(rbind(c(29,12,61),c(8,47,56)), row.names=c("Sing","Marr"))
colnames(data3) <- c("Pep","Sau","Che")

df3 <- (dim(data3)[1]-1) * (dim(data3)[2]-1)
chit3 <- chisq.test(data3)
qchi3 <- qchisq(.95, df=df3)
stat3 <- as.numeric(chit3$statistic)
reject3 <- stat3 > qchi3
