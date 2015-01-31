library(SDSFoundations)
acl <- AustinCityLimits

#-----------------------------------------------------------------------
# Q1: We want to know if the proportion of female performers on Austin City 
#     Limits Live has changed in the past two years. 
#-----------------------------------------------------------------------
acl$Recent <- acl$Year == 2012 | acl$Year == 2013
observed1 <- table(acl$Gender, acl$Recent)
t1 <- chisq.test(observed1,correct=F)
t1$expected
t1

#-----------------------------------------------------------------------
# Q2: When crossing white and yellow summer squash, a genetic model predicts 
#     that 75% of resulting offspring will be white, 15% will be yellow and 
#     10% will be green. Below are the results from an experiment run on a 
#     random sample of 205 squash offspring. 
#-----------------------------------------------------------------------
observed2 <- c(152, 39, 14)
expected2 <- c(.75, .15, .10)
t2 <- chisq.test(observed2,p=expected2)
t2$expected
t2
qchi2 <- qchisq(.95, df=2)

#-----------------------------------------------------------------------
# Q3: Approximately 13% of the world's population is left-handed, but is 
#     this proportion the same across men and women? 
#-----------------------------------------------------------------------
data3 <- read.csv('ps10_data3.csv',header=T)
table3 <- table(data3$Gender, data3$Dominant.Hand)
t3 <- chisq.test(table3,correct=F)
t3$expected
t3
qchi3 <- qchisq(.95, df=1)

#-----------------------------------------------------------------------
# Q4: Of the 123 survey respondents, 28 were from rural areas, 42 were 
#     from suburban areas, and 53 were from urban areas.  13 rural 
#     respondents, 35 suburban respondents, and 50 urban respondents said 
#     they had access to internet at home. 
#-----------------------------------------------------------------------
data4 <- matrix(c(13,35,50,28-13,42-35,53-50), nrow=2,ncol=3, byrow=T)
dimnames(data4) <- list(c("Int", "NoInt"),c("Rural","Suburb","Urban"))
haveInternet <- sum(prop.table(data4)["Int",])
noInternet <- sum(prop.table(data4)["NoInt",])

t4 <- chisq.test(data4,correct=F)
t4$expected
t4


