library(SDSFoundations)
acl <- AustinCityLimits

#-----------------------------------------------------------------------
# 1. Are there an equal number of male and female performers on Austin City Limits?
# (Goodness of Fit Test)
#-----------------------------------------------------------------------
observed1 <- table(acl$Gender)
expected1 <- c(.50, .50)
t1 <- chisq.test(observed1,p=expected1)
t1$expected
t1


#-----------------------------------------------------------------------
# 2. Are male performers just as likely to have had a Top 10 hit as female performers?
# (Test of Independence)
#-----------------------------------------------------------------------
observed2 <- table(acl$Gender, acl$BB.wk.top10)
t2 <- chisq.test(observed2,correct=F)
t2$expected
t2


