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
