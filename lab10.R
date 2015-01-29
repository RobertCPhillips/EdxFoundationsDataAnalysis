library(SDSFoundations)
acl <- AustinCityLimits

#-----------------------------------------------------------------------
# 1. Are each of the four musical genres equally represented on Austin City Limits?   
# (Goodness of Fit Test)
#-----------------------------------------------------------------------
observed1 <- table(acl$Genre)
expected1 <- c(.25, .25, .25, .25)
t1 <- chisq.test(observed1,p=expected1)
t1$expected
t1

#-----------------------------------------------------------------------
# 2. Are some genres more likely to draw a large (100K+) Twitter following than others?
# (Independence Test)
#-----------------------------------------------------------------------
observed2 <- table(acl$Genre, acl$Twitter.100k)
margins2 <- prop.table(observed2,1)
t2 <- chisq.test(observed2,correct=F)
t2$expected
t2



