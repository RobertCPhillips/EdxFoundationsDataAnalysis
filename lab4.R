library(SDSFoundations)
acl <- AustinCityLimits

genre <- table(acl$Genre)
grammy <- table(acl$Grammy)

t <- table(acl$Genre,acl$Grammy)

barplot(t, legend=F, beside=T)

