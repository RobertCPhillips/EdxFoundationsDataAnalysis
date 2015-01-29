
#1
total_observed1 <- 38+28+24
expected1 <- c(.33*total_observed1, .33*total_observed1, .33*total_observed1)
expected1 <- round(expected1)
observed1 <- c(38, 28, 24)

df1 <- length(observed1)-1
chi_squared1 <- sum(((observed1-expected1)^2)/expected1)
qchi1 <- qchisq(.95, df=df1)
reject1 <- chi_squared1 > qchi1


#2
total_observed2 <- 12+36+32
expected2 <- c(.20*total_observed2, .45*total_observed2, .35*total_observed2)
observed2 <- c(12, 36, 32)

df2 <- length(observed2)-1
chi_squared2 <- sum(((observed2-expected2)^2)/expected2)
qchi2 <- qchisq(.95, df=df2)
reject2 <- chi_squared2 > qchi2


#3
data3 <- data.frame(rbind(c(68,109),c(94,89)), row.names=c("Fear","No Fear"))
colnames(data3) <- c("Men","Women")

df3 <- (dim(data3)[1]-1) * (dim(data3)[2]-1)
chit3 <- chisq.test(data3)
expected3 <- chit3$expected
qchi3 <- qchisq(.95, df=df3)
stat3 <- as.numeric(chit3$statistic)
reject3 <- stat3 > qchi3
