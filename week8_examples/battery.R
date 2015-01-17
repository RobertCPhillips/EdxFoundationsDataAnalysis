#Duracell manufactures batteries that the CEO claims will last an average of 
# 300 hours under normal use. A researcher randomly selected 20 batteries from 
# the production line and tested these batteries. The tested batteries had a mean 
# life span of 270 hours with a standard deviation of 50 hours. 

#Do we have enough evidence to suggest that the claim of an average lifetime of 
# 300 hours is false?

mu <- 300

n <- 20
muhat <- 270
s <- 50

se <- s/sqrt(n)
t <- (muhat-mu)/se

a <- .05/2
c <- qt(1-a,df=n-1)

me <- c*se
xx <- muhat + me*c(-1,1)
