#You have just taken ownership of a pizza shop. 
# The previous owner told you that you would save money if you bought the 
# mozzarella cheese in a 4.5 pound slab. Each time you purchase a slab of 
# cheese, you weigh it to ensure that you are receiving 72 ounces of cheese. 
# The results of 7 random measurements are 70, 69, 73, 68, 71, 69 and 71 ounces. 

#Are these differences due to chance or is the distributor giving you less cheese 
#than you deserve?

mu <- 72

samples <- c(70,69,73,68,71,69,71)
muhat <- mean(samples)
s <- sd(samples)
n <- length(samples)
se <- s/sqrt(n)

t <- (muhat-mu)/se

a1 <- .1/2
c1 <- qt(1-a1,df=n-1)

a2 <- .05/2
c2 <- qt(1-a2,df=n-1)

a3 <- .01/2
c3 <- qt(1-a3,df=n-1)



