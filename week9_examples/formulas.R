#independent
n1 <- 1
n2 <- 1
df <- if (n1 <= n2) n1-1 else n2-1

mu1 <- 0
mu2 <- 0

h0 <- (mu1 - mu2 == 0)
ha <- (mu1 - mu2 != 0)

xbar1 <- 0
xbar2 <- 0
sd1 <- 0
sd2 <- 0

se <- sqrt(sd1^2/n1 + sd2^2/n2)
t = ((xbar1-xbar2)-(mu1-mu2))/se

#dependent
s1 <- c()
s2 <- c()
n <- length(s1)

d <- s1 - s2
dbar <- mean(d)
delta <- 0
ds <- sum((d-dbar)^2)
  
sd <- sqrt(ds/(n-1))
se <- sd/sqrt(n)

t <- (dbar - delta)/se






