#independent samples
s1 <- c(35, 51, 66, 42, 37, 46, 60, 55, 53)
s2 <- c(52, 87, 76, 62, 81, 71, 55, 67)

n1 <- length(s1)
n2 <- length(s2)
df <- if (n1 <= n2) n1-1 else n2-1

xbar1 <- mean(s1)
xbar2 <- mean(s2)
sd1 <- sd(s1)
sd2 <- sd(s2)

se <- sqrt(sd1^2/n1 + sd2^2/n2)
t = (xbar1-xbar2)/se

st <- 1-.05/2
cr <- qt(st,df=df)*c(-1,1)
