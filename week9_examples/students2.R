#paired samples
s1 <- c(78,67,56,78,96,82,84,90,87)
s2 <- c(80,69,70,79,96,84,88,92,92)
n <- length(s1)

d <- s2 - s1
dbar <- mean(d)
delta <- 0
ds <- sum((d-dbar)^2)

sd <- sqrt(ds/(n-1))
se <- sd/sqrt(n)

t <- (dbar - delta)/se

st <- 1-.05/2
cr <- qt(st,df=(n-1))*c(-1,1)


