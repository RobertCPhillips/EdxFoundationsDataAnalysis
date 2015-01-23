#paired samples
s1 <- c(79,95,85,82) #chewing
s2 <- c(80,94,87,84) #not chewing
n <- length(s1)

#difference score = accuracy while chewing - accuracy without gum
d = s1 - s2
dbar = mean(d)
delta <- 0
ds <- sum((d-dbar)^2)

sd <- sqrt(ds/(n-1))
se <- sd/sqrt(n)

t <- (dbar - delta)/se

st <- 1-.05/2
cr <- qt(st,df=(n-1))*c(-1,1)

