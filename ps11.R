library(SDSFoundations)
film <- FilmData

#--------------------------------------------------------------------
#High - med - low budget
#--------------------------------------------------------------------
budget.size <- function(x) {
  bud <- x['Budget']
  if (is.na(bud)) { NA }
  else if (bud < 100) { 'Low' }
  else if (bud >= 100 && bud < 150) { 'Med' }
  else { 'High' }
}

film$budget.size <- apply(film, 1, budget.size)
film$budget.size <- as.factor(film$budget.size)

table(film$budget.size)

pct.dom_mu <- aggregate(Pct.Dom~budget.size, film, mean)

pct.dom_model <- aov(Pct.Dom~budget.size,data=film)
summary(pct.dom_model)

TukeyHSD(pct.dom_model)

#--------------------------------------------------------------------
#2. dogs...
#--------------------------------------------------------------------

sst2 <- 5949.1
ssb2 <- 2387.7

ssw2 <- sst2 - ssb2

# Next solve for degrees of freedom for the test
dfBetween2 = 2
dfWithin2 = 42

# Calculate the Mean Squares Between (MSB) and Mean Squares Within (MSW)

msb2 = ssb2/dfBetween2
msw2 = ssw2/dfWithin2

# Calculate the F statistic using the following ratio:
f2 = msb2/msw2

#--------------------------------------------------------------------
#3. police...
#--------------------------------------------------------------------
#tickets <-  c(8,4,6,8,6,4,3,7,0,2,7,5,1,2,7,6,5,0)
tickets <- cbind(c(8,4,6,8,6,4), c(3,7,0,2,7,5), c(1,2,7,6,5,0))
#sections <- c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3)
n3 <- sum(!is.na(tickets))
sst3 <- sum(tickets^2) - (sum(tickets)^2)/n3

ybar3 <- sum(tickets,na.rm=T)/n3
ybars_k3 <- colMeans(tickets,na.rm=T)
rows3 <- apply(tickets,2,function(x){sum(!is.na(x))})
k3 <- dim(tickets)[2]

ssb3 <- sum(rows3*(ybars_k3 - ybar3)^2)

# Find the sum of squares within groups
ssw3 <- sst3 - ssb3

# Next solve for degrees of freedom for the test
dfTotal3 = n3 - 1
dfBetween3 = k3 - 1
dfWithin3 = n3 - k3

# Calculate the Mean Squares Between (MSB) and Mean Squares Within (MSW)

msb3 = ssb3/dfBetween3
msw3 = ssw3/dfWithin3

# Calculate the F statistic using the following ratio:
f3 = msb3/msw3
fc3 <- qf(.95, dfBetween3, dfWithin3)


#--------------------------------------------------------------------
#4. sleep...
#--------------------------------------------------------------------
sst4 <- 2147.00
ss
ssw4 <- sst4 - ssb4






