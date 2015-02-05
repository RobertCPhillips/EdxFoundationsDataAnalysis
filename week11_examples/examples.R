#------------------------------------------------
# example 1
#------------------------------------------------
customers <- read.csv('customer.csv', header=T)

# Calculate the total sum of squares
n <- sum(!is.na(customers))
sst <- sum(customers^2) - (sum(customers)^2)/n

# Calculate the sum of squares between
ybar <- sum(customers)/n
ybars_k <- colMeans(customers)
rows <- apply(methods,2,function(x){sum(!is.na(x))})
k <- dim(customers)[2]

ssb <- sum(rows*(ybars_k - ybar)^2)

# Find the sum of squares within groups
ssw <- sst - ssb

# Next solve for degrees of freedom for the test
dfTotal = n - 1
dfBetween = k - 1
dfWithin = n - k

# Calculate the Mean Squares Between (MSB) and Mean Squares Within (MSW)
msb = ssb/dfBetween
msw = ssw/dfWithin

# Calculate the F statistic using the following ratio:
f = msb/msw

fc <- qf(.95, dfBetween, dfWithin)

#------------------------------------------------
# example 2
#------------------------------------------------
methods <- read.csv('methods.csv', header=T)

# Calculate the total sum of squares
n2 <- sum(!is.na(methods))
sst2 <- sum(methods^2,na.rm=T) - (sum(methods,na.rm=T)^2)/n2

# Calculate the sum of squares between
ybar2 <- sum(methods,na.rm=T)/n2
ybars_k2 <- colMeans(methods,na.rm=T)
rows2 <- apply(methods,2,function(x){sum(!is.na(x))})
k2 <- dim(methods)[2]

ssb2 <- sum(rows2*(ybars_k2 - ybar2)^2)

# Find the sum of squares within groups
ssw2 <- sst2 - ssb2

# Next solve for degrees of freedom for the test
dfTotal2 = n2 - 1
dfBetween2 = k2 - 1
dfWithin2 = n2 - k2

# Calculate the Mean Squares Between (MSB) and Mean Squares Within (MSW)

msb2 = ssb2/dfBetween2
msw2 = ssw2/dfWithin2

# Calculate the F statistic using the following ratio:
f2 = msb2/msw2

fc2 <- qf(.95, dfBetween2, dfWithin2)
