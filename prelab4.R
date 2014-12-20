library(SDSFoundations)
acl <- AustinCityLimits

#----------------------
#question 1
#----------------------
q1b <- sum(acl[1:10,]$Grammy=="Y")
q1i <- which(acl$Gender=="F" & acl$Age > 60)[1]
q1c <- acl[q1i[1],"Genre"]

#------------------------------
# Create tables of marginal distributions
genre <- table(acl$Genre)
genre
gender <- table(acl$Gender)
gender

# Create contingency table 
twoway <- table (acl$Gender,acl$Genre)
twoway

# Visualize the counts
barplot(twoway, legend=T, beside=T)

# Calculate P(A): the probability of each genre being played
prop.table(genre)

# Calculate P(A|B): the probability of each genre being played, given the artist's gender
prop.table(twoway,1)