animaladults <- animaldata[animaldata$Age.Intake >= 1,]
table(animaladults$Animal.Type)

par(mfrow=c(2,1))
hist(animaladults[animaladults$Animal.Type=="Dog","Weight"],breaks=12)
hist(animaladults[animaladults$Animal.Type=="Cat","Weight"],breaks=12)

mcat <- mean(animaladults[animaladults$Animal.Type=="Cat","Weight"])
medcat <- median(animaladults[animaladults$Animal.Type=="Cat","Weight"])
sdcat <- sd(animaladults[animaladults$Animal.Type=="Cat","Weight"])
zcat13 <- (13-mcat)/sdcat
pcat13 <- pnorm(zcat13,lower.tail=F)

qtdog <- fivenum(animaladults[animaladults$Animal.Type=="Dog","Weight"])
qtdog
