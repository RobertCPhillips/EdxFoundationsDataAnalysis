library(SDSFoundations)
film <- FilmData

titanic <- film[film$Film == 'Titanic',]
uni <- film[film$Studio == 'Uni.',][1,]
imdb <- min(film[film$Rank < 11,'IMDB'])

#--------------------------------------------------------------------
#1. Does a film's rating (PG, PG-13, or R) impact its cost to produce?
#--------------------------------------------------------------------

#1. Identify the number of films in each rating group (PG, PG-13, R).
rating <- table(film$Rating)

#2. Compute the mean and standard deviation of the variable of interest for each group.
rating_mu <- aggregate(Budget~Rating, film, mean)
rating_sd <- aggregate(Budget~Rating, film, sd)

#3. Create boxplots to help visualize group differences and check test assumptions.
boxplot(film$Budget~film$Rating, 
        main="Film Budgets by Rating", ylab="Budget", xlab="MPAA Rating")

#4. Run ANOVA.
rating_model <- aov(Budget~Rating,data=film)
summary(rating_model)

#5. If the F statistic is significant, run a Tukey HSD test to determine which groups are different.
TukeyHSD(rating_model)


#--------------------------------------------------------------------
#2. Does a film's rating (PG, PG-13, or R) influence its IMDB score?
#--------------------------------------------------------------------

#1. Compute the mean and standard deviation of the variable of interest for each group.
imdb_mu <- aggregate(IMDB~Rating,film,mean)
imdb_sd <- aggregate(IMDB~Rating,film,sd)


#2. Create boxplots to help visualize group differences and check test assumptions
boxplot(film$IMDB~film$Rating, 
        main="IMDB Scores by Rating", ylab="IMDB Score", xlab="MPAA Rating")

#3. Run ANOVA
imdb_model <- aov(film$IMDB~film$Rating)
summary(imdb_model)

#4. If the F statistic is significant, run a Tukey HSD test to determine which groups are different.
TukeyHSD(modelscore)

