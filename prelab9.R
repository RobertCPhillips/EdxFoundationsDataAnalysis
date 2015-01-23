library(SDSFoundations)
post <- PostSurvey

#--------------------------------------------------
#Question 1:  Independent t-test
#--------------------------------------------------

#1. Make a vector of happiness scores for each sample (under- and upper-classmen).
sunder <- post$happy[post$classification == "Freshman" | post$classification == "Sophomore"] 
supper <- post$happy[post$classification == "Junior" | post$classification == "Senior"] 

xbar_under <- mean(sunder)
xbar_upper <- mean(supper)

#2. Generate histograms to check the Normality assumption. 
hist(sunder)
hist(supper)

#3. Run an independent t-test.
t.test(sunder,supper)

#4. Interpret the results.

#--------------------------------------------------
#Question 2:  Dependent t-test
#--------------------------------------------------

#1. Make a vector of difference scores for student happiness from the beginning to end of semester.
post$diff_happy <- post$happy - post$post_happy
dbar <- mean(post$diff_happy)

#2. Generate a histogram of the difference scores to check the Normality assumption.
hist(post$diff_happy)

#3. Run a dependent t-test.
t.test(post$happy,post$post_happy,paired=TRUE)

#4. Interpret the results.




