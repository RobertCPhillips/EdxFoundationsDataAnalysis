library(SDSFoundations)
post <- PostSurvey

#--------------------------------------------------
#Question 1: Do students at UT spend more time on homework 
#per week in college than they did in highschool? (dependent paired)
#--------------------------------------------------

#1. Create vectors of the scores that you wish to analyze.
post$hw_hours_diff <- post$hw_hours_college - post$hw_hours_HS
dbar <- mean(post$hw_hours_diff)

#2. Check the assumption of normality by generating a histogram for each variable of interest. 
hist(post$hw_hours_HS)
hist(post$hw_hours_college)
hist(post$hw_hours_diff)

#3. Find the t-statistic and p-value.
t.test(post$hw_hours_college, post$hw_hours_HS, alternative = 'greater', paired=T)

#4. Interpret the results of each test. 


#--------------------------------------------------
#Question 2: Do students in fraternities and sororities get 
#less sleep on the weekends than other college students? (independent)
#--------------------------------------------------

#1. Create vectors of the scores that you wish to analyze.
isgreek <- post$sleep_Sat[post$greek=="yes"]
nogreek <- post$sleep_Sat[post$greek=="no"]

sleep_diff <- mean(nogreek) - mean(isgreek)

#2. Check the assumption of normality by generating a histogram for each variable of interest. 
hist(isgreek)
hist(nogreek)

#3. Find the t-statistic and p-value.
t.test(isgreek, nogreek, alternative = 'less')




