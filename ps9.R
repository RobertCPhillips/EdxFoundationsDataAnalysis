library(SDSFoundations)
post <- PostSurvey

#--------------------------------------------------
#Question 1: Is the increase in time spent studying 
# from high school to college the same for nursing majors 
# and biology majors? (independent)
#--------------------------------------------------

post$hw_hours_diff <- post$hw_hours_college - post$hw_hours_HS
diff_nursing <- post$hw_hours_diff[post$major=="Nursing"]
diff_biology <- post$hw_hours_diff[post$major=="Biology"]

hist(diff_nursing)
hist(diff_biology)

t.test(diff_biology, diff_nursing)

#--------------------------------------------------
#Question 2: Researchers expect smoking to raise 
# pulse rates? (independent)
#--------------------------------------------------
n_smokers <- 26
mu_smokers <- 80
sd_smokers <- 5

n_nonsmokers <- 32 	
mu_nonsmokers <- 74 	
sd_nonsmokers <- 6

df <- if (n_smokers <= n_nonsmokers) n_smokers-1 else n_nonsmokers-1

se <- round(sqrt(sd_smokers^2/n_smokers + sd_nonsmokers^2/n_nonsmokers),2)
t = round((mu_smokers-mu_nonsmokers)/se,2)

st <- 1-.05/1
cr <- round(qt(st,df=df)*c(-1,1),3)

#--------------------------------------------------
#Question 3: Compare CP levels from a monkey. (Dependent)
#--------------------------------------------------
#d = CPleft - CPright
cpleft <- c(16.3, 4.8, 10.7, 14, 15.7, 9.9, 29.3, 20.4, 15.7, 7.6, 16.2, 14.7, 15, 8.4, 23.3, 17.7)
cpright <- c(11.5, 3.5, 12.8, 7.9, 15.2, 9.8, 24, 14.9, 12.6, 8.2, 8.4, 11, 12.5, 9.2, 17.5, 11.1)
n <- length(cpleft)
df <- n-1

d <- cpleft - cpright
dbar <- mean(d)
delta <- 0
ds <- sum((d-dbar)^2)

sd <- sqrt(ds/df)
se <- sd/sqrt(n)

t <- (dbar - delta)/se

st <- 1-.05/2
cr <- qt(st,df=df)

ci <- round(dbar + c(-1,1)*se*cr,2)

