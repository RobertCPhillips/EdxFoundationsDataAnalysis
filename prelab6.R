library(SDSFoundations)
world <- WorldBankData 

#------------------------------------------------------
# part 1
#------------------------------------------------------
low <- world[world$IncomeGroup == "Low income",]
aruba1970 <- world[world$Country == "Aruba" & world$year == 1970,]
aus <- world[world$Country == "Australia" 
             & !is.na(world$mobile.user) 
             & world$mobile.users > 0,]

#------------------------------------------------------
# part 2
#------------------------------------------------------

# 1. Create a subset of the dataset that contains only the information for the United States.
us <- world[world$Country.Code == "USA",]

# 2. Create a subset of the US data that contains only the years 1990 to 1999. 
us1990 <- us[us$year >= 1990 & us$year <= 1999,]
us1990$time <- us1990$year - 1990
us1990$internet.mil <- us1990$internet.users / 1000000

# 3. Use a function to fit both an exponential and a logistic model to the data.  
efit <- expFit(us1990$time, us1990$internet.mil)
logfit <- logisticFit(us1990$time, us1990$internet.mil)

# 4. Using each model, predict the number of internet users in 2006. 
epred <- expFitPred(us1990$time, us1990$internet.mil, 16)
lpred <- logisticFitPred(us1990$time, us1990$internet.mil, 16)

# 5. Compare the size of the residuals for 2006 to determine which model appears to have better long-term fit.
internet.mil.2006 <- us[us$year == 2006, "internet.users"] / 1000000
eresid <- internet.mil.2006 - epred
lresid <- internet.mil.2006 - lpred

# 6. Fit both models for the full range of years available (1990 to 2012) to determine which fits the data best.
us$time <- us$year - 1990
us$internet.mil <- us$internet.users / 1000000

tfit <- tripleFit(us$time, us$internet.mil)

epred2012 <- expFitPred(us1990$time,us1990$internet.mil,22)
