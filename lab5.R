#Invoke the SDSFoundataions package
library(SDSFoundations)
WR <- WorldRecords

#1. Create a subset of the data that contains World Record cases 
#   for the men's Mile event.
mensmile <- WR[WR$Event=='Mens Mile',]

#2. Create a subset of the data that  contains World Record cases 
#   for the women's Mile event. 
womensmile <- WR[WR$Event=='Womens Mile',] 

#3. Create a scatterplot for each relationship of Mile time and year: 
#   one for men and one for women.  
plot(mensmile$Year,mensmile$Record,main='Mens Mile Records',xlab='Year',ylab='Record Distance (m)',pch=16)
plot(womensmile$Year,womensmile$Record,main='Womens Mile Records',xlab='Year',ylab='Record Distance (m)',pch=16)

#4. Confirm from these plots that a linear model is appropriate.  


#5. Run a linear model for each event and then interpret the results. 
#   Be sure to calculate R-squared values for each model.
mfit <- linFit(mensmile$Year, mensmile$Record)
wfit <- linFit(womensmile$Year,womensmile$Record)

#--------------------------------------
myear <- -1/mfit$Slope
wyear <- -1/wfit$Slope

#--------------------------------------
# coeff of determination
mcor <- round(mfit$r_sq,3)
wcor <- round(wfit$r_sq,3)
