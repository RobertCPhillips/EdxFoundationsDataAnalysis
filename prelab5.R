#Invoke the SDSFoundataions package
library(SDSFoundations)
WR <- WorldRecords

#Subset the data

menshot <- WR[WR$Event=='Mens Shotput',]
womenshot <- WR[WR$Event=='Womens Shotput',] 

#Create scatterplots

plot(menshot$Year,menshot$Record,main='Mens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)
plot(womenshot$Year,womenshot$Record,main='Womens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)

#Run linear models

linFit(menshot$Year, menshot$Record)
linFit(womenshot$Year,womenshot$Record)