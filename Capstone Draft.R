#load libraries

library(dplyr)
library(tidyr)
library(ggplot2)

#Medicare Charge Data from 2014
medicare.charge14 <- read.csv('Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv')


#Include Census Regions in Medicare Charge Data
regions <- read.csv('censusregions.csv')

medicare.charge14 <- merge(medicare.charge14,regions,by.x="Provider.State", by.y = "State.Code")

#Add info about zip codes to data. 
#Include mean income, median income, and population (2006-2010)
income <- read.csv('Income.by.zipcode.csv')
medicare.charge14 <- merge(medicare.charge14,income,by.x="Provider.Zip.Code", by.y = "Zip")

#Include unemployment and density
unemployment <- read.csv('Unemployment+Unsorted.csv')
medicare.charge14 <- merge(medicare.charge14, unemployment,by.x="Provider.Zip.Code", by.y = "Zip")

density <- read.csv('Zipcode-ZCTA-Population-Density-And-Area-Unsorted.csv')
medicare.charge14 <- merge(medicare.charge14, density,by.x="Provider.Zip.Code", by.y = "Zip.ZCTA")
#Note: This seems to be by ZCTA, not Zipcode. I might need to fix this. 




#Look at histograms of continuous variables from original data set

ggplot(medicare.charge14, aes(Total.Discharges)) + geom_histogram(binwidth = 50)

ggplot(medicare.charge14, aes(Average.Covered.Charges)) + geom_histogram(binwidth = 10000)

ggplot(medicare.charge14, aes(Average.Total.Payments)) + geom_histogram(binwidth = 10000)

ggplot(medicare.charge14, aes(Average.Medicare.Payments)) + geom_histogram(binwidth = 10000)



#Since data is all very skewed right, look at the histogram of the logs of each.
medicare.charge14$log.Total.Discharges <- log(medicare.charge14$Total.Discharges)
medicare.charge14$log.Average.Covered.Charges <- log(medicare.charge14$Average.Covered.Charges)
medicare.charge14$log.Average.Total.Payments <- log(medicare.charge14$Average.Total.Payments)
medicare.charge14$log.Average.Medicare.Payments <- log(medicare.charge14$Average.Medicare.Payments)

ggplot(medicare.charge14, aes(log.Total.Discharges)) + geom_histogram(binwidth = 0.1)

ggplot(medicare.charge14, aes(log.Average.Covered.Charges)) + geom_histogram(binwidth = 0.25)

ggplot(medicare.charge14, aes(log.Average.Total.Payments)) + geom_histogram(binwidth = 0.25)

ggplot(medicare.charge14, aes(log.Average.Medicare.Payments)) + geom_histogram(binwidth = 0.25)

# take a random sample of size 200 from a dataset medicare.charge.14 
# sample without replacement
medicare.sample <- medicare.charge14[sample(1:nrow(medicare.charge14), 200,
                          replace=FALSE),]

#Scatterplot matrix for random sample of medicare data

pairs(~Total.Discharges+Average.Covered.Charges+Average.Total.Payments+Average.Medicare.Payments,data=medicare.sample, 
      main="Scatterplot Matrix")

pairs(~log.Total.Discharges + log.Average.Covered.Charges+log.Average.Total.Payments+log.Average.Medicare.Payments,data=medicare.sample, 
      main="Scatterplot Matrix Using Logs")

#Plot Average.Medicare.Payments vs. Average.Total.Payments, colored by region
ggplot(medicare.sample, aes(x = Average.Total.Payments, y = Average.Medicare.Payments, col = Region)) + 
  geom_point()

#Plot Average.Medicare.Payments vs. Total.Discharges, colored by region
ggplot(medicare.sample, aes(x = Total.Discharges, y = Average.Medicare.Payments, col = Region)) + 
  geom_point()