#load libraries

library(dplyr)
library(tidyr)
library(ggplot2)

#Medicare Charge Data from 2014
medicare.df <- read.csv('Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv')
medicare.df$Provider.Id <- factor(medicare.df$Provider.Id)

#Include Census Regions in Medicare Charge Data
regions <- read.csv('censusregions.csv')

medicare.df <- merge(medicare.df,regions,by.x="Provider.State", by.y = "State.Code")

#Add info about zip codes to data. 
#Include mean income, median income, and population (2006-2010)
income <- read.csv('Income.by.zipcode.csv')
medicare.df <- merge(medicare.df,income,by.x="Provider.Zip.Code", by.y = "Zip")

#Include unemployment and density
unemployment <- read.csv('Unemployment+Unsorted.csv')
medicare.df <- merge(medicare.df, unemployment,by.x="Provider.Zip.Code", by.y = "Zip")

density <- read.csv('Zipcode-ZCTA-Population-Density-And-Area-Unsorted.csv')
medicare.df <- merge(medicare.df, density,by.x="Provider.Zip.Code", by.y = "Zip.ZCTA")



#Look at histograms of continuous variables from original data set

ggplot(medicare.df, aes(Total.Discharges)) + geom_histogram(binwidth = 50)

ggplot(medicare.df, aes(Average.Covered.Charges)) + geom_histogram(binwidth = 10000)

ggplot(medicare.df, aes(Average.Total.Payments)) + geom_histogram(binwidth = 10000)

ggplot(medicare.df, aes(Average.Medicare.Payments)) + geom_histogram(binwidth = 10000)



#Since data is all very skewed right, look at the histogram of the logs of each.
medicare.df$log.Total.Discharges <- log(medicare.df$Total.Discharges)
medicare.df$log.Average.Covered.Charges <- log(medicare.df$Average.Covered.Charges)
medicare.df$log.Average.Total.Payments <- log(medicare.df$Average.Total.Payments)
medicare.df$log.Average.Medicare.Payments <- log(medicare.df$Average.Medicare.Payments)

ggplot(medicare.df, aes(log.Total.Discharges)) + geom_histogram(binwidth = 0.1)

ggplot(medicare.df, aes(log.Average.Covered.Charges)) + geom_histogram(binwidth = 0.25)

ggplot(medicare.df, aes(log.Average.Total.Payments)) + geom_histogram(binwidth = 0.25)

ggplot(medicare.df, aes(log.Average.Medicare.Payments)) + geom_histogram(binwidth = 0.25)

# take a random sample of size 200 from a dataset medicare.charge.14 
# sample without replacement
medicare.sample <- medicare.df[sample(1:nrow(medicare.df), 200,
                          replace=FALSE),]

#Scatterplot matrix for random sample of medicare data

pairs(~Total.Discharges+Average.Covered.Charges+Average.Total.Payments+Average.Medicare.Payments,data=medicare.sample, 
      main="Scatterplot Matrix")

pairs(~log.Total.Discharges + log.Average.Covered.Charges+log.Average.Total.Payments+log.Average.Medicare.Payments,data=medicare.sample, 
      main="Scatterplot Matrix Using Logs")

#Plot log.Average.Medicare.Payments vs. log.Average.Total.Payments, colored by region
ggplot(medicare.sample, aes(x = log.Average.Total.Payments, y = log.Average.Medicare.Payments, col = Region)) + 
  geom_point()


#________________________________________________________________________________________________________________
# 8-12-16 Work

# Look at the distributions of income, density, and unemployment
medicare.df$Median.Income.ZC <- as.numeric(medicare.df$Median.Income.ZC)
medicare.df$Mean.Income.ZC <- as.numeric(medicare.df$Mean.Income.ZC)
medicare.df$Pop.ZC <- as.numeric(medicare.df$Pop.ZC)

ggplot(medicare.df, aes(Median.Income.ZC)) + geom_histogram(binwidth = 500)

ggplot(medicare.df, aes(Mean.Income.ZC)) + geom_histogram(binwidth = 500)

ggplot(medicare.df, aes(Density.Per.Sq.Mile)) + geom_histogram(binwidth = 1500)

ggplot(medicare.df, aes(Unemp.Rate)) + geom_histogram(binwidth = 0.01)

# Transform distributions that are not symmetric and graph again
medicare.df$log.Density.Per.Sq.Mile <- log(medicare.df$Density.Per.Sq.Mile)
ggplot(medicare.df, aes(log.Density.Per.Sq.Mile)) + geom_histogram(binwidth = 0.5)

medicare.df$log.Unemp.Rate <- log(medicare.df$Unemp.Rate)
ggplot(medicare.df, aes(log.Unemp.Rate)) + geom_histogram(binwidth = 0.2)

#Look at correlations between all the numerical data I have so far. 

medicare.num <- data.frame(medicare.df$Total.Discharges, medicare.df$Average.Covered.Charges, medicare.df$Average.Total.Payments, medicare.df$Average.Medicare.Payments,
                           medicare.df$Median.Income.ZC, medicare.df$Mean.Income.ZC, medicare.df$Pop.ZC, medicare.df$Unemp.Rate, medicare.df$Density.Per.Sq.Mile, 
                           medicare.df$log.Total.Discharges, medicare.df$log.Average.Covered.Charges, medicare.df$log.Average.Total.Payments, 
                           medicare.df$log.Average.Medicare.Payments, medicare.df$log.Density.Per.Sq.Mile, medicare.df$log.Unemp.Rate)

cor(medicare.num)
#### I don't think this method ended up being very efficient. Is there a faster way to find correlations?


# Check out scatterplots, include some new variables about the Zipcodes in which the hospital is located
# Of the payments variables, I only included Average.Total.Payments because that's the variable I eventually want to predict. 
pairs(~Average.Total.Payments + log.Average.Total.Payments + Median.Income.ZC, Mean.Income.ZC, Unemp.Rate, Density.Per.Sq.Mile, 
      log.Unemp.Rate, log.Density.Per.Sq.Mile, data = medicare.df, main="Scatterplot Matrix")

###### This didn't work. I think the problem has to do with NA's in the data set or because I was taking the log of zero... 





## Create different data tables for most common DRG Defintions

summary(medicare.df$DRG.Definition)
# Summary tells me that the top 5 most common DRG definitions are: 
# 1.)     194 - SIMPLE PNEUMONIA & PLEURISY W CC
# 2.)     871 - SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W MCC 
# 3.)     292 - HEART FAILURE & SHOCK W CC
# 4.)     392 - ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W/O MCC 
# 5.)     690 - KIDNEY & URINARY TRACT INFECTIONS W/O MCC 

medicare.194 <- filter(medicare.df, DRG.DEFINITION == "194 - SIMPLE PNEUMONIA & PLEURISY W CC")

###### This also did not work. Help!













#Future Step: Use some of this additional data about each of the hospitals. Which of these should I use? How can I add on only a few of 
# these columns to my other data frame? Do I want to get into using the categorical variables? What kind of model would I use? 

hospitals <- read.csv("Hospital_General_Information.csv")
str(hospitals)
