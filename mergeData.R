#####################
#======
#Purpose: Create a shapefile with homicides rates for all municipalities in Mexico
#Author: Jose Gonzalez
#Website: www.jose-gonzalez.org
#Date: 22 Feb 2014
#Copyright: Jose Gonzalez. All rights reserved

#Data was previously downloaded from CONEVAL and put in shapefile format

#Merge total crimes and homicides
#====
homicides  <- read.csv("homicidesPerYear.csv")
crime  <- read.csv("crimesPerYear.csv")
head(crime)
head(homicides)
str(crime)
str(homicides)
data  <- merge()

#Due to file encoding issues the 
#====

library(foreign)
poverty  <- read.dbf("shapefiles/crimeAndPoverty.dbf", as.is=T)
write.dbf(poverty, "shapefiles/crimeAndPoverty.dbf")
head(poverty)
names(poverty)

data  <-


