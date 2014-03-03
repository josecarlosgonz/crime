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
homicides  <- homicides[,c(-2,-6:-8)]
names
names(homicides)  <- c("id","h2011","h2012","h2013","rh2011","rh2012","rh2013")
names(homicides)

data  <- merge(crime,homicides, by="id",all=T)
str(crime)
str(data)
length(unique(data$id))
getwd()
write.csv(data, "crimestats.csv", row.names=F)
write
#The file gets corrupted when mergin in R so I used QGIS
#====

#Change string fields to numeric in shapefile
#====


library(foreign)
names(poverty)
cols  <- c(5:22)
poverty  <- read.dbf("shapefiles/crimestats.dbf", as.is=T)
names(poverty)
poverty[,cols] <- apply(poverty[,cols], 2, function(x) gsub(",","",x))
poverty[,cols] <- apply(poverty[,cols], 2, function(x) as.numeric(x))
#Round up numbers

poverty[,cols] <- apply(poverty[,cols], 2, function(x) signif(x, digits=2))
str(data)
write.dbf(data,"shapefiles/crimestats.dbf")
#Check if crimes have increased or decreased

