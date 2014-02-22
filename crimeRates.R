#####################
#======
#Purpose: Create a shapefile with crime rates for all municipalities in Mexico
#Author: Jose Gonzalez
#Website: www.jose-gonzalez.org
#Date: 17 Feb 2014
#Copyright: Jose Gonzalez. All rights reserved



#Download data from Diego Valle's blog (Gracias Diego!) http://crimenmexico.diegovalle.net/en/csv/
# Original data source: http://www.secretariadoejecutivo.gob.mx/es/SecretariadoEjecutivo/Incidencia_Delictiva
#====

require(R.utils) 
temp <- tempfile()
download.file("http://crimenmexico.diegovalle.net/en/csv/fuero-comun-municipios.csv.gz",temp)
data <- read.csv(gunzip(temp, "fuero-comun-municipios.csvuero-comun-municipios.csv",overwrite=T))
unlink(temp)


#Total number of crimes 
sum(data$count, na.rm=T) #1,157,425

#This data set only contains a selected number of crimes
#====
names(data)
unique(data[,c("crime","category","type","subtype")])
head(data[data$subtype == "SIN DATOS",])
#Check out this diagram for more info https://raw.github.com/josecarlosgonz/crime/master/images/selected_crimes.png

#Add unique mun id
#====
data$mun_code  <- sprintf("%03s", data$mun_code)
id  <- paste(data$state_code,data$mun_code,sep="")
head(id); tail(id)
data$id  <- id

#Charts
#====
require(ggplot2)
data$date  <- paste(sprintf("%02d",data$month),data$year,sep="-")
data$date  <- paste("01",data$date,sep="-")
table(data$date)
data$date  <- as.Date(data$date, "%d-%m-%Y")


#Chart for total number of crimes
png("images/MonthlyCrimesSmooth.png")
c <- ggplot(data, aes(date, count)) + stat_smooth()
c + ggtitle("Number of selected crimes per municipality") +xlab("Month") + ylab("Number of selected crimes")  
##Looks like the total number of monthly selected crimes per municipality is decreasing
dev.off()

#Chart for number of homicides
table(data$crime)
png("images/homicidesSmooth.png")
d <- ggplot(subset(data, data$crime=="HOMICIDIOS"), aes(date, count)) + stat_smooth()
d + ggtitle("Number of homicides per municipality") +xlab("Month") + ylab("Number of homicides")  
##Looks like the total number of homicides per municipality may be decreasing too
dev.off()

#Crime rates for 2011, 2012 and 2013
#=====
library(reshape)
library(plyr)
names(data)

#Melt data
data  <- subset(data, is.na(data$count) ==FALSE)
head(data)
crime <- ddply(data, c("id","state_code","year"), summarize,
               total = sum(count),
               population = max(population))
head(crime); tail(crime)
sum(data$count, na.rm=T)
sum(crime$total, na.rm=T) #Total number of crimes match 1,157,425
table(population$year) # We have slightly more data for 2013, 1,331 municipalities
ddply(crime, c("year"), summarize, population = prettyNum(sum(population), big.mark =",")) # Population estimates around 107 million look ok
#Calculate crime rate per 100 k people
crime$rate  <- (crime$total / crime$population)*1000
summary(crime$rate)
qplot(crime$rate, crime$year) # who is the outlier?
head(data[data$id == 12037,]) #Looks like a ton of crimes for a population around 6k. 
crime[crime$rate >60,] #Ixcateopan de Cuauhtemoc in Guerrero. 449 crimes are too much for a population of 6k.

#Change data format from long to wide
#======
head(crime)
head(data)
#Melt data
crime  <- melt(crime, id=c("id","state_code","year"), measured=c("total","rate"))
table(crime$variable)

#Reshape data from long to wide
head(crime)
crime  <- cast(crime, id + state_code ~ variable + year)
sum(crime$total_2011,na.rm=T) + sum(crime$total_2012,na.rm=T) + sum(crime$total_2013,na.rm=T) #Boom numbers are ok


#Format numbers
head(crime); names(crime)
cols  <- c(8:11)
crime[,cols] <- apply(crime[,cols], 2, function(x) signif(x, digits=2))
crime[is.na(crime)]  <- 0
head(crime)
write.csv(crime, "crimesPerYear.csv", row.names=F)
