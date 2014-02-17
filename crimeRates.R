#####################
#======
#Purpose: Create a shapefile with crime rates for all municipalities in Mexico
#Author: Jose Gonzalez
#Website: www.jose-gonzalez.org
#Date: 17 Feb 2014
#Copyright: Jose Gonzalez. All rights reserved



#Download data from Diego Valle's blog (Gracias Diego!)
# Original data source: http://www.secretariadoejecutivo.gob.mx/es/SecretariadoEjecutivo/Incidencia_Delictiva
#====
library(R.utils) 
temp <- tempfile()
download.file("http://crimenmexico.diegovalle.net/en/csv/fuero-comun-municipios.csv.gz",temp)
data <- read.csv(gunzip(temp, "fuero-comun-municipios.csv"))
unlink(temp)


#Total number of crimes 
sum(data$count, na.rm=T) #1,157,425

#This data set only contains a selected number of crimes
#====
names(data)
unique(data[,c("crime","category","type","subtype")])
#Check this image for mor info https://raw.github.com/josecarlosgonz/crime/master/images/selected_crimes.png

#Add unique mun id
#====
data$mun_code  <- sprintf("%03d", data$mun_code)
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
crime <- ddply(data, c("id","date","population"), summarize,
               total = sum(count))
crime$rate  <- (crime$total / crime$population)*1000

sum(crime$total)
#crime  <- crime[,c(-3)]
head(crime)
table()
ggplot(crime, aes(x=rate, y=date)) + geom_tile(aes(fill=crime)) 
c <- ggplot(crime, aes(date, total))
c + stat_smooth()

#Melt data
crime <- ddply(data, c("id","year","population"), summarize,
               total = sum(count))
crime$rate  <- (crime$total / crime$population)*1000
sum(crime$total)

crime  <- melt(crime, id=c("id","year"), measured=c("total","rate"))
head(crime)
str(crime)
#Cast data from long to wide
test  <- cast(crime, id ~ variable + year, fun.aggregate=sum)
test  <- as.data.frame(test)
str(test)
sum(test$total_2011) + sum(test$total_2012) + sum(test$total_2013) #Boom numbers are ok
crime  <- test

#Match formatting for both IDs
str(crime)
names(crime)
summary(crime$rate_2013)
summary(crime$rate_2012)
summary(crime$rate_2011)
crime$chR13  <- crime[,10] -crime[,9]
crime$chR12  <- crime[,9] -crime[,8]
qplot(crime$chR13, crime$chR12)

#File gets corrupted with the steps below will try qgis
#====
getwd()
write.csv(crime,"~/Dropbox/Blog/crimeStatistics/crimeRates.csv",row.names=F,fileEncoding="latin1")

