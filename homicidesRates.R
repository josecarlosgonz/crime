#####################
#======
#Purpose: Create a shapefile with homicides rates for all municipalities in Mexico
#Author: Jose Gonzalez
#Website: www.jose-gonzalez.org
#Date: 22 Feb 2014
#Copyright: Jose Gonzalez. All rights reserved



#Download data from Diego Valle's blog (Gracias Diego!) http://crimenmexico.diegovalle.net/en/csv/
# Original data source: http://www.secretariadoejecutivo.gob.mx/es/SecretariadoEjecutivo/Incidencia_Delictiva
#====

require(R.utils) 
temp <- tempfile()
download.file("http://crimenmexico.diegovalle.net/en/csv/fuero-comun-municipios.csv.gz",temp)
data <- read.csv(gunzip(temp, "fuero-comun-municipios.csvuero-comun-municipios.csv", overwrite=T))
unlink(temp)


#Total number of crimes 
sum(data$count, na.rm=T) #1,157,425

#This data set only contains a selected number of crimes
#====
names(data)
unique(data[,c("crime","category","type","subtype")])
head(data[data$subtype == "SIN DATOS",])
#Check out this diagram for more info https://raw.github.com/josecarlosgonz/crime/master/images/selected_crimes.png

#Keep homicide data
#=====
data  <- subset(data, data$crime == "HOMICIDIOS")
#Add unique mun id
sum(data$count, na.rm=T) #57,314 homicides in 2011-2013
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

#Chart for number of homicides
table(data$crime)
png("images/homicidesSmooth.png")
h <- ggplot(subset(data, data$crime=="HOMICIDIOS"), aes(date, count)) + stat_smooth()
h + ggtitle("Number of homicides per municipality") +xlab("Month") + ylab("Number of homicides")  
##Looks like the total number of homicides per municipality may be decreasing too
dev.off()

#Rates for 2011, 2012 and 2013
#=====
require(reshape)
require(plyr)
names(data)

#Melt data
data  <- subset(data, is.na(data$count) ==FALSE)
head(data)
crime <- ddply(data, c("id","state_code","year"), summarize,
               total = sum(count),
               population = max(population))
head(crime); tail(crime)
sum(data$count) #Total number of homicides match 57,314
sum(crime$total) #Total number of homicides match 57,314
ddply(crime, c("year"), summarize, population = prettyNum(sum(population), big.mark =",")) # Population estimates around 107 million look ok
#Calculate crime rate per 100 k people
crime$rate  <- (crime$total / crime$population)*1000
summary(crime$rate)
qplot(crime$rate, crime$year) # who is the outlier?
head(data[data$id == 12037,]) #Looks like a ton of crimes for a population around 6k. 
crime[crime$rate >9,] #Ixcateopan de Cuauhtemoc in Guerrero. 80 homicides in 2012! An a population of 6k

#Change data format from long to wide
#======
head(crime)
#Melt
crime  <- melt(crime, id=c("id","state_code","year"), measured=c("total","rate"))

table(crime$variable)

#Reshape data from long to wide
head(crime)
crime  <- cast(crime, id + state_code ~ variable + year)
head(crime)
sum(crime$total_2011,na.rm=T) + sum(crime$total_2012,na.rm=T) + sum(crime$total_2013,na.rm=T) #Boom numbers are ok


#Format numbers
head(crime); names(crime)
cols  <- c(8:11)
crime[,cols] <- apply(crime[,cols], 2, function(x) signif(x, digits=2))
crime[is.na(crime)]  <- 0
head(crime) #Notice population estimates only appear when crime rates are available
write.csv(crime, "homicidesPerYear.csv", row.names=F)
