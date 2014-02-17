#####################
#======
#Author Jose Gonzalez
#Website: www.jose-gonzalez.org

#Crime analysis for Mexican municipalities

#Download data from Diego Valle's blog 
# Original data source: http://www.secretariadoejecutivo.gob.mx/es/SecretariadoEjecutivo/Incidencia_Delictiva
#====
library(R.utils) 
temp <- tempfile()
download.file("http://crimenmexico.diegovalle.net/en/csv/fuero-comun-municipios.csv.gz",temp)
data <- read.csv(gunzip(temp, "fuero-comun-municipios.csv"))
unlink(temp)


#Total number of crimes 
sum(data$count, na.rm=T)

#This data set only contains a selected number of crimes
#Ignore incomplete cases
data  <- subset(data, is.na(data$count)== FALSE)

#Add unique mun id
data$mun_code  <- sprintf("%03d", data$mun_code)
id  <- paste(data$state_code,data$mun_code,sep="")
head(id); tail(id)
data$id  <- id

#stationality data


str(data$month)
data$date  <- paste(sprintf("%02d",data$month),data$year,sep="-")
data$date  <- paste("01",data$date,sep="-")
table(data$date)
data$date  <- as.Date(data$date, "%d-%m-%Y")
qplot(data$date, data$count)
c <- ggplot(data, aes(date, count))
c +
d  <-  ggplot(data, aes(x=date, y=count)) + geom_line()



#Crime rate for 2011-2013
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

