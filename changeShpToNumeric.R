require(foreign)
list.files()
temp <- read.dbf("shapefiles/Mex_Pov_mun.dbf", as.is=T)
names(temp)
str(temp)


require(plyr)
list.files()

data  <- temp
str(data)
length(names(data))
cols  <- c(5:21)
data[,cols] <- apply(data[,cols], 2, function(x) gsub(",","",x))
data[,cols] <- apply(data[,cols], 2, function(x) as.numeric(x))
#Round up numbers
data[,cols] <- apply(data[,cols], 2, function(x) signif(x, digits=2))
str(data)
write.dbf(data,"shapefiles/Mex_Pov_mun.dbf")
#Check if crimes have increased or decreased


#Calculate breaks for each rate

qplot(data$rate_2013) # 0 -2, 2-4, 4 -6
qplot(data$rate_2012) # 0 -2, 2-4, 4 -6
qplot(data$rate_2011) # 0 -2, 2-4, 4 -6 ,8-10, 10-12