plot3 <-function() {
# Exploratory Data Analysis - Course Project 1
# 05.03.15 first try
# ------------------------------------------------------------------------------------------------------------	
	library("dplyr")
	library("lubridate")
# input file
	url=".\\household_power_consumption.txt"
# output file
	pngFile="plot3.png"
# reads input file
	readFile<- function(url) {
		if(!file.exists(url)) stop(paste("can't read ",url))
		pocons=read.table(url,header=TRUE,sep=";")
		return(pcons)
	}
# converting Date into a number
	dateConvert<-function(date){
		return(gsub("-","",dmy(date)%>%as.character())%>%as.numeric())
	}
# only data between 2007-02-01 and 2007-02-01 are used
	subsetData<-function(data){
		pcons=filter(data,20070201 <= dateConvert(Date) &  dateConvert(Date)<=20070202)
		return(pcons)
	}
# create graph and write as png file	
	createGraph<-function(data,pngFile){
		daytime=paste(dmy(pcons$Date),pcons$Time)
		png(pngFile)
		plot(as.POSIXlt(daytime),pcons$Sub_metering_1,type="n",xlab="",ylab="Energy sub metering")
		lines(as.POSIXlt(daytime),pcons$Sub_metering_1)
		lines(as.POSIXlt(daytime),pcons$Sub_metering_2,col="red")
		lines(as.POSIXlt(daytime),pcons$Sub_metering_3,col="blue")
		legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"),text.col = "black", lty = c(1, 1, 1),lwd=c(2,2,2),merge = TRUE)
		dev.off()
	}
# here wo go ---------------------------------------------------------------------------------------------------------
	pcons=readFile(url)
	pcons=subsetData(pcons)
	createGraph(pcons,pngFile)
}
