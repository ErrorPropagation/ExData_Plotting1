plot2 <-function() {
# Exploratory Data Analysis - Course Project 1
# 05.03.15 first try
# ------------------------------------------------------------------------------------------------------------	
	library("dplyr")
	library("lubridate")
# input file
	url=".\\household_power_consumption.txt"
# output file
	pngFile="plot2.png"
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
		plot(as.POSIXlt(daytime),pcons$Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)")
		dev.off()
	}
# here wo go ---------------------------------------------------------------------------------------------------------
	pcons=readFile(url)
	pcons=subsetData(pcons)
	createGraph(pcons,pngFile)
}
