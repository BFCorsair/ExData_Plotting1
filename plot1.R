plot1 <- function() {

	# Read in the data - only keep the 2 columns we need
	df <- fread("household_power_consumption.txt",sep=';',header=TRUE,na.strings=c('?'), select=c("Date","Global_active_power"))
	# Convert the 2nd column to numeric
	df<-df[,Global_active_power:=as.numeric(Global_active_power)]
	# Eliminate the NAs
	df2 <- df[complete.cases(df),]
	# Compute the start and end Epochs
	startEpoch <- strptime("2007-02-01-00:00:00","%Y-%m-%d-%H:%M:%S")
	# make sure we set the time to almost midnight
	endEpoch <- strptime("2007-02-02-23:59:59","%Y-%m-%d-%H:%M:%S")  
	dfFinal <- subset(df2, strptime(df2$Date,"%d/%m/%Y") <= endEpoch & 
		strptime(df2$Date,"%d/%m/%Y") >= startEpoch)

	# Create the plot
	png(filename="plot1.png", width=480,height = 480)
	hist(dfFinal$Global_active_power,col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)")
	dev.off()
}