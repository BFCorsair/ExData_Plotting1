plot4 <- function() {

	# Requires data.table package

	# Read in the data - only keep the 3 columns we need
	df <- fread("household_power_consumption.txt",sep=';',header=TRUE, 
		na.strings=c('?'))
	# Convert the 2nd column to numeric
	df2 <- df[complete.cases(df),]
	# df2 <- df2[,c(Sub_metering_1:=as.numeric(Sub_metering_1), Sub_metering_2:=as.numeric(Sub_metering_2),
		# Sub_metering_3:=as.numeric(Sub_metering_3))]
	# df2 <- df2[,c(Sub_metering_1, Sub_metering_2, Sub_metering_3) := 
		# c(as.numeric(Sub_metering_1), as.numeric(Sub_metering_2), as.numeric(Sub_metering_3))]
	# Compute the start and end Epochs
	startEpoch <- strptime("2007-02-01-00:00:00","%Y-%m-%d-%H:%M:%S")
	# make sure we set the time to almost midnight
	endEpoch <- strptime("2007-02-02-23:59:59","%Y-%m-%d-%H:%M:%S")  
	df3 <- subset(df2, strptime(df2$Date,"%d/%m/%Y") <= endEpoch & 
		strptime(df2$Date,"%d/%m/%Y") >= startEpoch)

	# Only keep days that are Thursday and Fridays
	df4 <- transform(df3, DayOfWeek=strftime(strptime(Date, "%d/%m/%Y"),"%a"))
	df5 <- subset(df4, DayOfWeek %in% c("Thu", "Fri"))
	# dfFinal <- transform(df4, DateTime = minOfWeek(Date,Time))
	df6 <- transform(df5, DateTime = minOfWeek(Date,Time))
	dfFinal <- df6[with(df6,order(DateTime))]
	generatePlot4(dfFinal)

	# dfFinal
}

generatePlot4 <- function(df) {

	# Create the plot
	png(filename="plot4.png", width=480,height = 480)

	# Figure out where to put the labels
	x1 <- df[1,DateTime]
	x3 <- df[nrow(df),DateTime]
	x2 <- as.integer(0.5*(x1+x3))

	# Set output to be 2x2
	par(mfrow=c(2,2))

	with (df, {
		# Plot 1
		plot(DateTime, Global_active_power, type='n', xaxt ="n", xlab ="", ylab="")
		lines(DateTime, Global_active_power, type='l',col = "black")
		title(ylab="Global Active Power (kilowatts)", xlab="")
		axis(1, at=c(x1,x2,x3), labels=c("Thu","Fri", "Sat"))	

		# Plot 2
		plot(DateTime, Voltage, type='n', xaxt ="n", xlab ="", ylab="")
		lines(DateTime, Voltage, type='l',col = "black")
		title(ylab="Voltage", xlab="datetime")
		axis(1, at=c(x1,x2,x3), labels=c("Thu","Fri", "Sat"))	

		# Plot 3
		plot(DateTime, Sub_metering_1, type='n', xaxt ="n", xlab ="", ylab="")
		lines(DateTime, Sub_metering_1, type='l', col = "black")
		lines(DateTime, Sub_metering_2, type='l', col = "red")
		lines(DateTime, Sub_metering_3, type='l', col = "blue")
		title(ylab="Energy sub metering", xlab="")
		axis(1, at=c(x1,x2,x3), labels=c("Thu","Fri", "Sat"))
		legend("topright", pch="_", col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

		# Plot 4
		plot(DateTime, Global_reactive_power, type='n', xaxt ="n", xlab ="", ylab="")
		lines(DateTime, Global_reactive_power, type='l',col = "black")
		title(ylab="Global_reactive_power", xlab="datetime")
		axis(1, at=c(x1,x2,x3), labels=c("Thu","Fri", "Sat"))			
	})

	# Done - save the file
	dev.off()
}

minOfWeek <- function(date,time) {
	lt <- strptime(paste(date, time), "%d/%m/%Y %H:%M:%S")
	d <- as.integer(strftime(lt, "%w"))  # day of week (0-6) Sunday =0
	h <- as.integer(strftime(lt, "%H"))  # hour of day 0-23
	m <- as.integer(strftime(lt, "%M"))  # Minute 0-59
	moW <- m + 60 * (h + 24 * d)
	moW
}