plot2 <- function() {

	# Read in the data - only keep the 3 columns we need
	df <- fread("household_power_consumption.txt",sep=';',header=TRUE, na.strings=c('?'), select=c("Date","Time","Global_active_power"))
	# Convert the 2nd column to numeric
	df2 <- df[complete.cases(df),]
	df2 <- df2[,Global_active_power:=as.numeric(Global_active_power)]
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
	generatePlot2(dfFinal)
	# dfFinal
}


generatePlot2 <- function(df) {
	# Create the plot
	# Figure out where to put the labels
	x1 <- df[1,DateTime]
	x3 <- df[nrow(df),DateTime]
	x2 <- as.integer(0.5*(x1+x3))

	png(filename="plot2.png", width=480,height = 480)
	with(df, plot(DateTime, Global_active_power, type='n', xaxt ="n", xlab ="", ylab=""))
	with(df, lines(DateTime, Global_active_power, type='l',col = "black")) 
	title(ylab="Global Active Power (kilowatts)", xlab="")
	axis(1, at=c(x1,x2,x3), labels=c("Thu","Fri", "Sat"))
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