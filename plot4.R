plot4 <- function() {
    ## Read in the data file
    x <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", stringsAsFactors = FALSE)
    
    ## Create a datetime column
    x$DateTime <- as.POSIXct(paste(x$Date, x$Time), format="%d/%m/%Y %H:%M:%S")
    
    ## Coerce the date column to date format
    x$Date <- as.Date(x$Date,format='%d/%m/%Y')
    
    ## Subset x to obtain the data for the required dates
    x <- x[x$Date %in% as.Date(c('2007-02-01', '2007-02-02')),]
    
    ## Replace ? with NA for remaining columns
    x$Global_active_power[x$Global_active_power == '?'] <- NA
    x$Global_active_power <- as.numeric(x$Global_active_power)
    
    x$Global_reactive_power[x$Global_reactive_power == '?'] <- NA
    x$Global_reactive_power <- as.numeric(x$Global_reactive_power)
    
    x$Voltage[x$Voltage == '?'] <- NA
    x$Voltage <- as.numeric(x$Voltage)
    
    x$Global_intensity[x$Global_intensity == '?'] <- NA
    x$Global_intensity <- as.numeric(x$Global_intensity)
    
    x$Sub_metering_1[x$Sub_metering_1 == '?'] <- NA
    x$Sub_metering_1 <- as.numeric(x$Sub_metering_1)
    
    x$Sub_metering_2[x$Sub_metering_2 == '?'] <- NA
    x$Sub_metering_2 <- as.numeric(x$Sub_metering_2)
    
    x$Sub_metering_3[x$Sub_metering_3 == '?'] <- NA
    x$Sub_metering_3 <- as.numeric(x$Sub_metering_3)
    
    ## Configure the number of graphs to display
    old.par <- par(mfrow=c(2, 2))
    
    ## Add the first plot
    plot(x$DateTime, x$Global_active_power, type="l", xlab="", ylab="Global Active Power", main="")

    ## Add the second plot
    plot(x$DateTime, x$Voltage, type="l", xlab="datetime", ylab="Voltage", main="")
    
    ## Add the third plot
    ## Get the max range
    yrange<-range(c(x$Sub_metering_1, x$Sub_metering_2, x$Sub_metering_3))
    
    ## Plot the graph
    plot(x$DateTime, x$Sub_metering_1, type="l", ylim=yrange, xlab="", ylab="Energy sub metering", main="")
    par(new=T)
    plot(x$DateTime, x$Sub_metering_2, type="l", col="red", ylim=yrange, xlab="", ylab="", main="")
    par(new=T)
    plot(x$DateTime, x$Sub_metering_3, type="l", col="blue", ylim=yrange, xlab="", ylab="", main="")
    par(new=F)
    
    ## Add the legend
    legend(x="topright", c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), lty=c(1, 1, 1), lwd=c(2.5, 2.5, 2.5), col=c("black", "red","blue"), bty = "n")
    
    ## Add the fourth plot
    plot(x$DateTime, x$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power", main="")
    
    ## Reset the plotting options
    par(old.par)
    
    ## Write the plot to a PNG file
    png(file = "plot4.png")
    
    ## Configure the number of graphs to display
    old.par <- par(mfrow=c(2, 2))
    
    ## Add the first plot
    plot(x$DateTime, x$Global_active_power, type="l", xlab="", ylab="Global Active Power", main="")
    
    ## Add the second plot
    plot(x$DateTime, x$Voltage, type="l", xlab="datetime", ylab="Voltage", main="")
    
    ## Add the third plot
    ## Get the max range
    yrange<-range(c(x$Sub_metering_1, x$Sub_metering_2, x$Sub_metering_3))
    
    ## Plot the graph
    plot(x$DateTime, x$Sub_metering_1, type="l", ylim=yrange, xlab="", ylab="Energy sub metering", main="")
    par(new=T)
    plot(x$DateTime, x$Sub_metering_2, type="l", col="red", ylim=yrange, xlab="", ylab="", main="")
    par(new=T)
    plot(x$DateTime, x$Sub_metering_3, type="l", col="blue", ylim=yrange, xlab="", ylab="", main="")
    par(new=F)
    
    ## Add the legend
    legend(x="topright", c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), lty=c(1, 1, 1), lwd=c(2.5, 2.5, 2.5), col=c("black", "red","blue"), bty = "n")
    
    ## Add the fourth plot
    plot(x$DateTime, x$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power", main="")
    
    ## Reset the plotting options
    par(old.par)
    
    dev.off()
    
    ## Return the data frame
    x
}
