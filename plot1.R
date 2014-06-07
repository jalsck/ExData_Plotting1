plot1 <- function() {
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
    
    ## Plot the histogram
    hist(x$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", ylab="Frequency", main="Global Active Power")
    
    ## Write the plot to a PNG file
    png(file = "plot1.png")
    hist(x$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", ylab="Frequency", main="Global Active Power")
    dev.off()
    
    ## Return the data frame
    x
}
