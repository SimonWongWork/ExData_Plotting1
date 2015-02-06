plot4 = function()
{
    # Go to Working directory of R studio.
    # load the unzipped dataset file into variable data.
    library(data.table)
    readData <- fread("household_power_consumption.txt")

    
    # Clean data,  make the Data and time variables to be characters.
    class(readData$Date)
    class(readData$Time)


    # Change the format of Date variable using as.Date as suggested in assignment.
    readData$Date <- as.Date(readData$Date, format = "%d/%m/%Y")
    class(readData$Date)

    
    # Subset the data for the two dates of interest.
    subsetData <- readData[readData$Date=="2007-02-01" | readData$Date=="2007-02-02"]

    
    # Convert data subset to a data frame.
    class(subsetData)
    subsetData <- data.frame(subsetData)

    
    # Convert values of columns (from "Global_active_power" to "Sub_metering") to numeric.
    for(i in c(3:9)) 
    {
        subsetData[,i] <- as.numeric(as.character(subsetData[,i])) 
    }


    # Create Date_Time variable.
    subsetData$Date_Time <- paste(subsetData$Date, subsetData$Time)

    
    # Convert Date_Time variable to proper format.
    subsetData$Date_Time <- strptime(subsetData$Date_Time, format="%Y-%m-%d %H:%M:%S")
    class(subsetData$Date_Time)

    
    # Start plot the result.
    png(filename = "plot4.png", width = 480, height = 480, units = "px", bg = "white")
    par(mfrow = c(2, 2), mar = c(14, 6, 2, 2), cex = .5)

    
    
    # Parameters setting:
    # type = "n" : develop the plot without points.
    # xlab = ""  : takes away the x axis labels.
    # xaxt = NULL: suppresses x axis.
    # Top left graph.
    plot(subsetData$Date_Time, subsetData$Global_active_power, xaxt = NULL, xlab = "", ylab = "Global Active Power", type = "n")
    lines(subsetData$Date_Time, subsetData$Global_active_power, type = "S")

    
    # Top right graph
    plot(subsetData$Date_Time, subsetData$Voltage, xaxt = NULL, xlab = "datetime", ylab = "Voltage", type = "n")
    lines(subsetData$Date_Time, subsetData$Voltage, type = "S")


    # Bottom left graph.
    # Legends:
    # lwd = c(1, 1, 1)  : set the lines widths of 1.
    # lty = c(1, 1)     : set the line type within the legend.    
    # bty = "n"         : sets the box type to none.
    plot(subsetData$Date_Time, subsetData$Sub_metering_1, xaxt = NULL, xlab = "", ylab = "Energy sub metering", type = "n")
    lines(subsetData$Date_Time, subsetData$Sub_metering_1, col = "black", type = "S")
    lines(subsetData$Date_Time, subsetData$Sub_metering_2, col = "red", type = "S")
    lines(subsetData$Date_Time, subsetData$Sub_metering_3, col = "blue", type = "S")
    legend("topright", bty = "n", lty = c(1, 1), lwd = c(1, 1, 1), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))


    # Bottom right graph.
    plot(subsetData$Date_Time, subsetData$Global_reactive_power, xaxt = NULL, xlab = "datetime", ylab = "Global_reactive_power", type = "n")
    lines(subsetData$Date_Time, subsetData$Global_reactive_power, type = "S")
    dev.off()
    
}

