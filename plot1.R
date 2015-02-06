plot1 = function()
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
    png(filename = "plot1.png", width = 480, height = 480, units = "px", bg = "white")
    

    # Set the grapical quality with the form c(bottom, left, top, right) which gives the number of lines 
    # of margin to be specified on the four sides of the plot
    par(mar = c(7, 7, 5, 2))
    hist(subsetData$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power(kilowatts)")
    dev.off()

}


