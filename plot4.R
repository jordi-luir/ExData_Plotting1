## ---------------------------------------------------------------
## PLOT 4
##    1.- Loads datafile in dataHPC
##    2.- Plots the Histogram as required
##    3.- Saves Plot to a PNG file
## ---------------------------------------------------------------

## LOAD DATA
##    Reads the data from File, and converts Dates and Times
##    Datetime: New Column calculated 
loadData <- function(filename) {
    ## Setting up Path for Working Directory
    # setwd("~/Coursera/EDA/data")
    
    # Read File and Load Data
    message("Reading File")
    dataTMP <- read.csv2(filename, dec=".", na.strings="?")
    message("Filtering Data")
    idxOK <- dataTMP[,"Date"][]=="1/2/2007" | dataTMP[,"Date"][]=="2/2/2007"
    dataTMP2 <- dataTMP[idxOK,]
    message("Converting Times")
    strDateTimes <- paste(dataTMP2$Date, dataTMP2$Time)
    dataTMP2$datetime <- as.POSIXct(strptime(strDateTimes, "%d/%m/%Y %H:%M:%S"))
    message("Converting Dates")
    dataTMP2[,1] <- as.Date(dataTMP2[,1], format="%d/%m/%Y")
    
    message("Data Loaded")
    dataTMP2
}

## CREATE PLOT 4_1:  Plots the second graph
createPlot4_1 <- function(values){
    plot(x = values$datetime, 
         y = values$Global_active_power,
         type = "l",
         col = "black",
         xlab = "",
         ylab = "Global Active Power")
}

## CREATE PLOT 4_2:  Plots the second graph
createPlot4_2 <- function(values){
    plot(x = values$datetime, 
         y = values$Voltage,
         type = "l",
         col = "black",
         xlab = "Datetime",
         ylab = "Voltage")
}


## CREATE PLOT 4_3    
createPlot4_3 <- function(values){
    with( values, 
          plot(x = values$datetime, y = Sub_metering_1, type = "l",
               col = "black", xlab = "", ylab = "Energy sub metering") )
    with( values, 
          lines(x = values$datetime, y = Sub_metering_2, col = "red") )
    with( values, 
          lines(x = values$datetime, y = Sub_metering_3, col = "blue") )
    legend( "topright", pch=c("_","_","_"), col=c("black", "red", "blue"),
            legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
            bty="n" )
}

## CREATE PLOT 4_4:  Plots the second graph
createPlot4_4 <- function(values){
    plot(x = values$datetime, 
         y = values$Global_reactive_power,
         type = "l",
         col = "black",
         xlab = "Datetime",
         ylab = "Global Reactive Power")
}


## CREATE PLOT 4    
##    Plots the 4 graphs
createMerge4Plots <- function(values){
    par( mfrow = c(2, 2) )
    createPlot4_1( values )
    createPlot4_2( values )
    createPlot4_3( values )
    createPlot4_4( values )
}


## SAVE PNG
##    Saves the Plot in a PNG file
savePng <- function(filePng){
    ## Copy my plot to a PNG file of 480x480 pixels
    dev.copy(png, file = filePng, width = 480, height = 480)   
    ## Don't forget to close the PNG device!
    dev.off()                      
}

## Load Data File
dataHPC <- loadData("household_power_consumption.txt")
## Plots the "Plot 4"
createMerge4Plots( dataHPC )
## Saves PNG to a File
savePng("plot4.png")
