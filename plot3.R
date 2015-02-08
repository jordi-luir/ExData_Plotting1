## ---------------------------------------------------------------
## PLOT 3
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

## CREATE PLOT 3    
##    Plots the third graph
##    Parameters 
##        values: Table with values read of the datafile
createPlot3 <- function(values){
    par(mfrow = c(1, 1))
    with( values, 
          plot(x = values$datetime, y = Sub_metering_1, type = "l",
          col = "black", xlab = "", ylab = "Energy sub metering") )
    with( values, 
          lines(x = values$datetime, y = Sub_metering_2, col = "red") )
    with( values, 
          lines(x = values$datetime, y = Sub_metering_3, col = "blue") )
    legend( "topright", pch=c("_","_","_"), col=c("black", "red", "blue"),
            legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3") )
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
## Plots the "Plot 2"
createPlot3( dataHPC )
## Saves PNG to a File
savePng("plot3.png")