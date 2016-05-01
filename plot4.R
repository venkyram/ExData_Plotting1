##------------------------------------------------------------------------------
## Exploratory Data Analysis - Assignment 1
## plot4.R
## Author: Venky Ramasubramani
## 2016-04-30
##
## As outlined in the assignment requirement, this script does the following
## Data Source: 
##    https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
##
##  Make sure that the above file is downloaded, unzipped and available in 
##  the current working directory for R
##
##  Our overall goal here is simply to examine how household energy usage varies
##  over a 2-day period in February, 2007. Task is to reconstruct the 
##  plots over the date range of
##  Feb 1-2, 2007
##------------------------------------------------------------------------------

# Setup the environment
rm(list = ls())
library(dplyr)

# Load the household power consumption data set

# Set the typecast for the Date column to a Date date type
setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )

# Read the entire data file
# Note the header, sep, na.strings arguments
# colClasses argument is set to respective classes to help faster loading

hPower <- read.table("household_power_consumption.txt",
                    header = TRUE,
                    sep = ";",
                    na.strings = "?",
                    colClasses = c("myDate","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

# Subset the data to the target for the assignment - Feb 1/2 of 2007
hPower <- subset(hPower, Date == "2007-02-01" | Date == "2007-02-02")

# Create a combined datetime varibale 'datetime' for additional analysis reasons
datetime <- strptime(paste(hPower$Date, hPower$Time), format = "%Y-%m-%d %H:%M:%S")
hPower <- cbind(hPower, datetime)

# Set one graph row / column - Done to change any default settings for this in the user env.
par(mfcol = c(1,1))

# Now set 2 x 2 graphs sheet
par(mfrow = c(2,2))
par(cex = 0.5)

# Graph 1
# Generate the Line Plot
with(hPower, plot(Global_active_power ~ datetime, 
                  type = "l",
                  ylab = "Global Active Power",
                  xlab = "")
)

# Graph 2
# Generate the Line Plot
with(hPower, plot(Voltage ~ datetime, 
                  type = "l",
                  ylab = "Voltage",
                  xlab = "datetime")
)

# Graph 3
# Generate the Line Plot
with(hPower, plot(Sub_metering_1 ~ datetime, 
                  type = "n",
                  ylab = "Energy sub metering",
                  xlab = "")
     )

with(hPower, points(Sub_metering_1 ~ datetime,
                    type = "l",
                    col = "black")
     )

with(hPower, points(Sub_metering_2 ~ datetime,
                    type = "l",
                    col = "red")
     )

with(hPower, points(Sub_metering_3 ~ datetime,
                    type = "l",
                    col = "blue")
     )

legend("topright", 
       lty = 1, 
       col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       cex = 0.75
       )

# Graph 4
# Generate the line plot
with(hPower, plot(Global_reactive_power ~ datetime, 
                  type = "l"))

# Copy the chart to a png file
dev.copy(png, "plot4.png")
dev.off()