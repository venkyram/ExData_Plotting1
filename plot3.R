##------------------------------------------------------------------------------
## Exploratory Data Analysis - Assignment 1
## plot3.R
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
##  bar plot of Global Active Power to Frequency over the date range of
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

# Create a combined timestamp varibale 'ts' for additional analysis reasons
ts <- strptime(paste(hPower$Date, hPower$Time), format = "%Y-%m-%d %H:%M:%S")
hPower <- cbind(hPower, ts)

# Set one graph row / column - Done to change any default settings for this in the user env.
par(mfcol = c(1,1), mfrow = c(1,1))

# Generate the Line Plot
with(hPower, plot(Sub_metering_1 ~ ts, 
                  type = "n",
                  ylab = "Energy sub metering",
                  xlab = "")
     )

with(hPower, points(Sub_metering_1 ~ ts,
                    type = "l",
                    col = "black")
     )

with(hPower, points(Sub_metering_2 ~ ts,
                    type = "l",
                    col = "red")
     )

with(hPower, points(Sub_metering_3 ~ ts,
                    type = "l",
                    col = "blue")
     )

legend("topright", 
       lty = 1, 
       col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
       )

# Copy the chart to a png file
dev.copy(png, "plot3.png")
dev.off()