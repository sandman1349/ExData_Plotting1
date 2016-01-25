## load data into R 
columnNames <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
energydata <- read.table("/Users/Sandeep/Documents/household_power_consumption.txt", header=F, sep=";", skip=1,  col.names=columnNames, na.strings="?")
head(energydata)

## filter data into correct date standards 
energydata$Date <- as.Date(as.character(energydata$Date), format="%d/%m/%Y")
filterenergydata <- energydata[energydata$Date=="2007-02-01" | energydata$Date=="2007-02-02",]

## create .png file of the histogram
png("plot1.png",width=480,height=480)
hist(filterenergydata$Global_active_power,col= "red", main="Global Active Power", xlab="Global Active Power (kilowatts)",ylab="Frequency")
dev.off()   ## Close graphics file