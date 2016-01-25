## load data into R 
columnNames <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
energydata <- read.table("/Users/Sandeep/Documents/household_power_consumption.txt", header=F, sep=";", skip=1,  col.names=columnNames, na.strings="?")
head(energydata)

## filter data into correct date standards 
energydata$Date <- as.Date(as.character(energydata$Date), format="%d/%m/%Y")
filterenergydata <- energydata[energydata$Date=="2007-02-01" | energydata$Date=="2007-02-02",]

## add date time elements
filterenergydata$date_time <- strptime(paste(filterenergydata$Date, filterenergydata$Time), "%Y-%m-%d %H:%M:%S", tz = "EST")

## create plot3.png file of the time chart
png("plot3.png",width=480,height=480)
plot(filterenergydata$date_time, filterenergydata$Sub_metering_1, type='l',
     xlab='', ylab='Energy sub metering')
points(filterenergydata$date_time, filterenergydata$Sub_metering_2,type='l',col="Red")
points(filterenergydata$date_time, filterenergydata$Sub_metering_3,type='l',col="Blue")
legend("topright", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       pch=1,lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("black","red","blue"))
dev.off()   ## Close graphics file



      
      
      