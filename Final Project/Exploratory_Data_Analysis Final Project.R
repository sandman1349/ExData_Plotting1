NEI <- readRDS("/Users/Sandeep/Documents/summarySCC_PM25.rds")
SCC <- readRDS("/Users/Sandeep/Documents/Source_Classification_Code.rds")

## Question 1
library(dplyr)
years <- group_by(NEI, year)
yeargroup <- summarize(years, sum(Emissions, na.rm=TRUE))
colnames(yeargroup) <- c("Year","Emissions")
head(yeargroup)
plot(yeargroup$Year,yeargroup$Emissions, type="l", col="blue",xlab = "Year", ylab = "Emissions",main="Emissions by Year")
dev.off()   
## Yes, looking at the code, total emissions from PM2.5 have decreased from 1999 to 2008


## Question 2: Have total emissions from PM2.5 decreased in the  Baltimore City, Maryland ( 𝚏𝚒𝚙𝚜 == 𝟸𝟺𝟻𝟷𝟶) from 1999 to 2008? Use the base plotting system to make a plot answering this question.
balt <- NEI[NEI$fips=="24510",]
byears <- group_by(balt, year)
byeargroup <- summarize(byears, sum(Emissions, na.rm=TRUE))
colnames(byeargroup) <- c("Year","Emissions")
head(byeargroup)
png("plot2.png",width=480,height=480)
plot(byeargroup$Year,byeargroup$Emissions,type="l",, col="blue", xlab = "Year", ylab = "Emissions",main="Baltimore Emissions by Year")
dev.off() 

#Yes,emissions in Baltimore have decreased over time (overall) despite an increase betwen 2002 - 2005

## Question 3: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
sources <- group_by(balt, year, type)
typeyeargroup <- summarize(sources, sum(Emissions, na.rm=TRUE))
colnames(typeyeargroup) <- c("Year","Type", "Emissions")
library(ggplot2)
png("plot3.png",width=480,height=480)
g <- qplot(Year, Emissions,data=typeyeargroup, size=5, colour = Type, xlab="Year",ylab="Emissions",main="Baltimore Emissions over Time by Type")
g <- g + theme(axis.title=element_text(face="bold.italic", 
                                     size="12"), legend.position="bottom")
g
dev.off()

## ANSWER: Emissions in Baltimore have decreased over time, except for the "Point" type which increased between 2002 and 2005.

## Question 4: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

NEISCC <- merge(NEI,SCC,by.x = "SCC",by.y="SCC") 

coal <- NEISCC[grep("Coal",NEISCC$Short.Name),]
coalyears <- group_by(coal, year)
coalgroup <- summarize(years, sum(Emissions, na.rm=TRUE))
colnames(coalgroup) <- c("Year","Emissions")
png("plot4.png",width=480,height=480)
plot(coalgroup$Year,coalgroup$Emissions,type="l",xlab = "Year", ylab = "Emissions",main="Coal Emissions by Year")
dev.off()

## ANSWER: Coal emissions have decreased over time, from 1999 to 2008

## Question 5:How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

NEISCC <- merge(NEI,SCC,by.x = "SCC",by.y="SCC") 
dist <- distinct(SCC, EI.Sector)
balt <- NEISCC[NEISCC$fips=="24510",]
mobile <- NEISCC[grep("Vehicle",NEISCC$SCC.Level.Two),]
mobileyears <- group_by(mobile, year)
mobilegroup <- summarize(mobileyears, sum(Emissions, na.rm=TRUE))
colnames(mobilegroup) <- c("Year","Emissions")
png("plot5.png",width=480,height=480)
plot(mobilegroup$Year,mobilegroup$Emissions,type="l",xlab = "Year", ylab = "Emissions",main="Motor Vehicle Emissions Over Time")
dev.off()

## ANSWER:Motor vehicle emissions have decreased over time, from 1999 to 2008


## Question 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == 𝟶𝟼𝟶𝟹𝟽). Which city has seen greater changes over time in motor vehicle emissions?
NEISCC <- merge(NEI,SCC,by.x = "SCC",by.y="SCC") 
ques6city <- NEISCC[NEISCC$fips %in% c("24510", "06037"),]
ques6 <- ques6city[grep("Vehicle",ques6city$SCC.Level.Two),]
head(ques6)
library(dplyr)
citygroup <- group_by(ques6, fips, year)
cityyear <- summarize(citygroup, sum(Emissions, na.rm=TRUE))
colnames(cityyear) <- c("City","Year","Emissions")
cityyear$City <-  ifelse(cityyear$City== "24510", "Baltimore","LA")
LAyearly <- cityyear[cityyear$City=="LA",]
Balyearly <- cityyear[cityyear$City=="Baltimore",]
head(Balyearly)

png("plot6.png",width=480,height=480)
par(mfrow=c(2,1))
plot(LAyearly$Year,LAyearly$Emissions,type="l",xlab = "Year", ylab = "Emissions",main="LA Motor Vehicle Emissions Over Time")
plot(Balyearly$Year,Balyearly$Emissions,type="l",xlab = "Year", ylab = "Emissions",main="Baltimore Motor Vehicle Emissions Over Time")
dev.off()

## ANSWER: LA has seen an increase in motor vehicle emissions while Baltimore has decreased.
