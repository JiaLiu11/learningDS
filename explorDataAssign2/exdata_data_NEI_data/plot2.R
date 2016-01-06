### Common block for reading data

library(data.table)
# Read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# convert to data table for easy manipulation
dt_NEI = data.table(NEI)
dt_SCC = data.table(SCC)
rm(NEI, SCC)# clean data to save some memory
###############################################################

# plot 2: Baltimore city
# Question: Have total emissions from PM2.5 decreased in the Baltimore City
# from 1999 to 2008?

# extract data
setkey(dt_NEI, fips)
baltimoreData = dt_NEI["24510"]
baltimoreYearly_totalEmission = 
    baltimoreData[,lapply(.SD, sum), .SDcol=("Emissions"),by="year"]
# begin to plot
png("plot2.png",width=480,height=480)
plot(baltimoreYearly_totalEmission$year,baltimoreYearly_totalEmission$Emissions,
     lwd=2,col="red",
     type="o",
     xlab="Year", ylab="Total PM2.5 Emission (tons)",
)
title(main="Study Baltimore city total Emission", col.main="red",font.main=4)
dev.off()

# Conclusion:
# From the plot2, we see the total emission in this city is not a monotonic function
# of year, but it decreases at 2008 if we compare it to 1998.