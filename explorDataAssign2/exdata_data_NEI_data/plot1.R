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

# plot 1: has total emission in US increased from 1999 to 2008?
# extract the required data
yearly_totalEmission = 
    dt_NEI[,lapply(.SD, sum), .SDcol=("Emissions"),by="year"]
# begin to plot
png("plot1.png",width=480,height=480)
plot(yearly_totalEmission$year,yearly_totalEmission$Emissions,
     lwd=2,col="red",
     type="o",
     xlab="Year", ylab="US Total PM2.5 Emission (tons)",
     )
title(main="Study US total Emission", col.main="red",font.main=4)
dev.off()

# conclusion:
# From the plot1, we clearly see the total emission decreases as time 
# pass by.



