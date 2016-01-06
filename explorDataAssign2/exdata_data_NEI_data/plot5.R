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

# plot 5:How have emissions from motor vehicle sources changed 
#from 1999–2008 in Baltimore City?
library(ggplot2)

# extract data: what does mobile vehicle sources means? Run the following line
unique(dt_SCC$EI.Sector[grep("On-Road", dt_SCC$EI.Sector)])
# returns:
# [1] Mobile - On-Road Gasoline Light Duty Vehicles Mobile - On-Road Gasoline Heavy Duty Vehicles
# [3] Mobile - On-Road Diesel Light Duty Vehicles   Mobile - On-Road Diesel Heavy Duty Vehicles  
# 59 Levels: Agriculture - Crops & Livestock Dust ... Waste Disposal
# so on-road type is equivalent to mobile vehicle. Therefore we just need to extract
# type=on-road data in dt_NEI
setkey(dt_NEI, fips, type)
baltimoreData = dt_NEI[J("24510", "ON-ROAD")] # select city and pollution type
baltimoreYearly_Emission = 
    baltimoreData[,lapply(.SD, sum), 
                  .SDcol=("Emissions"),by=c("year")] # group and sum data

# begin to plot
png("plot5.png",width=600,height=480)
ggplot(baltimoreYearly_Emission, aes(year, Emissions)) +
    geom_line(size=1.5,alpha=0.7)+
    geom_point(size=4,alpha=1)+
    labs(title="Change of motor vehicles emissions in Baltimore city")+
    labs(x="Year",y="Emission (ton)")
dev.off()

# Conclusion:
# From the plot, we have the result that the motor vehicle sources decreases
# from 1999 to 2008
###############################################################