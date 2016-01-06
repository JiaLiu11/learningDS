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

# plot 6: Compare emissions from motor vehicle sources in Baltimore 
# City with emissions from motor vehicle sources in Los Angeles County, California 
# (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

library(ggplot2)

# extract data: what does mobile vehicle sources means? Run the following line
unique(dt_SCC$EI.Sector[grep("On-Road", dt_SCC$EI.Sector)])
# returns:
# [1] Mobile - On-Road Gasoline Light Duty Vehicles Mobile - On-Road Gasoline Heavy Duty Vehicles
# [3] Mobile - On-Road Diesel Light Duty Vehicles   Mobile - On-Road Diesel Heavy Duty Vehicles  
# 59 Levels: Agriculture - Crops & Livestock Dust ... Waste Disposal
# so on-road type is equivalent to mobile vehicle. Therefore we just need to extract
# type=on-road data in dt_NEI

# extract two cities' data
setkey(dt_NEI, fips, type)
compareData = dt_NEI[J(c("24510","06037"), "ON-ROAD")]
compareData_grouped = 
    compareData[,lapply(.SD, sum), 
                  .SDcol=("Emissions"),by=c("year", "fips")]

# begin to plot
png("plot6.png",width=600,height=480)
ggplot(compareData_grouped, aes(year, Emissions)) +
    geom_line(aes(color=fips),size=1.5,alpha=0.7)+
    geom_point()+
    labs(title="Comparison of motor vehicles emissions in Baltimore and LA")+
    labs(x="Year",y="Emission (ton)")+
    scale_colour_discrete(name  ="City",
                          breaks=c("24510", "06037"),
                          labels=c("Baltimore", "LA"))
dev.off()

# Conclusion:
# From the plot, we see the motor vehicles emission in Baltimore decreases from 1999
# to 2008, while that in LA slightly increases.
###############################################################