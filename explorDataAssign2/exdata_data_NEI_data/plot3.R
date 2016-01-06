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

# plot 3: Baltimore city
# Question: Of the four types of sources indicated by the type 
# (point, nonpoint, onroad, nonroad) variable, which of these four 
# sources have seen decreases in emissions from 1999–2008 for Baltimore 
# City? Which have seen increases in emissions from 1999–2008? Use the 
# ggplot2 plotting system to make a plot answer this question.
library(ggplot2)
# extract data
setkey(dt_NEI, fips)
baltimoreData = dt_NEI["24510"]
baltimoreYearly_Emission = 
    baltimoreData[,lapply(.SD, sum), 
    .SDcol=("Emissions"),by=c("year", "type")]

# begin to plot
png("plot3.png",width=600,height=480)
ggplot(baltimoreYearly_Emission, aes(year, Emissions)) +
    geom_line(aes(color=type), size=1.5,alpha=1)+
    labs(title="Change of different type of emissions in Baltimore city")+
    labs(x="Year",y="Emission (ton)")
dev.off()

# Conclusion:
# From the plot, we have the result that the Non-road, non-point and on-road 
# source decrease, but the point source increase
###############################################################