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

# plot 4: 
# Question: Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008?


library(ggplot2)
# extract all coal combustion relation source
sccCode = dt_SCC$SCC[grep("coal",dt_SCC$EI.Sector, ignore.case = T)]
# the following line will check the three different kinds of coal combustion source
unique(dt_SCC$EI.Sector[grep("Coal",dt_SCC$EI.Sector)])
# subsect the data
coalSCCEmission = dt_NEI[dt_NEI$SCC %in% sccCode,]

coalSCCEmission_grouped = 
    coalSCCEmission[,lapply(.SD, sum), 
                  .SDcol=("Emissions"),by=c("year")]

# begin to plot
png("plot4.png",width=600,height=480)
ggplot(coalSCCEmission_grouped, aes(year, Emissions)) +
    geom_line(size=1.5,alpha=1)+
    labs(title="Change of coal combustion related emissions in US")+
    labs(x="Year",y="Emission (ton)")
dev.off()

# conclusion
# Based plot4, we know this pullution source decreases.
