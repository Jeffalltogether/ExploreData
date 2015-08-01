## Plot 5

library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

(WD <- getwd())
if (!is.null(WD)) setwd(WD)

SCC <- readRDS("./ExploreData_Data/Source_Classification_Code.rds")
NEI <- readRDS("./ExploreData_Data/summarySCC_PM25.rds")

## 5 - How have emissions from motor vehicle sources changed from 1999-2008 in 
## Baltimore City?
VehicleCat <- SCC %>%
        select(SCC, Short.Name) %>%
        filter(grepl('Motor|Vehicle', Short.Name )) %>%
        select(SCC)

NEIsub <- NEI %>%
        select(fips, year, SCC , Emissions) %>%
        rename(PM25 = Emissions) %>%
        filter(fips == "24510")

VehicleMerged <- left_join(VehicleCat, NEIsub, by = "SCC")
VehicleMerged <- na.omit(VehicleMerged)

ggplot(VehicleMerged, aes(year, PM25)) + 
        geom_point(size = 4, alpha = 0.5) + 
        geom_smooth(method = "lm") +
        labs(title = "Motor Vehicle Emissions in Baltimore, MD",x="Year", y=expression("Vehicle Emissions ("* PM[2.5]*" )")) +
        scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))

dev.copy(png, file = "plot5.png", width=500, height=300)
dev.off()
