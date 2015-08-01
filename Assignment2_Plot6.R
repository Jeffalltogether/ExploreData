## Plot 6

library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

(WD <- getwd())
if (!is.null(WD)) setwd(WD)

SCC <- readRDS("./ExploreData_Data/Source_Classification_Code.rds")
NEI <- readRDS("./ExploreData_Data/summarySCC_PM25.rds")

## 6 - Compare emissions from motor vehicle sources in Baltimore City with 
## emissions from motor vehicle sources in Los Angeles County, California 
## (fips == "06037"). Which city has seen greater changes over time in motor 
#  vehicle emissions?
NEIbaltimore <- NEI %>%
        select(fips, year, SCC , Emissions) %>%
        filter(fips == "24510") %>%
        rename(PM25 = Emissions) 

NEIla <- NEI %>%
        select(fips, year, SCC, Emissions) %>%
        filter(fips == "06037") %>%
        rename(PM25 = Emissions) 

Baltimore <- left_join(VehicleCat, NEIbaltimore, by = "SCC")
Baltimore <- na.omit(Baltimore)

LA <- left_join(VehicleCat, NEIla, by = "SCC")
LA <- na.omit(LA)

BLA <- bind_rows(Baltimore, LA)
BLA <- mutate(BLA, City = ifelse(fips == 24510, "Baltimore", "Los Angeles"))

ggplot(BLA, aes(factor(year), log(PM25))) + 
        geom_boxplot(aes(color = City)) +
        labs(title = "Motor Vehicle Emission Comparison of Two Cities",x="Year", y=expression("Vehicle Emissions log("* PM[2.5]*" )"))

dev.copy(png, file = "plot6.png", width=500, height=300)
dev.off()