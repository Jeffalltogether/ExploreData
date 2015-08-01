## Plot 4

library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

(WD <- getwd())
if (!is.null(WD)) setwd(WD)

SCC <- readRDS("./ExploreData_Data/Source_Classification_Code.rds")
NEI <- readRDS("./ExploreData_Data/summarySCC_PM25.rds")

## 4 - Across the United States, how have emissions from coal combustion-related
## sources changed from 1999-2008?
CoalCat <- SCC %>%
        select(SCC, Short.Name) %>%
        filter(grepl('Coal', Short.Name )) %>%
        select(SCC)

NEIsub <- NEI %>%
        select(year, SCC , Emissions) %>%
        rename(PM25 = Emissions)

CoalMerged <- left_join(CoalCat, NEIsub, by = "SCC") #warnings; slow execution time
CoalMerged <- na.omit(CoalMerged)

DF4 <- CoalMerged %>%
        group_by(year) %>%        
        summarize(PM25 = mean(PM25))

ggplot(DF4, aes(year, PM25)) + geom_point(size = 4, alpha = 0.5) + 
        geom_smooth(method = "lm") + labs(title = "Emissions From Coal Combustion-related Sources in U.S.", x="Year", y=expression("Average Coal Emissions "* PM[2.5])) +
        scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))

dev.copy(png, file = "plot4.png", width=500, height=400)
dev.off()
