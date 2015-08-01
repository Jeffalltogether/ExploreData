## Plot 3

library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

(WD <- getwd())
if (!is.null(WD)) setwd(WD)

SCC <- readRDS("./ExploreData_Data/Source_Classification_Code.rds")
NEI <- readRDS("./ExploreData_Data/summarySCC_PM25.rds")

## 3- Of the four types of sources indicated by the type (point, nonpoint, 
## onroad, nonroad) variable, which of these four sources have seen decreases in
## emissions from 1999-2008 for Baltimore City? Which have seen increases in 
## emissions from 1999-2008? Use the ggplot2 plotting system to make a plot 
## answer this question.
DF3 <- NEI %>%
        select(fips, year, type, Emissions) %>%
        filter(fips == "24510") %>%
        group_by(type, year) %>%
        summarize(PM25 = mean(Emissions))

qplot(year, PM25, facets = . ~ type, data=DF3, 
      main = "Emissions by Source Type in Baltimore, MD", 
      xlab = "Year", ylab = expression("Emissions ("* PM[2.5]*" )"),
      geom = c("point", "smooth"), method = "lm") +
        scale_x_continuous(breaks=c(1999, 2002, 2005, 2008)) +
        theme(panel.margin = unit(1, "lines"))

dev.copy(png, file = "plot3.png", width=700, height=300)
dev.off()
