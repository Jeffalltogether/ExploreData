## Plot 1

library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

(WD <- getwd())
if (!is.null(WD)) setwd(WD)

SCC <- readRDS("./ExploreData_Data/Source_Classification_Code.rds")
NEI <- readRDS("./ExploreData_Data/summarySCC_PM25.rds")

## 1 - Have total emissions from PM2.5 decreased in the United States from 1999 
## to 2008? Using the base plotting system, make a plot showing the total PM2.5 
## emission from all sources for each of the years 1999, 2002, 2005, and 2008.
DF1 <- NEI %>%
        select(year, type, Emissions) %>%
        group_by(type, year) %>%
        summarize(PM25 = mean(Emissions))

model <- lm(PM25 ~ year, DF1)

with(DF1, plot(year, PM25, main = "Total PM2.5 From All Sources", type = "n"))
with(subset(DF1, type == "NON-ROAD"), points(year, PM25), col = "black", pch = 15)
with(subset(DF1, type == "NONPOINT"), points(year, PM25, col = "blue", pch = 16))
with(subset(DF1, type == "ON-ROAD"), points(year, PM25, col = "red", pch = 17))
with(subset(DF1, type == "POINT"), points(year, PM25, col = "green",pch = 18))
abline(model, lwd=2)
legend(x = 2006, y = 55, pch = c(15,16,17,18), col = c("black", "blue", "red", "green"),
       legend = c("NON-ROAD", "NONPOINT", "ON-ROAD", "POINT"), cex = .75, bty= "n")


dev.copy(png, file = "plot1.png", width=500, height=350)
dev.off()
