## Plot 2

library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

(WD <- getwd())
if (!is.null(WD)) setwd(WD)

SCC <- readRDS("./ExploreData_Data/Source_Classification_Code.rds")
NEI <- readRDS("./ExploreData_Data/summarySCC_PM25.rds")

## 2 - Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
## (fips == "24510") from 1999 to 2008? Use the base plotting system to make a 
## plot answering this question.
DF2 <- NEI %>%
        select(fips, year, Emissions) %>%
        filter(fips == "24510") %>%
        group_by(year) %>%
        summarize(PM25 = mean(Emissions))

model <- lm(PM25 ~ year, DF2)

with(DF2, plot(year, PM25, pch = 15, main = "Average PM25 From All Emission Types \nin Baltimore, MD"))
abline(model, lwd = 1, col = "red")
legend(x=2005, y=10, pch = 15,bty = "n", col = c("black", "red"),
       legend = c("Mean PM25", "Regression Line"))

dev.copy(png, file = "plot2.png", width=600, height=450)
dev.off()
