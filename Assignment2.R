## Assignment 2
library(ggplot2)
library(dplyr)
library(tidyr)

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

with(DF1, plot(year, PM25, main = "total PM2.5 from all sources", type = "n"))
with(subset(DF1, type == "NON-ROAD"), points(year, PM25), col = "black")
with(subset(DF1, type == "NONPOINT"), points(year, PM25, col = "blue"))
with(subset(DF1, type == "ON-ROAD"), points(year, PM25, col = "red"))
with(subset(DF1, type == "POINT"), points(year, PM25, col = "green"))
abline(model, lwd=2)
legend("topright", pch = 1, col = c("black", "blue", "red", "green"),
       legend = c("NON-ROAD", "NONPOINT", "ON-ROAD", "POINT"))


dev.copy(png, file = "plot1.png", width=500, height=350)
dev.off()

## 2 - Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
## (fips == "24510") from 1999 to 2008? Use the base plotting system to make a 
## plot answering this question.
DF2 <- NEI %>%
        select(fips, year, Emissions) %>%
        filter(fips == "24510") %>%
        group_by(year) %>%
        summarize(PM25 = mean(Emissions))

model <- lm(PM25 ~ year, DF2)

with(DF2, plot(year, PM25, main = "Average PM25 From All Emission Types \nin Baltimore City, Maryland"))
abline(model, lwd = 1, col = "red")
legend("topright", pch = 1, col = c("black", "red"),
       legend = c("Mean PM25", "Regression Line"))

dev.copy(png, file = "plot2.png", width=600, height=450)
dev.off()

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

qplot(year, PM25, facets = . ~ type, data=DF3, geom = c("point", "smooth"), method = "lm")

dev.copy(png, file = "plot3.png", width=700, height=300)
dev.off()

## 4 - Across the United States, how have emissions from coal combustion-related
## sources changed from 1999-2008?
CoalCat <- SCC %>%
        select(SCC, Short.Name) %>%
        filter(grepl('Coal', Short.Name )) %>%
        select(SCC)

NEIsub <- NEI %>%
        select(year, SCC , Emissions) %>%
        rename(PM25 = Emissions)

CoalMerged <- left_join(CoalCat, NEIsub, by = "SCC") %>% #warnings; slow execution time

CoalMerged <- na.omit(CoalMerged)

DF4 <- CoalMerged %>%
        group_by(year) %>%        
        summarize(PM25 = mean(PM25))
        


ggplot(CoalMerged, aes(year, PM25)) + geom_point(size = 4, alpha = 0.5) + geom_smooth(method = "lm")
        
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

VehicleMerged <- left_join(VehicleCat, NEIsub, by = "SCC") #warnings; slow execution time
VehicleMerged <- na.omit(VehicleMerged)

ggplot(VehicleMerged, aes(year, PM25)) + 
        geom_point(size = 4, alpha = 0.5) + 
        geom_smooth(method = "lm") +
        labs(title = "Motor Vehicle Emissions in Baltimore, MD")

dev.copy(png, file = "plot5.png", width=500, height=300)
dev.off()

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

Baltimore <- left_join(VehicleCat, NEIbaltimore, by = "SCC") #warnings; slow execution time
Baltimore <- na.omit(Baltimore)

LA <- left_join(VehicleCat, NEIla, by = "SCC") #warnings; slow execution time
LA <- na.omit(LA)

BLA <- bind_rows(Baltimore, LA)
na.omit(BLA)

BLA2 <- BLA %>%
        group_by(year,fips) %>%
        summarize(avePM25 = mean(PM25))

ggplot(BLA, aes(factor(year), log(PM25))) + 
        geom_boxplot(aes(color = fips))

dev.copy(png, file = "plot6.png", width=500, height=300)
dev.off()