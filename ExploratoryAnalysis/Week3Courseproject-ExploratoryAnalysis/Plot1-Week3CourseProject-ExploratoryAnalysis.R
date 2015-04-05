"
  Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == 24510) from 1999 to 2008? Use the base plotting system to make a plot answering this question.

Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.

Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == 06037). Which city has seen greater changes over time in motor vehicle emissions?
"

setwd("C:\\Users\\rkmalaiya\\Documents\\GitHub\\DataMiningSpecialization\\ExploratoryAnalysis\\Week3Courseproject-ExploratoryAnalysis")

nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

str(nei)
str(scc)



## Answer 1:
library(plyr)
nei_summ <- ddply(nei, ~year, summarise, sumOfE = sum(Emissions))
with(nei_summ, plot(sumOfE ~ year, type = "l", ylab = "Sum of Emissions"))

## Answer 2:
library(plyr)
nei_summ_fips <- ddply(nei, ~year ~fips, summarise, sumOfE = sum(Emissions))
nei_summ_fips <-  nei_summ_fips[nei_summ_fips$fips == 24510, ]

with(nei_summ_fips, plot(sumOfE ~ year, type = "l", ylab = "Sum of Emissions"))



## Answer 3:
nei_summ_fips_type <- ddply(nei, ~year ~fips ~type, summarise, sumOfE = sum(Emissions))
nei_summ_fips_type <-  nei_summ_fips_type[nei_summ_fips$fips == 24510, ]
library(ggplot2)
ggplot(nei_summ_fips_type) + aes(year, sumOfE) + ylim(0,2000)  + geom_line(aes(color = type))

## g <- ggplot(nei_summ_fips_type[nei_summ_fips_type$type == "ON-ROAD",]) 

g <- ggplot(nei_summ_fips_type) + ylab(label = "Sum of Emissions")
g <- g + aes(year, sumOfE)  + geom_point(aes(color = type)) 
g <- g + facet_grid(.~type) + ylim(0,5000) + geom_smooth(method = "lm")
g

## Answer 4:
## scc_coal <- scc[scc$EI.Sector == levels(scc$EI.Sector)[grep("[c|C]oal",levels(scc$EI.Sector))], "SCC" ]
grep("[c|C]oal",scc$EI.Sector)
scc_coal <- scc[scc$EI.Sector == scc$EI.Sector[grep("[c|C]oal",scc$EI.Sector)], ]
str(scc_coal)
## nei$SCC_f <- as.factor(nei$SCC)
str(nei)
str(nei_coal)
library(plyr)
nei_coal <- join(nei, scc_coal, type = "inner")
nei_summ <- ddply(nei_coal, ~year, summarise, sumOfE = sum(Emissions))
with(nei_summ, plot(sumOfE ~ year, type = "l", ylab = "Sum of Emissions"))


## Answer 5:
grep("[v|V]ehicle", scc$EI.Sector)

scc_temp <- scc[scc$EI.Sector ==  unique(scc$EI.Sector), c("Data.Category", "EI.Sector")]
scc_temp[order(scc_temp),]

scc_vehicle <- scc[scc$Data.Category == "Onroad", ]
nei_vehicle <- nei[nei$fips == "24510", ]

str(scc$EI.Sector)
## nei$SCC_f <- as.factor(nei$SCC)
str(nei)


str(nei_vehicle)
library(plyr)
nei_scc_vehicle <- join(nei_vehicle, scc_vehicle)

nei_summ_fips <- ddply(nei_scc_vehicle, ~year, summarise, sumOfE = sum(Emissions))

with(nei_summ_fips, plot(sumOfE ~ year, type = "l", ylab = "Sum of Emissions"))


## Answer 6:

scc_vehicle <- scc[scc$Data.Category == "Onroad", ]
nei_vehicle <- nei[nei$fips == "24510" | nei$fips == "06037", ]

str(scc$EI.Sector)
## nei$SCC_f <- as.factor(nei$SCC)
str(nei)


str(nei_vehicle)
library(plyr)
nei_scc_vehicle <- join(nei_vehicle, scc_vehicle)

nei_summ_fips <- ddply(nei_scc_vehicle, ~fips ~year, summarise, sumOfE = sum(Emissions))

## with(nei_summ_fips, plot(sumOfE ~ year, type = "l", ylab = "Sum of Emissions"))

library(ggplot2)
ggplot(nei_summ_fips) + aes(year, sumOfE)  + geom_line(aes(color = fips))

