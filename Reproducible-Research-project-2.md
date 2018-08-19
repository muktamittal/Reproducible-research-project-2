---
title: "Reproducible Research PA2"
author: "BillSeliger"
date: "Sunday, February 15, 2015"
output: html_document
---

##An Examination of the NOAA Storm Database - Health and Economic Impacts

####Synopsis
The below analysis examines the health and economic impact of certain storm events as captured in the NOAA Storm Database.  The exploration and analysis of the data clearly shows that Tornadoes, by far, have the greatest health impact as measured by the number of injuries and deaths.  The analysis also shows that Floods cause the greatest economic impact as measured by property damage and crop damage - Floods are also have a significant lead over the event type with the next greatest economic impact.  

##Assignment

*The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. You must use the database to answer the questions below and show the code for your entire analysis. Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.*

I require several packages here that will be used in my analysis

```r
require(dplyr) ## create tbl_df object
require(ggplot2) ## for plotting
require(tidyr) ## for data wrangling
require(gridExtra)
```

##Data Processing

The file provided with the assignement is read with read.csv and assign it to a dataframe storm.  I cache this chunk of R code so that it doesn't get re-evaluated each time the Rmd file is knit.

```r
##  This chuck will cache the read results and only re-evaluate when changes are present.

if(!exists("storm")) {
    setwd("C:/Users/rr046302/Documents/Bill's Stuff/Coursera/Reproducible Research/Reproducible-Research-Project-2")
    storm <- read.csv(bzfile("repdata-data-StormData.csv.bz2"))
    storm <- tbl_df(storm) ## structure the data as a tbl class
  }
```


####Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

I create a new object, storm_health, and summarize INJURIES and FATALITIES by EVTYPE using the group_by function in dplyr.  I create a new variable, total, the sum of INJURIES and FATALITIES.

```r
storm_health <- storm %>% group_by(EVTYPE) %>% summarise(total.fatalities = sum(FATALITIES), total.injuries = sum(INJURIES))
storm_health$total <- storm_health$total.fatalities + storm_health$total.injuries
```

During exploratory data analysis of the above results there is a clear Pareto effect with a small number of event types accounting for much of the harm to the population (the significnat few) and a large number of nonsignificant event types.  Of the 985 event types, 765 of them have 0 injuries or fatalities.

After review of several different cutoff points I selected a cutoff at the total fatalities and injuries at 500.  I filter out the nonsignificant data and the object I use for this analysis, top_storm_health, includes observations over 500 total injuries and fatalities

```r
top_storm_health <- filter(storm_health, total > 500)
top_storm_health_gather <- gather(top_storm_health, "impactType", "impactNumber",2:3)
```

##Results

This barplot shows the total injuries and fatalities caused by each event type:

```r
healthplot <- ggplot(top_storm_health_gather,aes(x=reorder(EVTYPE,total), y=impactNumber, fill=impactType)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  xlab("Event Type") + ylab("Population Impacted") + 
  ggtitle("Event Types Most Harmful to Population Health") + 
  theme(plot.title = element_text(size=20, face="bold", vjust=2))
print(healthplot)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

It is clear tornadoes cause the most damage as measured by either fatalities, injuries, or the sum of fatalities and injuries.  Because there is some question about how to measure the harm caused to society by a fatality relative to an injury the ranking of the event types after tornado may be in questions but it's clear that tornado causes the most harm to the population.

I believe this shows quite clearly that tornadoes cause the greatest harm to population health whether measured by total fatalities, total injuries, or a weighted sum of the two.  


####Across the United States, which types of events have the greatest economic consequences?

The variables PROPDMG and CROPDMG will be used to determine the economic consequences but it is observed that each of these two variables has an additional variable and PROPDMGEXP and CROPDMGEXP respectively that must be taken into account.  It is also observed that these two variables require rather extensive munging
First, the observations with PROPDMG or CROPDMG greater than 0 are filtered to create a manageable dataset

```r
storm_econ <- filter(storm, PROPDMG > 0 | CROPDMG > 0)
```

A variable year is created

```r
storm_econ$date <- as.Date(storm_econ$BGN_DATE, "%m/%d/%Y")
storm_econ$year <- as.numeric(format(storm_econ$date, "%Y")) 
```

Then two plots are created that show the number of observations and their magnitude for PROPDMG and CROPDMG

```r
propPlot <- qplot(year, PROPDMG, data = storm_econ) + 
  scale_x_continuous(breaks=c(1960, 1980, 1990, 1995, 2000, 2005, 2010))
cropPlot <- qplot(year, CROPDMG, data = storm_econ) + 
  scale_x_continuous(breaks=c(1960, 1980, 1990, 1995, 2000, 2005, 2010))
grid.arrange(propPlot, cropPlot, ncol=1, main = "Observations and Impact - PROPDMG and CROPDMG")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

For the purposes of this assignment the data from year 1995 and prior will be filtered out and the data from 1995 and later will be used for the analysis.  

```r
storm_econ_recent <- filter(storm_econ, year > 1995)
```

The PROPDMGEXP and CROPDMGEXP (exponents o the PROPDMG and CROPDMG fields) are cleaned here and a new variable is created that represents the value inclusive of both variables.

```r
storm_econ_recent$PROPDMGEXP <- as.character(storm_econ_recent$PROPDMGEXP)
storm_econ_recent$PROPDMGEXP[grep("K", storm_econ_recent$PROPDMGEXP)] <- "3"
storm_econ_recent$PROPDMGEXP[grep("M", storm_econ_recent$PROPDMGEXP)] <- "6"
storm_econ_recent$PROPDMGEXP[grep("B", storm_econ_recent$PROPDMGEXP)] <- "9"
storm_econ_recent$PROPDMGEXP[is.na(storm_econ_recent$PROPDMGEXP)] <- 0
storm_econ_recent$PROPDMGEXP <- as.numeric(as.character(storm_econ_recent$PROPDMGEXP))
storm_econ_recent$property.damage <- storm_econ_recent$PROPDMG * 10^storm_econ_recent$PROPDMGEXP

storm_econ_recent$CROPDMGEXP <- as.character(storm_econ_recent$CROPDMGEXP)
storm_econ_recent$CROPDMGEXP[grep("K", storm_econ_recent$CROPDMGEXP)] <- "3"
storm_econ_recent$CROPDMGEXP[grep("M", storm_econ_recent$CROPDMGEXP)] <- "6"
storm_econ_recent$CROPDMGEXP[grep("B", storm_econ_recent$CROPDMGEXP)] <- "9"
storm_econ_recent$CROPDMGEXP[is.na(storm_econ_recent$CROPDMGEXP)] <- 0
storm_econ_recent$CROPDMGEXP <- as.numeric(as.character(storm_econ_recent$CROPDMGEXP))
storm_econ_recent$crop.damage <- storm_econ_recent$CROPDMG * 10^storm_econ_recent$CROPDMGEXP
```

Here the crop damage and property damage are summed by event type for the period and a new variable is created, total.damage

```r
storm_econ_recent <- storm_econ_recent %>% group_by(EVTYPE) %>% summarise(property.damage = sum(property.damage), crop.damage = sum(crop.damage))
storm_econ_recent$total.damage <- storm_econ_recent$property.damage + storm_econ_recent$crop.damage
top_storm_econ_gather <- gather(top_storm_econ, "damageType", "damageAmount",2:3)
```

A plot of the event type by total economic damage is created showing the property and crop damage as a secondary color element in the plot

```r
econplot <- ggplot(top_storm_econ_gather, aes(reorder(x=EVTYPE,total.damage), y=damageAmount, fill=damageType)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  xlab("Event Type") + ylab("Total Economic Impact") + 
  ggtitle("Event Types with the Greatest Economic Consequences") + 
  theme(plot.title = element_text(size=16, face="bold", vjust=2))
print(econplot)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

It is clear that flood events events have the greatest economic impact (of events since 1995).  As we saw with the population health analysis, unless a weighting factor was applied to property damage and crop damage so that crop damage was more significantly important than property damage, flood events have the greatest economic impact of all event types, with a far lead over the second place type.







