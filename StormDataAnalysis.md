Reproducible Research: Peer Assessment 2
==========================================
Created by Bui Dinh Ngoc - Feb 2016

## Effects on Public Health and Economy Caused by Storms and Other Severe Weather Events

### Synonpsis  
In this report, I aim to analyze the impact of different weather events on Public Health and Economy on the storm database collected from the U.S. National Oceanic and Atmospheric Administration's (NOAA) from 1950 - 2011. 

### Basic settings

```r
echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers
library(ggplot2)
library(plyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#install.packages("gridExtra") if it not installed yet
require(gridExtra)
```

```
## Loading required package: gridExtra
```

### Data Processing
First, i download the data file and unzip it.
We using R to download an extract

```r
require(gridExtra)
if (!"repdata-data-StormData.csv.bz2" %in% dir(".")) {
    
    download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "repdata-data-StormData.csv.bz2")
    #bunzip2("repdata-data-StormData.csv.bz2", overwrite=T, remove=F)
}
```
Then, we read the generated csv file. If the data already exists in the working environment, we do not need to load it again. Otherwise, we read the csv file.

```r
stormData <- read.csv("repdata-data-StormData.csv.bz2", sep = ",")
dim(stormData)
```

```
## [1] 902297     37
```

There are 902297 rows and 37 columns in total.



```r
if (dim(stormData)[2] == 37) {
    stormData$year <- as.numeric(format(as.Date(stormData$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
}
hist(stormData$year, breaks = 30)
```

![](StormDataAnalysis_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

So, i use the subset of the data from 1990 to 2011 to get most out of good records.


```r
storm <- stormData[stormData$year >= 1995, ]
dim(storm)
```

```
## [1] 681500     38
```

```r
hist(storm$year, breaks = 30)
```

![](StormDataAnalysis_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
Now, there are 681500 rows and 38 columns in total.

#### Most injuries Weather Events?

```r
groupByINJURIES <- group_by(storm, EVTYPE)
top5INJURIES <- summarise(groupByINJURIES ,
    total = sum(INJURIES)
) %>% arrange(desc(total)) %>% top_n(5)
```

```
## Selecting by total
```

```r
qplot(x=EVTYPE, y=total, data=top5INJURIES)
```

![](StormDataAnalysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

#### Most Severve Weather Events caused the most Economic Impact

### Property Damage

```r
unique(storm$PROPDMGEXP)
```

```
##  [1]   B M K m + 0 5 6 ? 4 2 3 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
storm$PROPDMGEXP <- as.character(storm$PROPDMGEXP)
storm$PROPDMGEXP = gsub("\\-|\\+|\\?","0",storm$PROPDMGEXP)
storm$PROPDMGEXP = gsub("B|b", "9", storm$PROPDMGEXP)
storm$PROPDMGEXP = gsub("M|m", "6", storm$PROPDMGEXP)
storm$PROPDMGEXP = gsub("K|k", "3", storm$PROPDMGEXP)
storm$PROPDMGEXP = gsub("H|h", "2", storm$PROPDMGEXP)
storm$PROPDMGEXP <- as.numeric(storm$PROPDMGEXP)
storm$PROPDMGEXP[is.na(storm$PROPDMGEXP)] = 0
storm$ActPropDam<- storm$PROPDMG * 10^storm$PROPDMGEXP
propDam <- aggregate(ActPropDam~EVTYPE, data=storm, sum)
propDam_reorder<- propDam[order(-propDam$ActPropDam),]
PropDam10<-propDam_reorder[1:5,]
```

Top 10 Events Causing Most Property Damage


```r
barplot(PropDam10$ActPropDam, 
        names = PropDam10$EVTYPE,
        cex.names = 0.7,
        cex.axis = 0.7,
        xlab = "Event Type",
        ylab = "Total Property Damage ($)",
        main = "Top 5 Events Causing \n Most Property Damage")
```

![](StormDataAnalysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


### Crop Damage


```r
unique(storm$CROPDMGEXP)
```

```
## [1]   M m K B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```

```r
storm$CROPDMGEXP <- as.character(storm$CROPDMGEXP)
storm$CROPDMGEXP = gsub("\\-|\\+|\\?","0",storm$CROPDMGEXP)
storm$CROPDMGEXP = gsub("B|b", "9", storm$CROPDMGEXP)
storm$CROPDMGEXP = gsub("M|m", "6", storm$CROPDMGEXP)
storm$CROPDMGEXP = gsub("K|k", "3", storm$CROPDMGEXP)
storm$CROPDMGEXP = gsub("H|h", "2", storm$CROPDMGEXP)
storm$CROPDMGEXP <- as.numeric(storm$CROPDMGEXP)
storm$CROPDMGEXP[is.na(storm$CROPDMGEXP)] = 0
storm$ActCropDam<- storm$CROPDMG * 10^storm$CROPDMGEXP
cropDam <- aggregate(ActCropDam~EVTYPE, data=storm, sum)
cropDam_orderd<- cropDam[order(-cropDam$ActCropDam),]
top5CropDam<-cropDam_orderd[1:5,]
barplot(top5CropDam$ActCropDam, 
        names = top5CropDam$EVTYPE,
        cex.names = 0.7,
        cex.axis = 0.7,
        xlab = "Event Type",
        ylab = "Total Crop Damage ($)",
        main = "Top 5 Events Causing \n Most Crop Damage")
```

![](StormDataAnalysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



```r
TotalDam <- aggregate(ActPropDam + ActCropDam~EVTYPE, data=storm, sum)
names(TotalDam)[2] <- "total"
topTotalDam <- arrange(TotalDam, desc(total)) %>% top_n(5)
```

```
## Selecting by total
```

```r
barplot(topTotalDam$total, 
        names = topTotalDam$EVTYPE,
        cex.names = 0.7,
        cex.axis = 0.7,
        xlab = "Event Type",
        ylab = "Total Crop Damage ($)",
        main = "Top 5 Events Causing \n Most Total Damage")
```

![](StormDataAnalysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### Conclusion  
From these data and my analysis TORNADO caused the most fatalities and most injuries. FLOOD caused the most property damage. DROUGHT caused the most crop damange, while FLOOD caused the most overall economic damage.
