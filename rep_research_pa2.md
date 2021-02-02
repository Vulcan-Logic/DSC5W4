---
title: "Reproducible Research Peer Assesment 2"
author: "Vineet W. Singh"
date: "17 January 2018"
output: 
  html_document:
    keep_md: true
    fig_width: 8
    fig_height: 10
---



# Impact on human health and economic loss due to major weather events and storms between 1950 and 2011, in the USA. 

## Abstract
Data of major storms and weather events that occurs in the US is recorded and forwarded by various US agencies to the U.S. National Oceanic and Atmospheric Administration's (NOAA) which collates and publishes this data online. 
The version of data available in the course website has information about 902297 major storms and weather events that took place between 1950 and 2011.  

The data consists of 37 specific pieces of information related to each event stored in columns. Relevant aspects of interest to this study was information stored in columns with the following labels: "STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP", "BGN_DATE", "END_DATE".  

After reading in the data some exploratory analysis was done to figure out the number of events recorded in the data and the structure of the data.  

In exploratory analysis, it was found that many events were recorded under duplicate or similar labels. Events with duplicate labels or events of similar nature were re-labeled so that they fell into one major category. 
Data (rows) that did not contribute to statistics of interest to this study i.e. that did not contribute to human or economic consequences was removed.  

The top ten events (labels) contributing to most impact in terms of human heath or economic consequences were extracted from the data after grouping the data and calculating statistics.  

Each event was relabeled to fall into 11 categories. Events belonging to the top ten events extracted in a previous step were retained and all other data was relabeled to fall in the "OTHER" event type label.   

Data was then grouped by event types and statistic of interest i.e. total sum of each relevant aspect was calculated and stored as results.  

Results and relevant analysis in the form of tables and barplots are included in this study.  

## Data Processing 
Packages required for the purpose of the study were checked for inclusion

```r
#load required packages
#check if data.table is available, stop if not
if(is.element("data.table", installed.packages()[,1])){ 
  require("data.table") 
} else{              
  stop("missing package: data.table, please install it first")
}
```

```
## Loading required package: data.table
```

```r
#check if dplyr is available, stop if not
if(is.element("dplyr", installed.packages()[,1])){ 
  require("dplyr") 
} else{            
  stop("missing package: dplyr, please install it first")
}
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
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
#check if lubridate is available, stop if not
if(is.element("lubridate", installed.packages()[,1])){ 
  require("lubridate") 
} else{            
  stop("missing package: lubridate, please install it first")
}
```

```
## Loading required package: lubridate
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:data.table':
## 
##     hour, isoweek, mday, minute, month, quarter, second, wday,
##     week, yday, year
```

```
## The following object is masked from 'package:base':
## 
##     date
```
Data was downloaded after checking if a copy was available locally or not:

```r
#check to see if input data exists or download it and then read it
if (file.exists("repdata_data_StormData.csv")){
  data<-fread('repdata_data_StormData.csv',select=c(
    "STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG",
    "CROPDMGEXP","BGN_DATE","END_DATE"))
} else {
  url<-
    "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(url,destfile="./datazip.zip",method="curl")
  unzip('./datazip.zip',exdir='./')
  data<-fread('repdata_data_StormData.csv',select=c(
    "STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG",
    "CROPDMGEXP","BGN_DATE","END_DATE"))
}
```

```
## Read 0.0% of 967216 rowsRead 43.4% of 967216 rowsRead 75.5% of 967216 rowsRead 902297 rows and 10 (of 37) columns from 0.523 GB file in 00:00:06
```
Based on some exploratory analysis in which all the major event type labels were listed and a number of similar or duplicate labels were found, the data was relabeled to remove duplicacy and ambiguity of event labels. This was done to imrove the accuracy of the calculations as data belonging to the same major event but treated differently due to a similar or duplicate label would not be included in the computed statistic when grouping data according to event type labels was done.

```r
#preprocessing of data
data<-as.data.frame(data)
#convert case of all event types
data$EVTYPE<-toupper(data$EVTYPE)
# remove duplicate event labels to cover major categories of events 
#  collate data under duplicate labels to ease processing
data$EVTYPE<-ifelse(as.character(data$EVTYPE) %in% c(
  "FLASH FLOOD/FLOOD","FLASH FLOODING","FLASH FLOODING/FLOOD","FLASH FLOODS",
  "FLOOD","FLOOD & HEAVY RAIN","FLOOD/FLASH FLOOD","FLOOD/RIVER FLOOD",
  "FLOODING","FLASH FLOOD"),"FLOOD",data$EVTYPE)
data$EVTYPE<-ifelse(as.character(data$EVTYPE) %in% c(
  "THUNDERSTORM","THUNDERSTORM  WINDS","THUNDERSTORM WIND",
  "THUNDERSTORM WIND (G40)","THUNDERSTORM WIND G52",
  "THUNDERSTORM WINDS","THUNDERSTORM WINDS 13",
  "THUNDERSTORM WINDS/HAIL","THUNDERSTORM WINDSS","THUNDERSTORMS WINDS",
  "THUNDERSTORMW","THUNDERTORM WINDS","LIGHTNING AND THUNDERSTORM WIN",
  "TSTM WIND","TSTM WIND (G35)","TSTM WIND (G40)","TSTM WIND (G45)",
  "TSTM WIND/HAIL"),"THUNDERSTORM",data$EVTYPE)
data$EVTYPE<-ifelse(as.character(data$EVTYPE) %in% c("HEAT",
     "HEAT WAVE","HEAT WAVES","EXCESSIVE HEAT", "EXTREME HEAT"),"HEAT",
                              data$EVTYPE)
data$EVTYPE<-ifelse(as.character(data$EVTYPE) %in% c(
  "HIGH WIND","HIGH WIND 48","HIGH WIND AND SEAS","HIGH WIND/SEAS",
  "HIGH WINDS","HIGH WINDS/COLD","HIGH WINDS/SNOW","HIGH WIND/HEAVY SNOW"),
  "HIGH WIND",data$EVTYPE)
data$EVTYPE<-ifelse(as.character(data$EVTYPE) %in% c(
  "GLAZE/ICE STORM","ICE STORM","ICE STORM/FLASH FLOOD","WINTER STORM",
  "WINTER STORM HIGH WINDS","WINTER STORMS"),
  "WINTER STORM",data$EVTYPE)
data$EVTYPE<-ifelse(as.character(data$EVTYPE) %in% c(
  "WILD FIRES","WILD/FOREST FIRE","WILDFIRE" ),
  "WILDFIRE",data$EVTYPE)
data$EVTYPE<-ifelse(as.character(data$EVTYPE) %in% c(
  "HURRICANE","HURRICANE EDOUARD","HURRICANE EMILY","HURRICANE ERIN",
  "HURRICANE FELIX","HURRICANE OPAL","HURRICANE OPAL/HIGH WINDS",
  "HURRICANE/TYPHOON","TROPICAL STORM", "TROPICAL STORM GORDON","TYPHOON"),
  "HURRICANE/TYPHOON",data$EVTYPE)
data$EVTYPE<-ifelse(as.character(data$EVTYPE) %in% c(
  "LIGHTNING","LIGHTNING INJURY","LIGHTNING."),
  "LIGHTNING",data$EVTYPE)
data$EVTYPE<-ifelse(as.character(data$EVTYPE) %in% c(
  "ICE ON ROAD","ICE ROADS","ICY ROADS"),"ICY ROADS",data$EVTYPE)
```
The next processing was done in two stages. 
In the first stage, the impact on human health from major storms and weather events was calculated. To do this, all rows that did not involve any injuries or fatalities were removed to give a subset of data that has non zero rows for either injuries or fatalities. 
A new column named "impact" that added up the fatalities and injuries for each row was added to the data frame.

```r
# script for proessing of injuries and fatalities data 
# subset those rows for which injuries and fatalities are not 0
dataInjFat<-data[which(data$FATALITIES!=0|data$INJURIES!=0),]
#add a column of total impact to human health
dataInjFat$impact<-dataInjFat$INJURIES+dataInjFat$FATALITIES
```
The number of events that resulted in fatalities or injuries was recorded. 
Data was grouped by event labels and the impact (injuries+fatalities) were summed up by group and sorted/arranged to extract the labels of the top ten events that have the maximum contribution in impact on human health. 
Rows with event type label belonging to the top ten events were extracted. The rest were relabeled as "OTHER" to make it easier to present results. 

```r
noEventsFatInj<-dim(dataInjFat)
dataInjFatTmp1<-dataInjFat
#extract list of top ten events that have the most impact
dataInjFatTmp<-as.data.frame(dataInjFatTmp1 %>% group_by(EVTYPE) 
                             %>% summarise(sum(impact)))
dataInjFatTmp8<-arrange(dataInjFatTmp, desc(dataInjFatTmp$`sum(impact)`))
#extract labels of the ten events that have the most impact
dataInjFatTmp8<-dataInjFatTmp8[1:10,]
#store the labels in a variable 
eventInjFat<-as.character(dataInjFatTmp8[,"EVTYPE"])
#convert all labels other than those in the eventIntFat varibale to other
#to make collation of data easier 
dataInjFatTmp1$EVTYPE<-ifelse(as.character(dataInjFatTmp1$EVTYPE) 
                              %in% eventInjFat , dataInjFatTmp1$EVTYPE,"OTHER")
```
Next rows(data) were grouped by each event type label and summed to provide the impact on human health under each event category. Intermediate results were stored in temporary data frames. 

```r
dataInjFat<-dataInjFatTmp1
#aggregate individual rows to prepare summaries for use in preparing reports 
dataInjFatTmp2<-as.data.frame(dataInjFat 
                              %>% group_by(EVTYPE) %>% summarise(sum(impact)))
dataInjFatTmp3<-as.data.frame(dataInjFat 
                              %>% group_by(EVTYPE) %>% summarise(sum(FATALITIES)))
dataInjFatTmp4<-as.data.frame(dataInjFat 
                              %>% group_by(EVTYPE) %>% summarise(sum(INJURIES)))
```
Names were given to the columns of the temporary data frames and merged together to form 1 data frame. 
The rows in this data frame were further extracted, sorted and organised so that the top ten rows of the frame corresponding to the top ten events that have the maximum impact on human health. 

```r
#give proper names to columns
names(dataInjFatTmp2)<-c("EVENT","IMPACT")
names(dataInjFatTmp3)<-c("EVENT","FATALITIES")
names(dataInjFatTmp4)<-c("EVENT","INJURIES")
#merge data into final data frame
dataInjFatTmp2<-merge(dataInjFatTmp2,dataInjFatTmp3,by="EVENT")
dataInjFatTmp2<-merge(dataInjFatTmp2,dataInjFatTmp4,by="EVENT")
#pull out data belonging to major events so that other is the last category
dataInjFatTmp5<-dataInjFatTmp2[which(dataInjFatTmp2$EVENT!="OTHER"),]
dataInjFatTmp6<-dataInjFatTmp2[which(dataInjFatTmp2$EVENT=="OTHER"),]
#arrange top ten by descending order of impact
dataInjFatTmp2<-arrange(dataInjFatTmp5,desc(dataInjFatTmp5$IMPACT))
```
The temporary data frames were again merged to give the final data frame that also carries the results. 
A column that specifies % value for each event type was included. 

```r
#prepare the final table
dataInjFat<-rbind(dataInjFatTmp2,dataInjFatTmp6)
#add a percentage column
totalImpact<-sum(dataInjFat$IMPACT)
dataInjFat$PERCENTAGE<-
  round((dataInjFat$IMPACT/totalImpact)*100,digits=2)
```
In the second stage, the economic impact from major storms and weather events was calculated. To do this, all rows that did not involve any property or crop damage were removed to give a subset of data that has non zero rows for either property or crop damage. 

```r
# script for proessing of properties and crop loss data 
# subset those rows for which property and crop damage are not 0
dataPropCropDmg<-data[which((data$PROPDMG!=0)|(data$CROPDMG!=0)),]
end<-as.numeric(dim(dataPropCropDmg)[1])
#convert damage to millions for consistency
for (ctr in 1:end){
  if (dataPropCropDmg[ctr,"PROPDMGEXP"]=="K"){
    dataPropCropDmg[ctr,"PROPDMG"]<-dataPropCropDmg[ctr,"PROPDMG"]*.001
  }
  else if 
  (dataPropCropDmg[ctr,"PROPDMGEXP"]=="B"){
    dataPropCropDmg[ctr,"PROPDMG"]<-dataPropCropDmg[ctr,"PROPDMG"]*1000
  }
  if (dataPropCropDmg[ctr,"CROPDMGEXP"]=="K"){
    dataPropCropDmg[ctr,"CROPDMG"]<-dataPropCropDmg[ctr,"CROPDMG"]*.001
  }
  else if 
  (dataPropCropDmg[ctr,"CROPDMGEXP"]=="B"){
    dataPropCropDmg[ctr,"CROPDMG"]<-dataPropCropDmg[ctr,"CROPDMG"]*1000
  }
}
```
The columns of interest were extracted into a smaller data frame.
A new column named "TOTALLOSS" that added up the property and crop damage for each row was added to the data. 

```r
#extract only the columns of interest
dataPropCropDmgTmp<-dataPropCropDmg[,c("STATE","EVTYPE","PROPDMG","CROPDMG")]
#compute a new column of total loss. use this to do computations, prepare
#summaries
dataPropCropDmgTmp$TOTALLOSS<-
  dataPropCropDmgTmp$PROPDMG+dataPropCropDmgTmp$CROPDMG
```
Data was grouped by event labels and the total loss were summed up by group and sorted/arranged to extract the labels of the top ten events that have the maximum contribution in impact on human health. 
Rows with event type label belonging to the top ten events were extracted. The rest were relabeled as "OTHER" to make it easier to present results. 

```r
#extract list of top ten events that have the most impact
dataPropCropDmgTmp1<-as.data.frame(dataPropCropDmgTmp %>% group_by(EVTYPE) 
                                   %>% summarise(sum(TOTALLOSS)))
dataPropCropDmgTmp8<-arrange(dataPropCropDmgTmp1, 
                             desc(dataPropCropDmgTmp1$`sum(TOTALLOSS)`))
#extract list of 10 events that have the most impact on loss
dataPropCropDmgTmp8<-dataPropCropDmgTmp8[1:10,]
#store the labels in a variable 
eventPropCrop<-as.character(dataPropCropDmgTmp8[,"EVTYPE"])
#convert all labels other than those in the eventIntFat varibale to other
#to make collation of data easier 
dataPropCropDmgTmp$EVTYPE<-ifelse(as.character(dataPropCropDmgTmp$EVTYPE) %in% 
                  eventPropCrop,dataPropCropDmgTmp$EVTYPE,"OTHER")
```
Next rows(data) were grouped by each event type label and summed to provide the loss by property and crop damage under each event category. Intermediate results were stored in temporary data frames. 

```r
dataPropCropDmgTmp$EVTYPE<-factor(dataPropCropDmgTmp$EVTYPE)
dataPropCropDmgTmpLevels<-levels(dataPropCropDmgTmp$EVTYPE)
#aggregate individual rows to prepare summaries for use in preparing reports 
dataPropCropDmgTmp2<-as.data.frame(dataPropCropDmgTmp 
                  %>% group_by(EVTYPE) %>% summarise(sum(TOTALLOSS)))
dataPropCropDmgTmp3<-as.data.frame(dataPropCropDmgTmp 
                  %>% group_by(EVTYPE) %>% summarise(sum(PROPDMG)))
dataPropCropDmgTmp4<-as.data.frame(dataPropCropDmgTmp 
                  %>% group_by(EVTYPE) %>% summarise(sum(CROPDMG)))
dataPropCropDmgTmp5<-arrange(dataPropCropDmgTmp2,
                  desc(dataPropCropDmgTmp2$`sum(TOTALLOSS)`))
```
Names were given to the columns of the temporary data frames and merged together to form 1 data frame. 
The rows in this data frame were further extracted, sorted and organised so that the top ten rows of the frame corresponding to the top ten events that cause the maximum loss to property and crops. 

```r
#give proper names to columns
names(dataPropCropDmgTmp2)<-c("EVENT","TOTAL_LOSS")
names(dataPropCropDmgTmp3)<-c("EVENT","PROPERTY_DAMAGE")
names(dataPropCropDmgTmp4)<-c("EVENT","CROP_DAMAGE")
#merge data into final data frame
dataPropCropDmgTmp2<-merge(dataPropCropDmgTmp2,dataPropCropDmgTmp3,by="EVENT")
dataPropCropDmgTmp2<-merge(dataPropCropDmgTmp2,dataPropCropDmgTmp4,by="EVENT")
#pull out data belonging to major events so that other is the last category
dataPropCropDmgTmp5<-
  dataPropCropDmgTmp2[which(dataPropCropDmgTmp2$EVENT!="OTHER"),]
dataPropCropDmgTmp6<-
  dataPropCropDmgTmp2[which(dataPropCropDmgTmp2$EVENT=="OTHER"),]
#arrange top ten by descending order of impact
dataPropCropDmgTmp2<-
  arrange(dataPropCropDmgTmp5,desc(dataPropCropDmgTmp5$TOTAL_LOSS))
```
The temporary data frames were again merged to give the final data frame that also carries the results. 
A column that specifies % value for each event type was included. 
As a final step the figures in the frame were converted to billions of dollars. 
We also make a couple of separate tables to provide figures for property and crop damage individually. 

```r
#prepare the final table
dataPropCropDmg<-
  rbind(dataPropCropDmgTmp2,dataPropCropDmgTmp6)
#add a percentage column
totaldamage<-sum(dataPropCropDmg$TOTAL_LOSS)
dataPropCropDmg$PERCENTAGE<-
  round((dataPropCropDmg$TOTAL_LOSS/totaldamage)*100,digits=2)
#convert figures to billions of dollars from millions
dataPropCropDmg$TOTAL_LOSS<-dataPropCropDmg$TOTAL_LOSS/1000
dataPropCropDmg$PROPERTY_DAMAGE<-dataPropCropDmg$PROPERTY_DAMAGE/1000
dataPropCropDmg$CROP_DAMAGE<-dataPropCropDmg$CROP_DAMAGE/1000
#prepare a table for property damage
dataPropDmgTmp<-
  arrange(dataPropCropDmgTmp5,desc(dataPropCropDmgTmp5$PROPERTY_DAMAGE))
dataPropDmg<-rbind(dataPropDmgTmp,dataPropCropDmgTmp6)
dataPropDmg<-dataPropDmg[,c("EVENT","PROPERTY_DAMAGE")]
totalPropDmg<-sum(dataPropDmg$PROPERTY_DAMAGE)
dataPropDmg$PERCENTAGE<-
round((dataPropDmg$PROPERTY_DAMAGE/totalPropDmg)*100,digits=2)
dataPropDmg$PROPERTY_DAMAGE<-dataPropDmg$PROPERTY_DAMAGE/1000
#prepare a table for crop damage
dataCropDmgTmp<-
  arrange(dataPropCropDmgTmp5,desc(dataPropCropDmgTmp5$CROP_DAMAGE))
dataCropDmg<-rbind(dataCropDmgTmp,dataPropCropDmgTmp6)
dataCropDmg<-dataCropDmg[,c("EVENT","CROP_DAMAGE")]
totalCropDmg<-sum(dataCropDmg$CROP_DAMAGE)
dataCropDmg$PERCENTAGE<-
round((dataCropDmg$CROP_DAMAGE/totalCropDmg)*100,digits=2)
dataCropDmg$CROP_DAMAGE<-dataCropDmg$CROP_DAMAGE/1000
```
## Results
The total number of events for which data is recorded is: 

```r
print(paste(dim(data)[1],"events"),row.names=FALSE)
```

```
## [1] "902297 events"
```
The earliest date and last date of events recorded in the data file are:  

```r
beginDate<-mdy_hms(as.character(head(data$BGN_DATE,1)))
endDate<-mdy_hms(as.character(tail(data$END_DATE,1)))
print(paste("The first event included in the file began on:", beginDate))
```

```
## [1] "The first event included in the file began on: 1950-04-18"
```

```r
print(paste("The last event included in the file ended on", endDate))
```

```
## [1] "The last event included in the file ended on 2011-11-29"
```
The total number of events in which a fatality or injury occured is: 

```r
print(paste(noEventsFatInj[1],"events"),row.names=FALSE)
```

```
## [1] "21929 events"
```
The total number of events in which some property or crop damage was incurred is:

```r
print(paste(end,"events"),row.names=FALSE)
```

```
## [1] "245031 events"
```
The following table and graph list/display the top ten types of major weather and storm events in which there was an impact on human health in terms of injuries and fatalities.  


```r
#print the result for the report
print(dataInjFat,row.names = FALSE)
```

```
##              EVENT IMPACT FATALITIES INJURIES PERCENTAGE
##            TORNADO  96979       5633    91346      62.30
##               HEAT  12272       3113     9159       7.88
##       THUNDERSTORM  10220        711     9509       6.57
##              FLOOD  10104       1513     8591       6.49
##          LIGHTNING   6048        817     5231       3.89
##       WINTER STORM   3651        306     3345       2.35
##  HURRICANE/TYPHOON   1915        201     1714       1.23
##          HIGH WIND   1764        293     1471       1.13
##           WILDFIRE   1696         90     1606       1.09
##               HAIL   1376         15     1361       0.88
##              OTHER   9648       2453     7195       6.20
```
<center> **_Table 1_** </center> 
---

```r
#get the labels for the plots
plotLabels<-as.character(dataInjFat[,"EVENT"])
#draw the plots
dataForPlot<-dataInjFat[,c("FATALITIES","INJURIES")]
par(mfrow=c(2,1),mai=c(2,1,0.5,0.5), mgp=c(4,1,0),omi=c(.5,.5,.5,.5),las=2)
s<-seq(0,6000,by=500)
barplot(dataForPlot$FATALITIES, 
        main="FATALATIES IN THE USA DUE TO MAJOR WEATHER EVENTS", 
        ylab="FATALITIES", col="red", 
        names.arg=plotLabels, space=0.1, cex.axis=0.6, cex=0.8, yaxt="n")
axis(2,at=s,cex.axis=0.7)
mtext("MAJOR EVENTS", side=1, outer=TRUE,las=1)
s2<-seq(0,96000,by=8000)
barplot(dataForPlot$INJURIES, 
        main="INJURIES IN THE USA DUE TO MAJOR WEATHER EVENTS", ylab="INJURIES",
        col="yellow", names.arg=plotLabels, space=0.1, cex.axis=0.6, cex=0.8,
        yaxt='n')
axis(2,at=s2,cex.axis=0.7)
```

![](rep_research_pa2_files/figure-html/graph1-1.png)<!-- -->
<center>**_Graph 1_**</center>   
---

The following table and graph list/display the top ten types of major weather and storm events in which there was an economic impact in terms of properties and crop damage.


```r
#print the result
print(dataPropCropDmg,row.names=FALSE)
```

```
##              EVENT TOTAL_LOSS PROPERTY_DAMAGE CROP_DAMAGE PERCENTAGE
##              FLOOD 169.484319      162.271307   7.2130111      34.92
##  HURRICANE/TYPHOON  99.255189       93.060226   6.1949638      20.45
##            TORNADO  57.810814       57.235860   0.5749531      11.91
##        STORM SURGE  43.323541       43.323536   0.0000050       8.93
##               HAIL  19.525504       16.059967   3.4655374       4.02
##       THUNDERSTORM  17.492593       16.175305   1.3172879       3.60
##       WINTER STORM  15.799483       10.744925   5.0545575       3.26
##            DROUGHT  15.018672        1.046106  13.9725660       3.09
##        RIVER FLOOD  10.148404        5.118945   5.0294590       2.09
##           WILDFIRE   8.793313        8.391044   0.4022696       1.81
##              OTHER  28.676277       22.090131   6.5861454       5.91
```
<center> **_Table 2:Property & Crop Damage (combined)_** </center>  
---

The following table lists the property damage losses due to major weather events.

```r
print(dataPropDmg,row.names=FALSE)
```

```
##              EVENT PROPERTY_DAMAGE PERCENTAGE
##              FLOOD      162.271307      37.26
##  HURRICANE/TYPHOON       93.060226      21.37
##            TORNADO       57.235860      13.14
##        STORM SURGE       43.323536       9.95
##       THUNDERSTORM       16.175305       3.71
##               HAIL       16.059967       3.69
##       WINTER STORM       10.744925       2.47
##           WILDFIRE        8.391044       1.93
##        RIVER FLOOD        5.118945       1.18
##            DROUGHT        1.046106       0.24
##              OTHER       22.090131       5.07
```
<center> **_Table 3:Property Damage_** </center>  
---  

The following table lists the property damage losses due to major weather events.

```r
print(dataCropDmg,row.names=FALSE)
```

```
##              EVENT CROP_DAMAGE PERCENTAGE
##            DROUGHT  13.9725660      28.05
##              FLOOD   7.2130111      14.48
##  HURRICANE/TYPHOON   6.1949638      12.44
##       WINTER STORM   5.0545575      10.15
##        RIVER FLOOD   5.0294590      10.10
##               HAIL   3.4655374       6.96
##       THUNDERSTORM   1.3172879       2.64
##            TORNADO   0.5749531       1.15
##           WILDFIRE   0.4022696       0.81
##        STORM SURGE   0.0000050       0.00
##              OTHER   6.5861454      13.22
```
<center> **_Table 4:Crop Damage_** </center>  
---  


```r
#get the labels for the plots
plotLabels<-as.character(dataPropCropDmg[,"EVENT"])
#draw the plots
par(mfrow=c(3,1),mar=c(10.1,6,3,2), mgp=c(4,1,0),oma=c(3,1,0,0),las=2)
s<-seq(0,180,by=15)
barplot(dataPropCropDmg$TOTAL_LOSS, 
        main=paste0("LOSS DUE TO PROPERTY AND CROP DAMAGE",
                   "\nIN THE USA DUE TO MAJOR WEATHER EVENTS"), 
        ylab="LOSS in BILLIONS of USD", col=c("darkred"),
        names.arg=plotLabels, space=0.1, cex.axis=0.7, cex=0.8, yaxt="n")
axis(2,at=s,cex.axis=0.8)
mtext("MAJOR EVENTS", side=1, outer=TRUE,las=1)
s<-seq(0,180,by=15)
barplot(dataPropCropDmg$PROPERTY_DAMAGE, 
        main="LOSS DUE TO PROPERTY DAMAGE IN THE USA DUE TO MAJOR WEATHER EVENTS", 
        ylab="LOSS in BILLIONS of USD", col=c("firebrick1"),
        names.arg=plotLabels, space=0.1, cex.axis=0.7, cex=0.8, yaxt="n")
axis(2,at=s,cex.axis=0.8)
s<-seq(0,15,by=1)
barplot(dataPropCropDmg$CROP_DAMAGE, 
        main="LOSS DUE TO CROP DAMAGE IN THE USA DUE TO MAJOR WEATHER EVENTS", 
        ylab="LOSS in BILLIONS of USD",col="indianred3", names.arg=plotLabels, 
        space=0.1, cex.axis=0.7, cex=0.8, yaxt='n')
axis(2,at=s,cex.axis=0.8)
```

![](rep_research_pa2_files/figure-html/graph2-1.png)<!-- -->
<center>**_Graph 2_** </center>   
---  

## Conclusions: 
From the table and graphs displayed above, it can be inferred that the maximum impact on human health in terms of both fatalities and injuries was caused by tornadoes. Tornadoes accounted for 62% of all fatalities and injuries(combined figure). 
The top three types of events that caused maximum injuries and fatalities were: Tornados, Heat related events and Thunderstorms and these three accounted for 77% of all injuries and fatalities. The top ten types of events caused 94% of all recorded injuries and fatalities.  

The maximum loss in terms of property damage was flood damage resulting in a loss of 169.48 billion dollars and the the maximum loss in terms of crop damage was due to drought wherein the loss amounted to 13.97 billion dollars.  

The top three event types that caused the maximum loss in terms of property and crop damage (combined) were: floods, typhoons and tornados and these three types were responsible for 67% of the total loss caused.  

The top three events for property damage were floods, typhoons and tornados and they account for around 72% of the total loss caused whereas in the case of crop damage; droughts, floods and hurrianes were the top three types of events that caused the maximum economic loss (aggregating to 55% of the total crop loss). In all, for crops, the top five types of events, caused 75% of the total crop loss. 
The top ten types of events were also responsible for 94% of the total combined (property+crop damage) loss.  

---  

