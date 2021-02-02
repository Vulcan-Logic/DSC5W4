#load required packages
#check if data.table is available, stop if not
if(is.element("data.table", installed.packages()[,1])){ 
  require("data.table") 
} else{              
  stop("missing package: data.table, please install it first")
}

#check if dplyr is available, stop if not
if(is.element("dplyr", installed.packages()[,1])){ 
  require("dplyr") 
} else{            
  stop("missing package: dplyr, please install it first")
}


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


# script for proessing of injuries and fatalities data 
# subset those rows for which injuries and fatalities are not 0
dataInjFat<-data[which(data$FATALITIES!=0|data$INJURIES!=0),]
#add a column of total impact to human health
dataInjFat$impact<-dataInjFat$INJURIES+dataInjFat$FATALITIES
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
dataInjFat<-dataInjFatTmp1
#aggregate individual rows to prepare summaries for use in preparing reports 
dataInjFatTmp2<-as.data.frame(dataInjFat 
                              %>% group_by(EVTYPE) %>% summarise(sum(impact)))
dataInjFatTmp3<-as.data.frame(dataInjFat 
                              %>% group_by(EVTYPE) %>% summarise(sum(FATALITIES)))
dataInjFatTmp4<-as.data.frame(dataInjFat 
                              %>% group_by(EVTYPE) %>% summarise(sum(INJURIES)))
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
#prepare the final table
dataInjFat<-rbind(dataInjFatTmp2,dataInjFatTmp6)
#add a percentage column
totalImpact<-sum(dataInjFat$IMPACT)
dataInjFat$PERCENTAGE<-
  round((dataInjFat$IMPACT/totalImpact)*100,digits=2)
#print the result for the report
print(dataInjFat)
#get the labels for the plots
plotLabels<-as.character(dataInjFat[,"EVENT"])
#draw the plots

dataForPlot<-dataInjFat[,c("FATALITIES","INJURIES")]

x11(width=8,height=10)
par(mfrow=c(2,1),mai=c(2,1,0.5,0.5), mgp=c(4,1,0),omi=c(.5,.5,.5,.5),las=2)
s<-seq(0,6000,by=500)


barplot(dataForPlot$FATALITIES, 
        main="FATALATIES IN THE USA DUE TO NATURAL EVENTS", 
        ylab="FATALITIES", col="red", 
        names.arg=plotLabels, space=0.1, cex.axis=0.6, cex=0.8, yaxt="n")
axis(2,at=s,cex.axis=0.7)
mtext("MAJOR EVENTS", side=1, outer=TRUE,las=1)


s2<-seq(0,96000,by=8000)
barplot(dataForPlot$INJURIES, 
        main="INJURIES IN THE USA DUE TO NATURAL EVENTS", ylab="INJURIES",
        col="yellow", names.arg=plotLabels, space=0.1, cex.axis=0.6, cex=0.8,
        yaxt='n')
axis(2,at=s2,cex.axis=0.7)


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
#extract only the columns of interest
dataPropCropDmgTmp<-dataPropCropDmg[,c("STATE","EVTYPE","PROPDMG","CROPDMG")]
#compute a new column of total loss. use this to do computations, prepare
#summaries
dataPropCropDmgTmp$TOTALLOSS<-
  dataPropCropDmgTmp$PROPDMG+dataPropCropDmgTmp$CROPDMG

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

#print the result
print(dataPropCropDmg)

#get the labels for the plots
plotLabels<-as.character(dataPropCropDmg[,"EVENT"])
#draw the plots


x11(width=8,height=12)
par(mfrow=c(3,1),mar=c(10.1,6,3,2), mgp=c(4,1,0),oma=c(3,1,0,0),las=2)



s<-seq(0,180,by=15)
barplot(dataPropCropDmg$TOTAL_LOSS, 
        main=paste0("LOSS DUE TO PROPERTY AND CROP DAMAGE",
                   "\nIN THE USA DUE TO NATURAL EVENTS"), 
        ylab="LOSS in BILLIONS of USD", col=c("darkred"),
        names.arg=plotLabels, space=0.1, cex.axis=0.7, cex=0.8, yaxt="n")
axis(2,at=s,cex.axis=0.8)
mtext("MAJOR EVENTS", side=1, outer=TRUE,las=1)

s<-seq(0,180,by=15)
barplot(dataPropCropDmg$PROPERTY_DAMAGE, 
        main="LOSS DUE TO PROPERTY DAMAGE IN THE USA DUE TO NATURAL EVENTS", 
        ylab="LOSS in BILLIONS of USD", col=c("firebrick1"),
        names.arg=plotLabels, space=0.1, cex.axis=0.7, cex=0.8, yaxt="n")
axis(2,at=s,cex.axis=0.8)

s<-seq(0,15,by=1)
barplot(dataPropCropDmg$CROP_DAMAGE, 
        main="LOSS DUE TO CROP DAMAGE IN THE USA DUE TO NATURAL EVENTS", 
        ylab="LOSS in BILLIONS of USD",col="indianred3", names.arg=plotLabels, 
        space=0.1, cex.axis=0.7, cex=0.8, yaxt='n')
axis(2,at=s,cex.axis=0.8)

