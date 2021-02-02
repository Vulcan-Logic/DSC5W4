


library(data.table)
library(dplyr)
data1<-fread('repdata_data_StormData.csv',select=c(
  "STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG",
  "CROPDMGEXP"))
data1<-as.data.frame(data1)
# script for proessing of injuries and fatalities data 
# subset those rows for which injuries and fatalities are not 0
dataInjFat<-data1[which(data1$FATALITIES!=0|data1$INJURIES!=0),]
#convert all event labels to uppercase for processing
dataInjFat$EVTYPE<-toupper(dataInjFat$EVTYPE)
#add up injuries and fatalities to give us a new column
dataInjFat$impact<-dataInjFat$INJURIES+dataInjFat$FATALITIES
dataInjFatTmp1<-dataInjFat
# remove duplicate event labels to cover major categories of events 
#  collate data under duplicate labels to ease processing
dataInjFatTmp1$EVTYPE<-ifelse(as.character(dataInjFatTmp1$EVTYPE) %in% c(
  "FLASH FLOOD/FLOOD","FLASH FLOODING","FLASH FLOODING/FLOOD","FLASH FLOODS",
  "FLOOD","FLOOD & HEAVY RAIN","FLOOD/FLASH FLOOD","FLOOD/RIVER FLOOD",
  "FLOODING","FLASH FLOOD"),"FLOOD",dataInjFatTmp1$EVTYPE)
dataInjFatTmp1$EVTYPE<-ifelse(as.character(dataInjFatTmp1$EVTYPE) %in% c(
  "THUNDERSTORM","THUNDERSTORM  WINDS","THUNDERSTORM WIND",
  "THUNDERSTORM WIND (G40)","THUNDERSTORM WIND G52",
  "THUNDERSTORM WINDS","THUNDERSTORM WINDS 13",
  "THUNDERSTORM WINDS/HAIL","THUNDERSTORM WINDSS","THUNDERSTORMS WINDS",
  "THUNDERSTORMW","THUNDERTORM WINDS","LIGHTNING AND THUNDERSTORM WIN",
  "TSTM WIND","TSTM WIND (G35)","TSTM WIND (G40)","TSTM WIND (G45)",
  "TSTM WIND/HAIL"),"THUNDERSTORM",dataInjFatTmp1$EVTYPE)
dataInjFatTmp1$EVTYPE<-ifelse(as.character(dataInjFatTmp1$EVTYPE) %in% c("HEAT",
  "HEAT WAVE","HEAT WAVES","EXCESSIVE HEAT", "EXTREME HEAT"),"HEAT",
  dataInjFatTmp1$EVTYPE)
dataInjFatTmp1$EVTYPE<-ifelse(as.character(dataInjFatTmp1$EVTYPE) %in% c(
  "HIGH WIND","HIGH WIND 48","HIGH WIND AND SEAS","HIGH WIND/SEAS",
  "HIGH WINDS","HIGH WINDS/COLD","HIGH WINDS/SNOW","HIGH WIND/HEAVY SNOW"),
  "HIGH WIND",dataInjFatTmp1$EVTYPE)
dataInjFatTmp1$EVTYPE<-ifelse(as.character(dataInjFatTmp1$EVTYPE) %in% c(
  "GLAZE/ICE STORM","ICE STORM","ICE STORM/FLASH FLOOD","WINTER STORM",
  "WINTER STORM HIGH WINDS","WINTER STORMS"),
  "WINTER STORM",dataInjFatTmp1$EVTYPE)
dataInjFatTmp1$EVTYPE<-ifelse(as.character(dataInjFatTmp1$EVTYPE) %in% c(
  "WILD FIRES","WILD/FOREST FIRE","WILDFIRE" ),
  "WILDFIRE",dataInjFatTmp1$EVTYPE)
dataInjFatTmp1$EVTYPE<-ifelse(as.character(dataInjFatTmp1$EVTYPE) %in% c(
  "HURRICANE","HURRICANE EDOUARD","HURRICANE EMILY","HURRICANE ERIN",
  "HURRICANE FELIX","HURRICANE OPAL","HURRICANE OPAL/HIGH WINDS",
  "HURRICANE/TYPHOON","TROPICAL STORM", "TROPICAL STORM GORDON","TYPHOON"),
  "HURRICANE/TYPHOON",dataInjFatTmp1$EVTYPE)
dataInjFatTmp1$EVTYPE<-ifelse(as.character(dataInjFatTmp1$EVTYPE) %in% c(
  "LIGHTNING","LIGHTNING INJURY","LIGHTNING."),
  "LIGHTNING",dataInjFatTmp1$EVTYPE)
dataInjFatTmp1$EVTYPE<-ifelse(as.character(dataInjFatTmp1$EVTYPE) %in% c(
  "ICE ON ROAD","ICE ROADS","ICY ROADS"),"ICY ROADS",dataInjFatTmp1$EVTYPE)

#extract list of top ten events that have the most impact
dataInjFatTmp1$EVTYPE<-factor(dataInjFatTmp1$EVTYPE)
dataInjFatTmp<-as.data.frame(dataInjFatTmp1 %>% group_by(EVTYPE) 
                             %>% summarise(sum(impact)))
dataInjFatTmp8<-arrange(dataInjFatTmp, desc(dataInjFatTmp$`sum(impact)`))
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
dataInjFat<-rbind(dataInjFatTmp2,dataInjFatTmp6)
#add a percentage column

#get the labels for the plots
plotLabels<-dataInjFat[,"EVENT"]
#draw the plots
par(las=2)
dataForPlot<-dataInjFat[,c("FATALITIES","INJURIES")]
barplot(dataForPlot$FATALITIES, 
        main="FATALATIES IN THE US DUE TO NATURAL EVENTS", 
        ylab="Total", col="red", 
        names.arg=plotLabels, space=0.1, cex.axis=0.8, cex=0.8)
barplot(dataForPlot$INJURIES, 
        main="INJURIES IN THE US DUE TO NATURAL EVENTS", ylab="Total", 
        col="yellow", names.arg=plotLabels, space=0.1, cex.axis=0.8, cex=0.8)





dataPropCropDmg<-data1[which((data1$PROPDMG!=0)|(data1$CROPDMG!=0)),]
end<-as.numeric(dim(dataPropCropDmg)[2])                                 
for (ctr in 1:100){
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
dataPropCropDmgTmp<-dataPropCropDmg[,c("STATE","EVTYPE","PROPDMG","CROPDMG")]
dataPropCropDmgTmp$EVTYPE<-toupper(dataPropCropDmgTmp$EVTYPE)


dataPropCropDmgTmp$EVTYPE<-ifelse(as.character(dataPropCropDmgTmp$EVTYPE) %in% 
    c("FLASH FLOOD/FLOOD","FLASH FLOODING","FLASH FLOODING/FLOOD",
      "FLASH FLOODS","FLOOD","FLOOD & HEAVY RAIN","FLOOD/FLASH FLOOD",
      "FLOOD/RIVER FLOOD","FLOODING","FLASH FLOOD"),"FLOOD",
    dataPropCropDmgTmp$EVTYPE)
dataPropCropDmgTmp$EVTYPE<-ifelse(as.character(dataPropCropDmgTmp$EVTYPE) %in% 
    c("THUNDERSTORM","THUNDERSTORM  WINDS","THUNDERSTORM WIND",
      "THUNDERSTORM WIND (G40)","THUNDERSTORM WIND G52",
      "THUNDERSTORM WINDS","THUNDERSTORM WINDS 13",
      "THUNDERSTORM WINDS/HAIL","THUNDERSTORM WINDSS",
      "THUNDERSTORMS WINDS","THUNDERSTORMW","THUNDERTORM WINDS",
      "LIGHTNING AND THUNDERSTORM WIN","TSTM WIND","TSTM WIND (G35)",
      "TSTM WIND (G40)","TSTM WIND (G45)","TSTM WIND/HAIL"),
    "THUNDERSTORM",dataPropCropDmgTmp$EVTYPE)
dataPropCropDmgTmp$EVTYPE<-ifelse(as.character(dataPropCropDmgTmp$EVTYPE) %in% 
    c("HEAT","HEAT WAVE","HEAT WAVES","EXCESSIVE HEAT", "EXTREME HEAT"),
    "HEAT",dataPropCropDmgTmp$EVTYPE)
dataPropCropDmgTmp$EVTYPE<-ifelse(as.character(dataPropCropDmgTmp$EVTYPE) %in% 
    c("HIGH WIND","HIGH WIND 48","HIGH WIND AND SEAS","HIGH WIND/SEAS",
      "HIGH WINDS","HIGH WINDS/COLD","HIGH WINDS/SNOW",
      "HIGH WIND/HEAVY SNOW"),"HIGH WIND",dataPropCropDmgTmp$EVTYPE)
dataPropCropDmgTmp$EVTYPE<-ifelse(as.character(dataPropCropDmgTmp$EVTYPE) %in% 
    c("GLAZE/ICE STORM","ICE STORM","ICE STORM/FLASH FLOOD",
      "WINTER STORM","WINTER STORM HIGH WINDS","WINTER STORMS"),"WINTER STORM",
    dataPropCropDmgTmp$EVTYPE)
dataPropCropDmgTmp$EVTYPE<-ifelse(as.character(dataPropCropDmgTmp$EVTYPE) %in% 
    c("WILD FIRES","WILD/FOREST FIRE","WILDFIRE" ),"WILDFIRE",
    dataPropCropDmgTmp$EVTYPE)
dataPropCropDmgTmp$EVTYPE<-ifelse(as.character(dataPropCropDmgTmp$EVTYPE) %in% 
    c("HURRICANE","HURRICANE EDOUARD","HURRICANE EMILY","HURRICANE ERIN",
      "HURRICANE FELIX","HURRICANE OPAL","HURRICANE OPAL/HIGH WINDS",
      "HURRICANE/TYPHOON","TROPICAL STORM", "TROPICAL STORM GORDON",
      "TYPHOON"),"HURRICANE/TYPHOON",dataPropCropDmgTmp$EVTYPE)
dataPropCropDmgTmp$EVTYPE<-ifelse(as.character(dataPropCropDmgTmp$EVTYPE) %in% 
    c("LIGHTNING","LIGHTNING INJURY","LIGHTNING."),
    "LIGHTNING",dataPropCropDmgTmp$EVTYPE)
dataPropCropDmgTmp$EVTYPE<-ifelse(as.character(dataPropCropDmgTmp$EVTYPE) %in% 
    c("ICE ON ROAD","ICE ROADS","ICY ROADS"),
    "ICY ROADS",dataPropCropDmgTmp$EVTYPE)

dataPropCropDmgTmp$TOTALLOSS<-
  dataPropCropDmgTmp$PROPDMG+dataPropCropDmgTmp$CROPDMG

#extract list of top ten events that have the most impact
dataPropCropDmgTmp$EVTYPE<-factor(dataPropCropDmgTmp$EVTYPE)
dataPropCropDmgTmp1<-as.data.frame(dataPropCropDmgTmp %>% group_by(EVTYPE) 
                                   %>% summarise(sum(TOTALLOSS)))
dataPropCropDmgTmp8<-arrange(dataPropCropDmgTmp1, 
                             desc(dataPropCropDmgTmp1$`sum(TOTALLOSS)`))
dataPropCropDmgTmp8<-dataPropCropDmgTmp8[1:10,]

#store the labels in a variable 
eventPropCrop<-as.character(dataPropCropDmgTmp8[,"EVTYPE"])


dataPropCropDmgTmp$EVTYPE<-ifelse(as.character(dataPropCropDmgTmp$EVTYPE) %in% 
                eventPropCrop,dataPropCropDmgTmp$EVTYPE,"OTHER")

dataPropCropDmgTmp$EVTYPE<-factor(dataPropCropDmgTmp$EVTYPE)
dataPropCropDmgTmpLevels<-levels(dataPropCropDmgTmp$EVTYPE)

dataPropCropDmgTmp$TOTALLOSS<-
  dataPropCropDmgTmp$PROPDMG+dataPropCropDmgTmp$CROPDMG
dataPropCropDmgTmp2<-as.data.frame(dataPropCropDmgTmp 
      %>% group_by(EVTYPE) %>% summarise(sum(TOTALLOSS)))
dataPropCropDmgTmp3<-as.data.frame(dataPropCropDmgTmp 
      %>% group_by(EVTYPE) %>% summarise(sum(PROPDMG)))
dataPropCropDmgTmp4<-as.data.frame(dataPropCropDmgTmp 
      %>% group_by(EVTYPE) %>% summarise(sum(CROPDMG)))
dataPropCropDmgTmp5<-arrange(dataPropCropDmgTmp2,
                             desc(dataPropCropDmgTmp2$`sum(TOTALLOSS)`))
names(dataPropCropDmgTmp2)<-c("EVENT","TOTAL_LOSS")
names(dataPropCropDmgTmp3)<-c("EVENT","PROPERTY_DAMAGE")
names(dataPropCropDmgTmp4)<-c("EVENT","CROP_DAMAGE")
dataPropCropDmgTmp2<-merge(dataPropCropDmgTmp2,dataPropCropDmgTmp3,by="EVENT")
dataPropCropDmgTmp2<-merge(dataPropCropDmgTmp2,dataPropCropDmgTmp4,by="EVENT")
dataPropCropDmgTmp5<-
  dataPropCropDmgTmp2[which(dataPropCropDmgTmp2$EVENT!="OTHER"),]
dataPropCropDmgTmp6<-
  dataPropCropDmgTmp2[which(dataPropCropDmgTmp2$EVENT=="OTHER"),]
dataPropCropDmgTmp2<-
  arrange(dataPropCropDmgTmp5,desc(dataPropCropDmgTmp5$TOTAL_LOSS))
dataPropCropDmg<-
  rbind(dataPropCropDmgTmp2,dataPropCropDmgTmp6)
totaldamage<-sum(dataPropCropDmg$TOTAL_LOSS)
dataPropCropDmg$PERCENTAGE<-
  round((dataPropCropDmg$TOTAL_LOSS/totaldamage)*100,digits=2)

print(dataPropCropDmg)
