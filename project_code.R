##set the environment
library(readr)
library(dplyr)
library(lubridate)
library(lattice)
##download the data
if(!file.exists("./data")){dir.create("./data")}
url1<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
url2<-"https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf"
download.file(url1,"./data/StormData.csv.bz2",method="curl")
download.file(url2,"./data/StormDataVar.pdf",method="curl")
storm_data<-read_csv("./data/StormData.csv.bz2")
storm_data<-storm_data %>% select(c(2,7:8,23:28)) %>% filter(PROPDMG>0|CROPDMG>0|
       FATALITIES>0|INJURIES>0)

storm_data$BGN_DATE<-mdy_hms(storm_data$BGN_DATE)

storm_data$PROPDMG<-paste(storm_data$PROPDMG,storm_data$PROPDMGEXP,sep="")
storm_data$PROPDMG<-replace(storm_data$PROPDMG,is.na(storm_data$PROPDMG),0)

storm_data$PROPDMG<-suppressWarnings(as.numeric(gsub("[Kk]","e3",
                                gsub("[Mm]","e6",
                                gsub("[Bb]","e9",storm_data$PROPDMG)))))    

storm_data$CROPDMG<- paste(storm_data$CROPDMG,storm_data$CROPDMGEXP,sep="")
storm_data$CROPDMG<-replace(storm_data$CROPDMG,is.na(storm_data$CROPDMG),0)
storm_data$CROPDMG<- suppressWarnings(as.numeric(gsub("[Kk]","e3",
                                gsub("[Mm]","e6",
                                gsub("[Bb]","e9",
                                    storm_data$CROPDMG)))))    
storm_data$CROPDMG<-replace(storm_data$CROPDMG,is.na(storm_data$CROPDMG),0)
storm_data$PROPDMG<-replace(storm_data$PROPDMG,is.na(storm_data$PROPDMG),0)

storm_data$PROPDMG<-replace(storm_data$PROPDMG,is.na(storm_data$PROPDMG),0)
storm_data$CROPDMG<-replace(storm_data$CROPDMG,is.na(storm_data$CROPDMG),0)
storm_data<-subset(storm_data,select=(-c(PROPDMGEXP,CROPDMGEXP)))
storm_data2<-storm_data%>%mutate(total_DMG=PROPDMG+CROPDMG,year=year(BGN_DATE))%>%
        rename("event_type"="EVTYPE","date"= "BGN_DATE")%>%
        group_by(year,event_type)%>%
        summarise(tot_fat_year=sum(FATALITIES),tot_inj_year=sum(INJURIES),
                  total_cost_year=(sum(total_DMG)/1000000000))

top1DMG<-quantile(storm_data2$total_cost_year,prob=.90)
top1FAT<-quantile(storm_data2$tot_fat_year,prob=.90)
top1INJ<-quantile(storm_data2$tot_inj_year,prob=.90)


storm_data_health<-storm_data2%>%filter(tot_fat_year>top1FAT|tot_inj_year>top1INJ)
storm_data_cost<-storm_data2%>%filter(total_cost_year>top1DMG)

storm_data_health$event_type<-
gsub("HEAVY SNOW|ICE STORM|BLIZZARD|EXTREME COLD|COLD/WIND CHILL|GLAZE",
     "WINTER STORM",
gsub("HIGH WIND|THUNDERSTORM WINDS|LIGHTNING|TSTM WIND|THUNDERSTORM WIND|HAIL",
                                     "THUNDERSTORM", 
gsub("TROPICAL STORM|HURRICANE/TYPHOON|HURRICANE","HURRICANE/TS",
gsub("EXTREME HEAT|EXCESSIVE HEAT|HEAT WAVE", "HEAT",
gsub("FLASH FLOOD|FLOOD", "FLOODING",
gsub("RIP CURRENTS","RIP CURRENT",storm_data_health$event_type))))))

storm_data_cost$event_type<-gsub("HURRICANE","HURRICANE/TS",
                                 storm_data_cost$event_type)
storm_data_cost$event_type<-
gsub("HEAVY SNOW|ICE STORM|BLIZZARD|EXTREME COLD|COLD/WIND CHILL|GLAZE",
     "WINTER STORM",
gsub("TROPICAL STORM|HURRICANE/TS/TYPHOON|HURRICANE OPAL|HURRICANE/TS OPAL",
     "HURRICANE/TS",
gsub("EXTREME HEAT|EXCESSIVE HEAT|HEAT WAVE", 
     "HEAT",
gsub("FLASH FLOOD|FLOOD|RIVER FLOOD|STORM SURGE|STORM SURGE/TIDE",
     "FLOODING",
gsub("RIP CURRENTS","RIP CURRENT",storm_data_cost$event_type)))))

storm_data_cost$event_type<-gsub("TYPHOON","HURRICANE/TS",
                                 storm_data_cost$event_type)
storm_data_cost$event_type<-gsub("TORNADOES, TSTM WIND, HAIL","TORNADO",
                                 storm_data_cost$event_type)
                                 
storm_data_cost$event_type<-
        gsub("HIGH WIND|THUNDERSTORM WINDS|LIGHTNING|TSTM WIND|THUNDERSTORM WIND|HAIL",
     "THUNDERSTORM",storm_data_cost$event_type)
storm_data_cost$event_type<-
        gsub("SEVERE THUNDERSTORM|THUNDERSTORMS",
             "THUNDERSTORM",storm_data_cost$event_type)
storm_data_cost$event_type<-
        gsub("/SEVERE WEATHER$","",storm_data_cost$event_type)
storm_data_cost$event_type<-
        gsub("/FOREST ","",storm_data_cost$event_type)
storm_data_cost$event_type<-
        gsub("WILDFIRE","WILD FIRES",storm_data_cost$event_type)

total_fat_by_event<-storm_data_health%>%group_by(event_type)%>%
        summarise(total=sum(tot_fat_year))%>%arrange(desc(total))
average_fat_year<-storm_data_health%>%group_by(event_type)%>%
        summarise(average=mean(tot_fat_year))%>%arrange(desc(average))
total_inj_by_event<-storm_data_health%>%group_by(event_type)%>%
        summarise(total=sum(tot_inj_year))%>%arrange(desc(total))
average_inj_year<-storm_data_health%>%group_by(event_type)%>%
        summarise(average=mean(tot_inj_year))%>%arrange(desc(average))

total_cost_by_event<-storm_data_cost%>%group_by(event_type)%>%
        summarise(total=sum(total_cost_year))%>%arrange(desc(total))
average_cost_year<-storm_data_cost%>%group_by(event_type)%>%
        summarise(average=mean(total_cost_year))%>%arrange(desc(average))

results_health<-merge(merge(total_fat_by_event,total_inj_by_event,1),
                      merge(average_fat_year,average_inj_year,1),1)
colnames(results_health)<-c("event","total_fatalities", "total_injuries",
                            "average_fatalities_year","average_injuries_year")
results_health<-arrange(results_health,desc(average_fatalities_year))

results_cost<-merge(total_cost_by_event,average_cost_year,1)
colnames(results_cost)<-c("event","total_cost", "average_cost_year")
results_cost<-arrange(results_cost,desc(average_cost_year))

png(file="plot1.png")
xyplot(tot_fat_year~year|event_type,data=storm_data_health,
       xlab = "Year 1950-2011",ylab = "Number of fatalites", 
       main = "Fatalities per Year for each Event Type")
dev.off()

png(file="plot2.png")
xyplot(tot_inj_year~year|event_type,data=storm_data_health,
       xlab = "Year 1950-2011",ylab = "Number of injuries", 
       main = "Injuries per Year for each Event Type")
dev.off()

png(file="plot3.png")

xyplot(total_cost_year~year|event_type,data=storm_data_cost,
       xlab = "Year 1950-2011",ylab = "Cost of Damage in Billions of $", 
       main = "Billions in Damages per Year for each Event Type")
dev.off()

