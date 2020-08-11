library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(leaflet)
library(chron)
library(data.table)

##Read the data. I need to use two data sets to generate reports in this situation.
## This step can be automated by httr request and pulling data from API.
d1<- read.csv(file.choose(),header = T)       #Delivery drivers data
d2<- read.csv(file.choose(),header =T)        #Orders data
adata <- d1
odata <- d2

################################## DATA-CLEANING ###################################
## Standard names you want to eliminate from your data. This could be because of variuos reasons as
## testing, wrong data, blah blah blah

adata<-adata[adata$Name!="name3",]
adata<-adata[adata$Name!="name2",]
adata<-adata[adata$Name!="name1",]

adata<-adata[adata$Revenue!=0,]             #Considering only individuals who have worked actually
adata<- adata[adata$Drop.completed!=0,]     #Same continues

adata <- adata %>% select(-Agent.ID,-Pick.up.failed,-Drop.off.failed,-First.pick.up.completed,-Last.drop.off.completed)
adata <- adata %>% select(-Delivery.Charges.Collected,-Collection.Amount.Collected,-Prepaid.Delivery.Charges)
adata <- adata %>% select(Date,Name, Drop.completed,Rush,Xpress,Standard,Revenue,Declined,On.duty.time,
                    First.on.duty,Last.off.duty,Distance,Week.Off,Work.Timings)

#################################### Date-time manipulations####################################
adata$Date <- dmy(adata$Date)
adata$Name <- toupper(adata$Name)
adata$First.on.duty <- chron(times=adata$First.on.duty)
adata$Last.off.duty <- chron(times=adata$Last.off.duty)
adata$dutyon <- adata$First.on.duty*1440
adata$dutyoff <- adata$Last.off.duty*1440
adata$dutytime <- adata$dutyoff-adata$dutyon

adata <- separate(adata,col=Work.Timings, into = c("shiftstart","shiftend"),sep = " to ")
adata$shiftstart<-as.ITime(as.POSIXct(adata$shiftstart,format="%H:%M"))
adata$shiftstart <- as.numeric(times(adata$shiftstart))/60
adata$shiftend<-as.ITime(as.POSIXct(adata$shiftend,format="%H:%M"))
adata$shiftend <- as.numeric(times(adata$shiftend))/60
adata$dutyon <- as.numeric(adata$dutyon)
adata$dutyoff <- as.numeric(adata$dutyoff)

#on-adherence: To check login punctuality
#off-adherence : To check logoff punctuality
#dutytime_adh : Actual duty time / Supposed duty time .... Percentage

adata$on_adherence <- adata$shiftstart-adata$dutyon     
adata$off_adherence <- adata$dutyoff-adata$shiftend     
adata$dutytime_adh <- as.numeric(times(as.ITime(adata$On.duty.time)))/36000

#He/she is eligible for full attendance only if duty_adherence is >80%, half attendance 
#when working hours are 55-80%

adata$Present <- ifelse(weekdays(as.Date(adata$Date))==adata$Week.Off,"Not eligible",
                ifelse(adata$dutytime_adh>=0.8,1,
                  ifelse(adata$dutytime_adh>=0.55 & adata$dutytime_adh<0.8,0.5,0)))
adata$Agent.hours <- adata$dutytime_adh*10


odata <- odata %>% select(ORDER.ID,ORDER.URGENCY.TYPE,DISTANCE,ORDER.STATUS,
                    CREATED.DATE,CREATED.TIME,REQUESTED.TIME,
                    PICK.UP.COMPLETED.DATE,PICK.UP.COMPLETED.TIME,
                    DROP.COMPLETED.DATE,DROP.COMPLETED.TIME,PICK.ADDRESS,DROP.ADDRESS,
                    USER.NAME,CANCELLATION.REASON,AGENT.NAME)
#
# Date-time manipulations
odata$CREATED.DATE <-dmy(odata$CREATED.DATE)
odata$CREATED<- paste(odata$CREATED.DATE,odata$CREATED.TIME)
odata$CREATED<-ymd_hms(odata$CREATED)

odata$PICK.UP.COMPLETED.DATE<-dmy(odata$PICK.UP.COMPLETED.DATE)
odata$PICKED <- paste(odata$PICK.UP.COMPLETED.DATE,odata$PICK.UP.COMPLETED.TIME)
odata$PICKED<- ymd_hms(odata$PICKED)

odata$DROP.COMPLETED.DATE<-dmy(odata$DROP.COMPLETED.DATE)
odata$DELIVERED <- paste(odata$DROP.COMPLETED.DATE,odata$DROP.COMPLETED.TIME)
odata$DELIVERED <- ymd_hms(odata$DELIVERED)

odata$REQUESTED.TIME <- dmy_hms(odata$REQUESTED.TIME)

odata <- odata %>% select(CREATED.DATE,ORDER.ID,ORDER.URGENCY.TYPE,DISTANCE,ORDER.STATUS,
                    CREATED,REQUESTED.TIME,
                    PICKED,DELIVERED,PICK.ADDRESS,DROP.ADDRESS,
                    USER.NAME,CANCELLATION.REASON,AGENT.NAME)
odata$FULFILLMENT.TIME<-odata$DELIVERED-odata$REQUESTED.TIME

odata$REACH.TIME <- ifelse(as.Date(odata$CREATED)==as.Date(odata$PICKED),
                           (odata$PICKED-odata$REQUESTED.TIME),NA)

odata$BREACH <- ifelse(as.Date(odata$CREATED)==as.Date(odata$PICKED), 
  ifelse(odata$ORDER.URGENCY.TYPE=="RUSH",ifelse(odata$FULFILLMENT.TIME>120,1,0),
                    ifelse(odata$ORDER.URGENCY.TYPE=="XPRESS",ifelse(odata$FULFILLMENT.TIME>240,1,0),0)),NA)
odata$DELAY <- ifelse(odata$ORDER.URGENCY.TYPE!="STANDARD",
                       ifelse(as.Date(odata$DELIVERED)>as.Date(odata$REQUESTED),1,0),
                        ifelse(as.Date(odata$DELIVERED)>(as.Date(odata$REQUESTED)+2),1,0))



##################################### DAY WISE SUMMARY (ORDERS)########################################
crt <- odata %>% group_by(CREATED.DATE,ORDER.URGENCY.TYPE) %>% 
                summarize(CREATED = n(),BREACH=sum(BREACH,na.rm=T),
                          DELAY=sum(DELAY,na.rm = T),REACH =round(mean(REACH.TIME,na.rm=T)))
can <- odata %>% filter(ORDER.STATUS=="CANCELLED") %>% group_by(CREATED.DATE,ORDER.URGENCY.TYPE) %>%
                            summarize(CANCELLED=n())
del <- odata %>% filter(ORDER.STATUS=="DROPCOMPLETED") %>% group_by(CREATED.DATE,ORDER.URGENCY.TYPE) %>% 
                       summarize(DELIVERED=n())

ordersdaywise <- merge(crt,can,by=c("CREATED.DATE","ORDER.URGENCY.TYPE"))
ordersdaywise <- merge(ordersdaywise,del,by=c("CREATED.DATE","ORDER.URGENCY.TYPE"))

ordersdaywise <- ordersdaywise %>% select(CREATED.DATE,ORDER.URGENCY.TYPE,CREATED,
                                          DELIVERED,CANCELLED,DELAY,BREACH,REACH)
rush_data <- odata %>% filter(odata$ORDER.URGENCY.TYPE=="RUSH") %>% group_by(CREATED.DATE)
xpress_data <-odata %>% filter(odata$ORDER.URGENCY.TYPE=="XPRESS") %>% group_by(CREATED.DATE)

demand_trend <- dcast(ordersdaywise,CREATED.DATE~ORDER.URGENCY.TYPE,fun.aggregate=CREATED)

###########################################Daywise summary (AGENTS)############################
agentdaywise <- adata %>% group_by(Date) %>%
                    summarize(o_a=sum(Drop.completed)/n(),
                                r_a=sum(Revenue)/n(),
                                TotalDrops=sum(Drop.completed),
                                Rush=sum(Rush),
                                Xpr=sum(Xpress),
                                Stand=sum(Standard),
                                Revenue=sum(Revenue),
                                FLEET = sum(Present,na.rm=T),
                                UNITS = sum(Agent.hours,na.rm = T))



########################################## AGENT DISCIPLINE ################################

latelogin_agents <- adata%>% filter(on_adherence<=-30) %>% 
                      select(Date,Name,Drop.completed,Declined,Revenue,dutytime_adh,
                             on_adherence,off_adherence)



earlylogoff_agents <- adata %>% filter(off_adherence<=-30) %>% 
                      select(Date,Name,Drop.completed,Declined,Revenue,dutytime_adh,
                             on_adherence,off_adherence)                      











