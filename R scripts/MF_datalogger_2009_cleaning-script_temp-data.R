#Mason Farm datalogger 2009 prelim plots

#load libraries

library(readr)
library(ggplot2)
library(tidyr)
library(Rmisc)
library(dplyr)
library(viridis)
library(cowplot)
library(extrafont)


#---------------------

#load data

plc <- read_csv("data files/MF_datalogger_2009_plant_clean_test.csv")
View(plc)

#add month to whole data frame
plc$month<-ifelse(plc$date.time.j<152, "may",
                  ifelse(plc$date.time.j>=152 & plc$date.time.j<182, "june",
                         ifelse(plc$date.time.j>=182 & plc$date.time.j<213, "july",
                                ifelse(plc$date.time.j>=213 & plc$date.time.j<244, "august",
                                       ifelse(plc$date.time.j>=244 & plc$date.time.j<274, "september", "october")))))



#remove may from data--temps most likely from lab
plc<-subset(plc, month!="may")

#----------------------

#Finding averages at each time point for plotting

modelsum<-summarySE(plc, measurevar = "temp",
                    groupvars = c("date.time.j", "loc", "pos"),
                    na.rm = TRUE)


modsum.plot<-ggplot(modelsum, aes(x=date.time.j, y=temp, group=interaction(loc, pos), color=loc))
modsum.plot+geom_line(
)+facet_wrap(~pos, dir="v")


#-----------------------

#Finding mean and standard deviation for all models by timepoint

tpsum<-summarySE(plc, measurevar = "temp",
                 groupvars = "date.time.j",
                 na.rm = TRUE)
head(tpsum)

#create columns with mean +/- 2 standard deviations
tpsum$sd2<-(tpsum$sd * 2)
tpsum$mn.sd2.pos<-(tpsum$temp+tpsum$sd2)
tpsum$mn.sd2.neg<-tpsum$temp-tpsum$sd2


#create dataframe of the 2 sd with columns named "temp" so I can add it to plot
tpsum.sd2.pos<-tpsum[, c("mn.sd2.pos", "date.time.j")]
tpsum.sd2.pos<-rename(tpsum.sd2.pos, temp=mn.sd2.pos)

tpsum.sd2.neg<-tpsum[, c("mn.sd2.neg", "date.time.j")]
tpsum.sd2.neg<-rename(tpsum.sd2.neg, temp=mn.sd2.neg)

#plot mean and 2 standard deviations above and below
mnsd.plot<-ggplot(tpsum, aes(x=date.time.j, y=temp))
mnsd.plot+geom_line(color="black"
)+geom_line(data=tpsum.sd2.neg, aes(x=date.time.j, y=temp),
            color="blue"
)+geom_line(data=tpsum.sd2.pos, aes(x=date.time.j, y=temp),
            color="red")

#Dividing into smaller chunks of time so data can be examined more closely

range(plc$date.time.j)



#mean data frame
tpsum$month<-ifelse(tpsum$date.time.j<152, "may",
                    ifelse(tpsum$date.time.j>=152 & tpsum$date.time.j<182, "june",
                           ifelse(tpsum$date.time.j>=182 & tpsum$date.time.j<213, "july",
                                  ifelse(tpsum$date.time.j>=213 & tpsum$date.time.j<244, "august",
                                         ifelse(tpsum$date.time.j>=244 & tpsum$date.time.j<274, "september", "october")))))


View(tpsum)


#create dataframe of the 2 sd with columns named "temp" so I can add it to plot
tpsum.sd2.pos<-tpsum[, c("mn.sd2.pos", "date.time.j", "month")]
tpsum.sd2.pos<-rename(tpsum.sd2.pos, temp=mn.sd2.pos)

tpsum.sd2.neg<-tpsum[, c("mn.sd2.neg", "date.time.j", "month")]
tpsum.sd2.neg<-rename(tpsum.sd2.neg, temp=mn.sd2.neg)



#creating data frames that only have data from each month

may<-subset(tpsum, month=="may")
may.sd2.pos<-subset(tpsum.sd2.pos, month=="may")
may.sd2.neg<-subset(tpsum.sd2.neg, month=="may")

june<-subset(tpsum, month=="june")
june.sd2.pos<-subset(tpsum.sd2.pos, month=="june")
june.sd2.neg<-subset(tpsum.sd2.neg, month=="june")

july<-subset(tpsum, month=="july")
july.sd2.pos<-subset(tpsum.sd2.pos, month=="july")
july.sd2.neg<-subset(tpsum.sd2.neg, month=="july")

august<-subset(tpsum, month=="august")
august.sd2.pos<-subset(tpsum.sd2.pos, month=="august")
august.sd2.neg<-subset(tpsum.sd2.neg, month=="august")

september<-subset(tpsum, month=="september")
september.sd2.pos<-subset(tpsum.sd2.pos, month=="september")
september.sd2.neg<-subset(tpsum.sd2.neg, month=="september")

october<-subset(tpsum, month=="october" & date.time.j>=274)
october.sd2.pos<-subset(tpsum.sd2.pos, month=="october" & date.time.j>=274)
october.sd2.neg<-subset(tpsum.sd2.neg, month=="october" & date.time.j>=274)



#PLOTTING BY MONTH

#MAY
may.plot<-ggplot(may, aes(x=date.time.j, y=temp))
may.plot+geom_line(color="black"
)+geom_line(data=may.sd2.neg, aes(x=date.time.j, y=temp),
            color="blue"
)+geom_line(data=may.sd2.pos, aes(x=date.time.j, y=temp),
            color="red"
)+labs(title="MAY")


#june
june.plot<-ggplot(june, aes(x=date.time.j, y=temp))
june.plot+geom_line(color="black"
)+geom_line(data=june.sd2.neg, aes(x=date.time.j, y=temp),
            color="blue"
)+geom_line(data=june.sd2.pos, aes(x=date.time.j, y=temp),
            color="red"
)+labs(title="JUNE")


#july
july.plot<-ggplot(july, aes(x=date.time.j, y=temp))
july.plot+geom_line(color="black"
)+geom_line(data=july.sd2.neg, aes(x=date.time.j, y=temp),
            color="blue"
)+geom_line(data=july.sd2.pos, aes(x=date.time.j, y=temp),
            color="red"
)+labs(title="JULY")


#august
august.plot<-ggplot(august, aes(x=date.time.j, y=temp))
august.plot+geom_line(color="black"
)+geom_line(data=august.sd2.neg, aes(x=date.time.j, y=temp),
            color="blue"
)+geom_line(data=august.sd2.pos, aes(x=date.time.j, y=temp),
            color="red"
)+labs(title="AUGUST")


#september
september.plot<-ggplot(september, aes(x=date.time.j, y=temp))
september.plot+geom_line(color="black"
)+geom_line(data=september.sd2.neg, aes(x=date.time.j, y=temp),
            color="blue"
)+geom_line(data=september.sd2.pos, aes(x=date.time.j, y=temp),
            color="red"
)+labs(title="SEPTEMBER")


#october
october.plot<-ggplot(october, aes(x=date.time.j, y=temp))
october.plot+geom_line(color="black"
)+geom_line(data=october.sd2.neg, aes(x=date.time.j, y=temp),
            color="blue"
)+geom_line(data=october.sd2.pos, aes(x=date.time.j, y=temp),
            color="red"
)+labs(title="OCTOBER")



#-------------------

#Finding daily mean temperatures

daymn.sum<-summarySE(plc, measurevar = "temp",
                     groupvars = "date.j",
                     na.rm = TRUE)


daymn.plot<-ggplot(daymn.sum, aes(x=date.j, y=temp))
daymn.plot+geom_line(
)+labs(y="daily mean temp")


#subset by month

june.all<-subset(plc, month=="june")
june.daymn.sum<-summarySE(june.all, measurevar = "temp",
                          groupvars = "date.j",
                          na.rm = TRUE)

june.daymn.plot<-ggplot(june.daymn.sum, aes(x=date.j, y=temp))
june.daymn.plot+geom_line()


july.all<-subset(plc, month=="july")
july.daymn.sum<-summarySE(july.all, measurevar = "temp",
                          groupvars = "date.j",
                          na.rm = TRUE)

july.daymn.plot<-ggplot(july.daymn.sum, aes(x=date.j, y=temp))
july.daymn.plot+geom_line()


august.all<-subset(plc, month=="august")
august.daymn.sum<-summarySE(august.all, measurevar = "temp",
                            groupvars = "date.j",
                            na.rm = TRUE)

august.daymn.plot<-ggplot(august.daymn.sum, aes(x=date.j, y=temp))
august.daymn.plot+geom_line()


september.all<-subset(plc, month=="september")
september.daymn.sum<-summarySE(september.all, measurevar = "temp",
                               groupvars = "date.j",
                               na.rm = TRUE)

september.daymn.plot<-ggplot(september.daymn.sum, aes(x=date.j, y=temp))
september.daymn.plot+geom_line()


october.all<-subset(plc, month=="october")
october.daymn.sum<-summarySE(october.all, measurevar = "temp",
                             groupvars = "date.j",
                             na.rm = TRUE)

october.daymn.plot<-ggplot(october.daymn.sum, aes(x=date.j, y=temp))
october.daymn.plot+geom_line()


#--------------------

#Find daily maximum mean--high mean temp for each day
  ##also highest SD for each day


#copy date.time.j column so that I can separate into just date.j
tpsum$date.sep<-tpsum$date.time.j

#convert date.sep into character class
tpsum$date.sep<-as.character(tpsum$date.sep)

#separate date and time
tpsum <-tpsum %>% separate(date.sep, into = c("date", "time"))
tpsum$time[is.na(tpsum$time)]<-0

#convert date back to numeric
tpsum$date<-as.numeric(tpsum$date)

#find the highest model mean temperature for each day (grouped by date, selects top row in each group)
dmaxmn <- tpsum %>% group_by(date) %>% top_n(1, temp)

#find the higest model SD2
dsdmn <- tpsum %>% group_by(date) %>% top_n(1, sd2)

#dsdmn has 1 less row for someunknown reason--giving it a dummy row to make it match dmaxmn
#dsdmn[131,]<-NA

#make column in daily max mean data set with daily max SD2
dmaxmn$max.sd2<-dsdmn$sd2


#plot daily max SD2 against daily max means

sdmnmax.plot<-ggplot(dmaxmn, aes(x=temp, y=max.sd2))
sdmnmax.plot+geom_point(
)+geom_smooth(method=lm)


#Plot daily max means against date

maxmndate.plot<-ggplot(dmaxmn, aes(x=date, y=temp))
maxmndate.plot+geom_point(
)+geom_line(
)+labs(y="daily max mean temp")

#plot daily max sd2 against date

maxsddate.plot<-ggplot(dmaxmn, aes(x=date, y=max.sd2))
maxsddate.plot+geom_point(
)+geom_line()






