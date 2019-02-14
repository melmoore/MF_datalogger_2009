#Mason Farm datalogger 2009 prelim plots

#load libraries

library(readr)
library(ggplot2)
library(tidyr)
library(Rmisc)
library(dplyr)
library(viridis)
library(cowplot)
library(forecast)

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
                    groupvars = c("date.time.j", "pos"),
                    na.rm = TRUE)


modsum.plot<-ggplot(modelsum, aes(x=date.time.j, y=temp, group=pos, color=pos))
modsum.plot+geom_line()


#Adding month column, to subset into smaller chunks for easier viewing
modelsum$month<-ifelse(modelsum$date.time.j<152, "may",
                       ifelse(modelsum$date.time.j>=152 & modelsum$date.time.j<182, "june",
                              ifelse(modelsum$date.time.j>=182 & modelsum$date.time.j<213, "july",
                                     ifelse(modelsum$date.time.j>=213 & modelsum$date.time.j<244, "august",
                                            ifelse(modelsum$date.time.j>=244 & modelsum$date.time.j<274, "september", "october")))))

View(modelsum)


#creating a 2SD column, then adding and subtracting from mean 
modelsum$sd2<-modelsum$sd*2

modelsum$sd2.pos<-modelsum$sd2 + modelsum$temp
modelsum$sd2.neg<-modelsum$temp - modelsum$sd2

#subsetting to summer (june-august) and month for easier viewing 
june.mod<-subset(modelsum, month=="june")
july.mod<-subset(modelsum, month=="july")
august.mod<-subset(modelsum, month=="august")
smr.mod<-subset(modelsum, month!="september" & month!="october")



#plotting summer with mean temp and 2SD for each position

#creating dataframes with just +/-2SD to plot as separate lines
smrpos.sd2.pos<-smr.mod[,c("date.time.j", "pos", "sd2.pos")]
smrpos.sd2.neg<-smr.mod[,c("date.time.j", "pos", "sd2.neg")]

#renaming sd2 columns to "temp" so it plots with aesthetics
smrpos.sd2.pos<-rename(smrpos.sd2.pos, temp=sd2.pos)
smrpos.sd2.neg<-rename(smrpos.sd2.neg, temp=sd2.neg)


smrpos.mnsd.plot<-ggplot(smr.mod, aes(x=date.time.j, y=temp))
smrpos.mnsd.plot+geom_line(color="black"
)+geom_line(data=smrpos.sd2.neg, aes(x=date.time.j, y=temp),
            color="blue"
)+geom_line(data=smrpos.sd2.pos, aes(x=date.time.j, y=temp),
            color="red"
)+facet_wrap(~pos, dir="v")


#------------------------

#looking at maximum mean temp per day by maximum 2SD per day, by position on plant to see if it differs

#Find daily maximum mean--high mean temp for each day
##also highest SD for each day

#copy date.time.j column so that I can separate into just date.j
smr.mod$date.sep<-smr.mod$date.time.j

#convert date.sep into character class
smr.mod$date.sep<-as.character(smr.mod$date.sep)

#separate date and time
smr.mod <-smr.mod %>% separate(date.sep, into = c("date", "time"))
smr.mod$time[is.na(smr.mod$time)]<-0

#convert date back to numeric
smr.mod$date<-as.numeric(smr.mod$date)


#find the highest model mean temperature for each day (grouped by date, selects top row in each group)
dmaxmn.pos <- smr.mod %>% group_by(date, pos) %>% top_n(1, temp)

#find the higest model SD2
dsdmn.pos <- smr.mod %>% group_by(date, pos) %>% top_n(1, sd2)

#dsdmn has 1 less row for someunknown reason--giving it a dummy row to make it match dmaxmn
dsdmn.pos[277,]<-NA

#make column in daily max mean data set with daily max SD2
dmaxmn.pos$max.sd2<-dsdmn.pos$sd2


#plot daily max SD2 against daily max means

smrpos.sdmnmax.plot<-ggplot(dmaxmn.pos, aes(x=temp, y=max.sd2, group=pos, color=pos))
smrpos.sdmnmax.plot+geom_point(
)+geom_smooth(method=lm)


#plotting time of day of daily max mean temp for different positions
dmaxmn.pos$time<-as.numeric(dmaxmn.pos$time)

#exclude timepoints with daily max temp at 0 (these don't really make sense--error?)
dmaxmn.pos.ex<-subset(dmaxmn.pos, time>.25)

tod.plot<-ggplot(dmaxmn.pos.ex, aes(x=time, y=temp, group=pos, color=pos))
tod.plot+geom_point(
)+geom_smooth(method=lm, se=FALSE)


#trying a boxplot
tod.boxplot<-ggplot(dmaxmn.pos.ex, aes(x=pos, y=time, fill=pos))
tod.boxplot+geom_boxplot()


#plotting daily max mean against date
dmaxmn.date.plot<-ggplot(dmaxmn.pos, aes(x=date, y=temp, group=pos, color=pos))
dmaxmn.date.plot+geom_point(
)+geom_line()


#plotting daily max SD against date
dmaxsd.date.plot<-ggplot(dsdmn.pos, aes(x=date, y=sd2, group=pos, color=pos))
dmaxsd.date.plot+geom_point(
)+geom_line()



#-----------------------

#Finding mean and standard deviation for all models by timepoint

tpsum<-summarySE(plc, measurevar = "temp",
                 groupvars = "date.time.j",
                 na.rm = TRUE)
head(tpsum)

#Dividing into smaller chunks of time so data can be examined more closely
tpsum$month<-ifelse(tpsum$date.time.j<152, "may",
                    ifelse(tpsum$date.time.j>=152 & tpsum$date.time.j<182, "june",
                           ifelse(tpsum$date.time.j>=182 & tpsum$date.time.j<213, "july",
                                  ifelse(tpsum$date.time.j>=213 & tpsum$date.time.j<244, "august",
                                         ifelse(tpsum$date.time.j>=244 & tpsum$date.time.j<274, "september", "october")))))


View(tpsum)


#create columns with mean +/- 2 standard deviations
tpsum$sd2<-(tpsum$sd * 2)
tpsum$mn.sd2.pos<-(tpsum$temp+tpsum$sd2)
tpsum$mn.sd2.neg<-tpsum$temp-tpsum$sd2


#create dataframe of the 2 sd with columns named "temp" so I can add it to plot
tpsum.sd2.pos<-tpsum[, c("mn.sd2.pos", "date.time.j", "month")]
tpsum.sd2.pos<-rename(tpsum.sd2.pos, temp=mn.sd2.pos)

tpsum.sd2.neg<-tpsum[, c("mn.sd2.neg", "date.time.j", "month")]
tpsum.sd2.neg<-rename(tpsum.sd2.neg, temp=mn.sd2.neg)


#plot mean and 2 standard deviations above and below
mnsd.plot<-ggplot(tpsum, aes(x=date.time.j, y=temp))
mnsd.plot+geom_line(color="black"
)+geom_line(data=tpsum.sd2.neg, aes(x=date.time.j, y=temp),
            color="blue"
)+geom_line(data=tpsum.sd2.pos, aes(x=date.time.j, y=temp),
            color="red")




#subsetting to only summer months (june-aug)

smr.tpsum<-subset(tpsum, month!="september" & month!="october")
smr.sd2.pos<-subset(tpsum.sd2.pos, month!="september" & month!="october")
smr.sd2.neg<-subset(tpsum.sd2.neg, month!="september" & month!="october")


#plotting mean +/- 2SD for summer months only
smr.mnsd.plot<-ggplot(smr.tpsum, aes(x=date.time.j, y=temp))
smr.mnsd.plot+geom_line(color="black"
)+geom_line(data=smr.sd2.neg, aes(x=date.time.j, y=temp),
            color="blue"
)+geom_line(data=smr.sd2.pos, aes(x=date.time.j, y=temp),
            color="red")



#creating data frames that only have data from each month
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


#subset to just summer months
smr.dmaxmn<-subset(dmaxmn, month!="september" & month!="october")

#plot daily max SD2 against daily max means

smr.sdmnmax.plot<-ggplot(smr.dmaxmn, aes(x=temp, y=max.sd2))
smr.sdmnmax.plot+geom_point(
)+geom_smooth(method=lm)

#----------------------

#LOOKING AT AUTOCORRELATION OF RESIDUALS FOR DAILY MAX MEAN TEMPS

#MEAN DATA FROM ALL TC

#subsetting mean data to only summer (june-august)
dmxmn_summer<-subset(dmaxmn, month!="september" & month!="october")

#running linear model of temp by date (both terms are numeric)
dmxmn.mod1<-lm(temp~date, 
               data = dmxmn_summer,
               na.action = na.omit)

anova(dmxmn.mod1)

#storing residuals in an object
dmxmn.res<-dmxmn.mod1$residuals

#running residuals in acf function
acf(dmxmn.res, type = "correlation", plot = TRUE)



#Running model with mean data with date as a quadratic term
  ##Doesn't seem to change much, either in model or in ACF plot
dmxmn.mod2<-lm(temp~I(date^2), 
               data = dmxmn_summer,
               na.action = na.omit)

anova(dmxmn.mod2)


#storing residuals in an object
dmxmn2.res<-dmxmn.mod2$residuals

#running residuals in acf function
acf(dmxmn2.res, type = "correlation", plot = TRUE)




#RAW DATA FOR ALL TC

#subset raw data to only summer months
plc.smr<-subset(plc, month!="september" & month!="october")

#group by date and thermocouple, then find the top value of temp for each combo
smr.dmaxraw <- plc.smr %>% group_by(date.j, tc) %>% top_n(1, temp)

#running linear model of temp by date (both terms are numeric)
dmxraw.mod1<-lm(temp~date.j, 
               data = smr.dmaxraw,
               na.action = na.omit)

anova(dmxraw.mod1)


#storing residuals in an object
dmxraw.res<-dmxraw.mod1$residuals


#running residuals in acf function
acf(dmxraw.res, type = "correlation", plot = TRUE)




#Modelling raw data with date.j as a quadratic term
  ##Doesn't seem to change much, either in model or in ACF plot
dmxraw.mod2<-lm(temp~I(date.j^2), 
                data = smr.dmaxraw,
                na.action = na.omit)

anova(dmxraw.mod2)


#storing residuals in an object
dmxraw2.res<-dmxraw.mod2$residuals


#running residuals in acf function
acf(dmxraw2.res, type = "correlation", plot = TRUE)





#-------------------------------------

#EXTRA PLOTS

#create small dataframe with date and maxsd2 (+mn temp) to plot along side max mn temp
maxsd2.pos<-dsdmn[,c("mn.sd2.pos", "date")]

#rename mn.sd2.pos to temp so it will plot on same axis
maxsd2.pos<-rename(maxsd2.pos, temp=mn.sd2.pos)


#plot max mn temp and max sd2 together

maxmnsd.plot<-ggplot(dmaxmn, aes(x=date, y=temp))
maxmnsd.plot+geom_point(
)+geom_line(
)+geom_point(data=maxsd2.pos, aes(x=date, y=temp),
             color="red"
)+geom_line(data=maxsd2.pos, aes(x=date, y=temp),
            color="red"
)+labs(y="daily max mean temp")



#plotting maxmn with sd2 at that time point

#making small dataframe with sd2 at maxmn temp
maxmnt.sd2.pos<-dmaxmn[,c("date", "mn.sd2.pos")]

#rename sd2 column temp so that it plots on y axis
maxmnt.sd2.pos<-rename(maxmnt.sd2.pos, temp=mn.sd2.pos)

#plot max mn temp wth the sd2 at that time point
maxmnsd.plot2<-ggplot(dmaxmn, aes(x=date, y=temp))
maxmnsd.plot2+geom_point(
)+geom_line(
)+geom_point(data=maxmnt.sd2.pos, aes(x=date, y=temp),
             color="red"
)+geom_line(data=maxmnt.sd2.pos, aes(x=date, y=temp),
            color="red"
)+labs(y="daily max mean temp")




#plotting just the max sd2 and the sd2 at the max mn temp (not added to mn temp)

#creating dataframes with just max sd2 and date, and just sd2 and date
maxsd<-dmaxmn[,c("date", "max.sd2")]
mnsd<-dmaxmn[,c("date", "sd2")]

#renaming both to sd to plot on same axis
maxsd<-rename(maxsd, sd=max.sd2)
mnsd<-rename(mnsd, sd=sd2)


#plot them against date

maxsd.plot<-ggplot(maxsd, aes(x=date, y=sd))
maxsd.plot+geom_point(color="red"
)+geom_line(color="red"
)+geom_point(data=mnsd, aes(x=date, y=sd),
             color="blue"
)+geom_line(data=mnsd, aes(x=date, y=sd),
            color="blue")


sdvsd.plot<-ggplot(dmaxmn, aes(x=max.sd2, y=sd2))
sdvsd.plot+geom_point(
)+geom_smooth(method=lm)

