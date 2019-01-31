#Cleaning MF datalogger data from 2009


#load libraries

library(readr)
library(ggplot2)
library(tidyr)
library(Rmisc)
library(dplyr)
library(viridis)
library(cowplot)
library(extrafont)


#----------------------------------

#load data

soil <- read_csv("Bert(ALL).MF.SoilTemps.2009.csv", 
                 col_names = FALSE)
View(soil)


plant <- read_csv("Ernie(ALL).MF.ModelsonPlants.2009.csv", 
                  col_names = FALSE)
View(plant)

#-------------------------------------

#Removing info in header, renaming columns, reincorporating header data into usuable columns

#Remove header info, make separate data frame

plant.head<-plant[c(1:12),]
soil.head<-soil[c(1:12),]

plant<-plant[-c(1:12),]
soil<-soil[-c(1:12),]


#rename columns

plant<-plant %>% rename(date.time = X1,
                        rec.num = X2,
                        se_volt = X3,
                        slrw_avg = X4,
                        slrk_tot = X5,
                        ref_avg = X6,
                        tc1 = X7, tc2 = X8, tc3 = X9, tc4 = X10, tc5 = X11, tc6 = X12, tc7 = X13, tc8 = X14,
                        tc9 = X15, tc10 = X16, tc11 = X17, tc12 = X18, tc13 = X19, tc14 = X20, tc15 = X21, tc16 = X22,
                        tc17 = X23, tc18 = X24, tc19 = X25, tc20 = X26, tc21 = X27, tc22 = X28, tc23 = X29, tc24 = X30, tc25 = X31)

soil<-soil %>% rename(date.time = X1,
                        rec.num = X2,
                        se_volt = X3,
                        slrw_avg = X4,
                        slrk_tot = X5,
                        ref_avg = X6,
                        tc1 = X7, tc2 = X8, tc3 = X9, tc4 = X10, tc5 = X11, tc6 = X12, tc7 = X13, tc8 = X14)
                       
plant.head<-plant.head %>% rename(tc1 = X7, tc2 = X8, tc3 = X9, tc4 = X10, tc5 = X11, tc6 = X12, tc7 = X13, tc8 = X14,
                                  tc9 = X15, tc10 = X16, tc11 = X17, tc12 = X18, tc13 = X19, tc14 = X20, tc15 = X21, tc16 = X22,
                                  tc17 = X23, tc18 = X24, tc19 = X25, tc20 = X26, tc21 = X27, tc22 = X28, tc23 = X29, tc24 = X30, tc25 = X31)


soil.head<-soil.head %>% rename(tc1 = X7, tc2 = X8, tc3 = X9, tc4 = X10, tc5 = X11, tc6 = X12, tc7 = X13, tc8 = X14)
                                

#remove columns from soil data frame that do not have thermocouples

soil<-soil[,-c(15:31)]

#Reincorporate header data into usuable columns
  ##not all data can be reincorporated in this data format--need to make long data to include location and position on plant

plant$farm<-"Mason Farm"
plant$plot<-"A"
plant$substrate<-"P"
plant$leaf_type<-"TBc"

soil$farm<-"Mason Farm"
soil$plot<-"C"
soil$substrate<-"S"

#----------------------------------

#Converting date and time to Julian date and dec time

#separate date and time by space
plant<-plant %>% separate(date.time, c("date", "time"), sep=" ")
soil<-soil %>% separate(date.time, c("date", "time"), sep=" ")

#create column with julian date
plant$date.j<-strptime(plant$date, "%m/%d")$yday+1
soil$date.j<-strptime(soil$date, "%m/%d")$yday+1

#separates time into hours and minutes, changes class from character to numeric. Calculates decimal time
#as minutes per hour (/60), then calculates decimal time as hour per day (/24), then adds to julian date
#to create column with julian day and decimal time of each recorded temp
plant <- plant %>% separate(time, c("h", "m"), ":", remove=FALSE) %>%
  mutate(h = as.numeric(h)) %>% mutate(m = as.numeric(m)) %>%
  mutate(time.dec = h+m/60) %>%
  mutate(time.dec.24 = time.dec/24) %>%
  mutate(date.time.j = date.j+time.dec.24)

soil <- soil %>% separate(time, c("h", "m"), ":", remove=FALSE) %>%
  mutate(h = as.numeric(h)) %>% mutate(m = as.numeric(m)) %>%
  mutate(time.dec = h+m/60) %>%
  mutate(time.dec.24 = time.dec/24) %>%
  mutate(date.time.j = date.j+time.dec.24)


#-----------------------------

#creating long data sets for plant and soil data, incorporating location and position data

plant.lng<-plant %>% gather(tc, temp, c(10:34))
soil.lng<-soil %>% gather(tc, temp, c(10: 17))

#Adding location and position data

plant.lng$loc<-ifelse(plant.lng$tc=="tc1", plant.head[3,7],
               ifelse(plant.lng$tc=="tc2", plant.head[3,8],
               ifelse(plant.lng$tc=="tc3", plant.head[3,9],
               ifelse(plant.lng$tc=="tc4", plant.head[3,10],
               ifelse(plant.lng$tc=="tc5", plant.head[3,11],
               ifelse(plant.lng$tc=="tc6", plant.head[3,12],
                ifelse(plant.lng$tc=="tc7", plant.head[3,13],
                 ifelse(plant.lng$tc=="tc8", plant.head[3,14],
                 ifelse(plant.lng$tc=="tc9", plant.head[3,15],
                 ifelse(plant.lng$tc=="tc10", plant.head[3,16],
                 ifelse(plant.lng$tc=="tc11", plant.head[3,17],
                 ifelse(plant.lng$tc=="tc12", plant.head[3,18],
                 ifelse(plant.lng$tc=="tc13", plant.head[3,19],
                 ifelse(plant.lng$tc=="tc14", plant.head[3,20],
                 ifelse(plant.lng$tc=="tc15", plant.head[3,21],
                 ifelse(plant.lng$tc=="tc16", plant.head[3,22],
                 ifelse(plant.lng$tc=="tc17", plant.head[3,23],
                 ifelse(plant.lng$tc=="tc18", plant.head[3,24],
                 ifelse(plant.lng$tc=="tc19", plant.head[3,25],
                 ifelse(plant.lng$tc=="tc20", plant.head[3,26],
                 ifelse(plant.lng$tc=="tc21", plant.head[3,27],
                 ifelse(plant.lng$tc=="tc22", plant.head[3,28],
                 ifelse(plant.lng$tc=="tc23", plant.head[3,29],
                 ifelse(plant.lng$tc=="tc24", plant.head[3,30],
                 ifelse(plant.lng$tc=="tc25", plant.head[3, 31],0)))))))))))))))))))))))))

#ifelse creates a list, so need to convert to character
plant.lng$loc<-as.character(plant.lng$loc)

plant.lng$pos<-ifelse(plant.lng$tc=="tc1", plant.head[6,7],
               ifelse(plant.lng$tc=="tc2", plant.head[6,8],
               ifelse(plant.lng$tc=="tc3", plant.head[6,9],
               ifelse(plant.lng$tc=="tc4", plant.head[6,10],
               ifelse(plant.lng$tc=="tc5", plant.head[6,11],
               ifelse(plant.lng$tc=="tc6", plant.head[6,12],
               ifelse(plant.lng$tc=="tc7", plant.head[6,13],
               ifelse(plant.lng$tc=="tc8", plant.head[6,14],
               ifelse(plant.lng$tc=="tc9", plant.head[6,15],
               ifelse(plant.lng$tc=="tc10", plant.head[6,16],
               ifelse(plant.lng$tc=="tc11", plant.head[6,17],
               ifelse(plant.lng$tc=="tc12", plant.head[6,18],
               ifelse(plant.lng$tc=="tc13", plant.head[6,19],
               ifelse(plant.lng$tc=="tc14", plant.head[6,20],
               ifelse(plant.lng$tc=="tc15", plant.head[6,21],
               ifelse(plant.lng$tc=="tc16", plant.head[6,22],
               ifelse(plant.lng$tc=="tc17", plant.head[6,23],
               ifelse(plant.lng$tc=="tc18", plant.head[6,24],
               ifelse(plant.lng$tc=="tc19", plant.head[6,25],
               ifelse(plant.lng$tc=="tc20", plant.head[6,26],
               ifelse(plant.lng$tc=="tc21", plant.head[6,27],
               ifelse(plant.lng$tc=="tc22", plant.head[6,28],
               ifelse(plant.lng$tc=="tc23", plant.head[6,29],
               ifelse(plant.lng$tc=="tc24", plant.head[6,30],
               ifelse(plant.lng$tc=="tc25", plant.head[6, 31],0)))))))))))))))))))))))))

#ifelse creates a list, so need to convert to character
plant.lng$pos<-as.character(plant.lng$pos)



soil.lng$loc<-ifelse(soil.lng$tc=="tc1", soil.head[3,7],
              ifelse(soil.lng$tc=="tc2", soil.head[3,8], 
              ifelse(soil.lng$tc=="tc3", soil.head[3,9],
              ifelse(soil.lng$tc=="tc4", soil.head[3,10],
              ifelse(soil.lng$tc=="tc5", soil.head[3,11],
              ifelse(soil.lng$tc=="tc6", soil.head[3,12],
              ifelse(soil.lng$tc=="tc7", soil.head[3,13],
              ifelse(soil.lng$tc=="tc8", soil.head[3,14], 0))))))))

#ifelse creates a list, so need to convert to character
soil.lng$loc<-as.character(soil.lng$loc)


soil.lng$pos<-ifelse(soil.lng$tc=="tc1", soil.head[6,7],
              ifelse(soil.lng$tc=="tc2", soil.head[6,8], 
              ifelse(soil.lng$tc=="tc3", soil.head[6,9],
              ifelse(soil.lng$tc=="tc4", soil.head[6,10],
              ifelse(soil.lng$tc=="tc5", soil.head[6,11],
              ifelse(soil.lng$tc=="tc6", soil.head[6,12],
              ifelse(soil.lng$tc=="tc7", soil.head[6,13],
              ifelse(soil.lng$tc=="tc8", soil.head[6,14], 0))))))))

#ifelse creates a list, so need to convert to character
soil.lng$pos<-as.character(soil.lng$pos)

#-------------------------

#write to csv

write.csv(plant, "MF_datalogger_2009_plant.csv",row.names = FALSE)
write.csv(plant.lng, "MF_datalogger_2009_plant-long.csv",row.names = FALSE)

write.csv(soil, "MF_datalogger_2009_soil.csv",row.names = FALSE)
write.csv(soil.lng, "MF_datalogger_2009_soil-long.csv",row.names = FALSE)



#--------------------------

#working on function to find area of errors

plant.lng$temp<-as.numeric(plant.lng$temp)

pl.tc2<-subset(plant.lng, tc=="tc2")


#This method seems to work!
#Method found at https://stackoverflow.com/questions/25790728/selection-of-rows-and-their-neighboring-rows-in-r

#Testing on thermocouple with known errors

#Creates an index of rows that fit your criteria (here, temp is between 0<x>50
ind.pltc2<-which(pl.tc2[,"temp"]>50 | pl.tc2[,"temp"]<0)

#creates a function that applies your specified index, then finds the rows 6 above and 6 below
ind.err<-Filter(function(x) x > 0, unique(unlist(lapply(ind.pltc2, "+", -6:6))))

#creates a dataframe that has only the erroneous rows specified in index, and the rows 
##specified by the function
err.rows<-pl.tc2[ind.err,]

#Creates a sorting column in your main dataframe  that has value of "FALSE" if that row DOES NOT contain
##an erroneous temp reading or isn't a row in the window to remove (an hour before and after the erroneous reading)
##a value of "TRUE" indicates that it is a row to be removed
pl.tc2$keep<-pl.tc2$date.time.j %in% err.rows$date.time.j


#Replace erroneous readings with a dummy variable (.000001) that can be used to convert those temp
  ##values to NA--this will remove distracting connecting lines from future plots
pl.tc2$temp<-ifelse(pl.tc2$keep==TRUE, .000001, pl.tc2$temp)
pl.tc2$temp[pl.tc2$temp==.000001]<-NA


#plot to double check your excision
pltc2.cl.plot<-ggplot(pl.tc2, aes(x=date.time.j, y=temp))
pltc2.cl.plot+geom_line(
)+geom_hline(yintercept = 50
)+geom_hline(yintercept = 0)


#----------------------

#testing to see if above method will work to remove errors from whole data sheet

#create an id column for later row matching
plant.lng$id<-c(1:499775)

#Creates an index of rows that fit your criteria (here, temp is between 0<x>50, or =="NaN")
ind.plall<-which(plant.lng[,"temp"]>50 | plant.lng[,"temp"]<0 | plant.lng[,"temp"]=="NaN")

#creates a function that applies your specified index, then finds the rows 6 above and 6 below
ind.err.all<-Filter(function(x) x > 0, unique(unlist(lapply(ind.plall, "+", -6:6))))

#creates a dataframe that has only the erroneous rows specified in index, and the rows 
##specified by the function
err.rows.all<-plant.lng[ind.err.all,]

#Creates a sorting column in your main dataframe  that has value of "FALSE" if that row DOES NOT 
##contain an erroneous temp reading or isn't a row in the window to remove 
##(an hour before and after the erroneous reading) a value of "TRUE" indicates that it is a row 
##to be removed
plant.lng.cl<-plant.lng
plant.lng.cl$keep<-plant.lng.cl$id %in% err.rows.all$id


#Replace erroneous readings with a dummy variable (.000001) that can be used to convert those temp
##values to NA--this will remove distracting connecting lines from future plots
plant.lng.cl$temp<-ifelse(plant.lng.cl$keep=="TRUE", .000001, plant.lng.cl$temp)
plant.lng.cl$temp[plant.lng.cl$temp==.000001]<-NA


#plot to double check your excision
plcl.all.plot<-ggplot(plant.lng.cl, aes(x=date.time.j, y=temp))
plcl.all.plot+geom_line(
)+geom_hline(yintercept = 50
)+geom_hline(yintercept = 0
)+facet_wrap(~tc)



#write to csv

write.csv(plant.lng.cl, "MF_datalogger_2009_plant_clean_test.csv",row.names = FALSE)


