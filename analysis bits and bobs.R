
#PACKAGES
install.packages("tidyr", "lubridate", "ggplot2", "dplyr", "gridExtra")
library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(gridExtra)

#IMPORT FULL DATASET
localizations<-read.csv("location")
localizations<-read.csv(
  "C:/Users/ethan/Dropbox/Flat Tub/Data/SS/!Results/1T_012820-020121_trimmed.csv")

#BREAKING UP INTO INDIVIDUAL SNAKES
subset_snake<-function(Tag_ID) {
  snake<-subset(localizations, localizations$TagId==Tag_ID)
  return(snake)
}

houdini<-subset_snake("66665578")


#COMPRESSING INTO DAILY SUMMARIES FOR INDIVIDUAL SNAKES

#**BEFORE DOING THIS CONVERT UTC TO EST IN EXCEL, SAVE AS "time_of_day_EST"**

#first
#take first of each day
remove_duplicates<-function(input){
  input[!duplicated(input$date),]
}
snake_firsts<-function(snake_name){
  snake_firsts<-snake_name %>% group_by(date) %>% slice_min(time_of_day_EST)
  intermediate<-remove_duplicates(snake_firsts)
  intermediate1<-subset(intermediate, select=c("date","time_of_day","time_of_day_EST"))
}


houdini_firsts<-snake_firsts(houdini)
names(houdini_firsts)<-c("date","first", "first_est")


#last
#take last of each day
snake_lasts<-function(snake_name){
  snake_lasts<-snake_name %>% group_by(date) %>% slice_max(time_of_day_EST)
  intermediate<-remove_duplicates(snake_lasts)
  intermediate1<-subset(intermediate, select=c("date","time_of_day", "time_of_day_EST"))
  }


houdini_lasts<-snake_lasts(houdini)
names(houdini_lasts)<-c("date","last", "last_est")

#Data Frame of first and last
houdini_first_last<-data_frame(houdini_firsts, houdini_lasts[c(2,3)])

#***NOTE: Go through in excel to turn 00:xx values into 24:xx and repair bugs where program misread start and end dates***
#tldr; export data to excel and proof it before going further


#setting date as a date variable rather than character
houdini_first_last$date<-as.POSIXct(houdini_first_last$date, format="%m/%d/%Y")
#ordering the df by date
houdini_first_last<-houdini_firs_last[order(houdini_first_last$date,decreasing=FALSE),]

#BEFORE MOVING ON***Manually add daily Photo-Period and weather data in excel.*** 
#*calculate difference between start and end times (diff) in H:M format, also do it in excel.*



#Active Period as quantities
#use the function below if you want to quantify photo-period as well
#quant_photo_hour, quant_photo_min
#this function will also quantify raw times into number of hours or minutes past midnight
#last-first hour
quant_diff_hour<-function(snake_diff){hour(as.POSIXlt.character(
  snake_diff$diff, format='%H:%M'))+minute(as.POSIXlt.character(
    snake_diff$diff, format='%H:%M'))/60}
#last-first min
quant_diff_min<-function(snake_diff){hour(as.POSIXlt.character(
  snake_diff$diff, format='%H:%M'))*60+minute(as.POSIXlt.character(
    snake_diff$diff, format='%H:%M'))}

quant_houdini_diff_hour<-quant_diff_hour(houdini_daily)
quant_houdini_diff_min<-quant_diff_min(houdini_daily)


#minutes active
snake_minutes<-function(snake){
  x<-snake%>%group_by(date)%>%summarise(n())
  x$date<-as.POSIXlt(x$date, format="%m/%d/%Y")
  x[order(x$date,decreasing=FALSE),]
}

houdini_minutes_active<-snake_minutes(houdini)

#Start and End compared to dawn and dusk as quantitities
#***first subtract dawn from start and end from dusk in excel to get "%H:%M" format***
#*name tfirst.trise and tset.tlast
#minute
quant_tfirst.trise_min<- function(snake_daily){
  hour(as.POSIXlt.character(
  snake_daily$tfirst.trise, format='%H:%M'))*60+minute(as.POSIXlt.character(
    snake_daily$tfirst.trise, format='%H:%M'))
}

quant_houdini_tfirst.trise_min<-quant_tfirst.trise_min(houdini_daily)

quant_tset.tlast_min<- function(snake_daily){
  hour(as.POSIXlt.character(
  snake_daily$tset.tlast, format='%H:%M'))*60+minute(as.POSIXlt.character(
    snake_daily$tset.tlast, format='%H:%M'))
}

quant_houdini_tset.tlast_min<-quant_tset.tlast_min(houdini_daily)

#hour
quant_tfirst.trise_hour<-function(snake_daily){
  hour(as.POSIXlt.character(
  snake_daily$tfirst.trise, format='%H:%M'))+minute(as.POSIXlt.character(
    snake_daily$tfirst.trise, format='%H:%M'))/60
}

quant_houdini_tfirst.tlast_hour<-quant_tfirst.trise_hour(houdini_daily)

quant_tset.tlast_hour<-function(snake_daily){
  hour(as.POSIXlt.character(
  snake_daily$tset.tlast, format='%H:%M'))+minute(as.POSIXlt.character(
    snake_daily$tset.tlast, format='%H:%M'))/60
}

quant_houdini_tset.tlast_hour<-quant_tset.tlast_hour(houdini_daily)


#Proportions
#active period/photo_period
prop_diffvphoto<-function(snake_daily){
  snake_daily$quant_diff_hour/
  snake_daily$quant_photo_hour
}

houdini_prop_diffvphoto<-prop_diffvphoto(houdini_daily)

#active min/photo-period
prop_activevdiff<-function(snake_daily){
  snake_daily$minutes_active/
  snake_daily$quant_diff_min
}

houdini_prop_activevdiff<-prop_activevdiff(houdini_daily)

#active min/active period
prop_activevphoto<-function(snake_daily){
  snake_daily$minutes_active/
  snake_daily$quant_photo_min
}

houdini_prop_activevphoto<-prop_activevphoto(houdini_daily)

#Date manipulation
#Calculating date as number of days
day_number<-function(snake_daily){
  x<-as.POSIXlt.character(snake_daily$date, format = "%m/%d/%Y")
  y<-strftime(x, format="%j")
  z<-as.numeric(y)
}

houdini_day_number<-day_number(houdini)

#Transform dates to be non-looping if you want to do that

num_days<-function(snake_daily,startdate){
  x<-as.Date(startdate, "%m/%d/%Y")
  as.numeric(difftime(snake_daily$date, x,units="days"))
}

houdini_num_days<-num_days(houdini_daily, "01/01/2020")

#qualify dates by month number
snake_month<-function(snake_daily){
  month(as.POSIXlt.character(snake_daily$date, format="%m/%d/%Y"))
}

houdini_month<-month(houdini_daily)

#MERGE YOUR DAILY FILE
houdini_daily<-data.frame("whatever you want to merge together")
names(houdini_daily)<-c("whatever names you want")

#Snake specific info
houdini_daily$sex<-"male"
houdini_daily$name<-"houdini"

#be careful, R sometimes adds an additional first column for no reason

#EXPORT DAILY FILE
write.csv(houdini_daily, "location/houdini_daily.csv")





#BRINGING EVERYONES DAILY DATA TOGETHER
#first make sure column names are consistent between snakes, change them with the names() function if necessary
names(houdini_daily)<-c("consistent", "naming", "scheme")

AllSnakes_daily <- rbind(bond_daily, bramble_daily, elm_daily, frodo_daily, houdini_daily, jack_daily, madonna_daily, rocky_daily, wiley_daily)

#MONTHLY STATISTICS
#average certain values by month
#NOTE: RESULTING VECTOR WILL BE SMALLER THAN ORIGINAL DATA FRAME AND WILL HAVE BE PART OF A NEW TABLE OF MONTHLY AVERAGES
#NOTE: I AM SHOWING HOW TO DO THIS ACROSS ALL SNAKES, USE INDIVIDUAL SNAKE DAILY FILES INSTEAD OF ALLSNAKES TO CONTROL FOR INDIVIDUAL
#characters work better than numeric variables for this

AllSnakes_monthly<-function(AllSnakes, var1, var2, var3, var4, var5){
  month_char<-as.character(AllSnakes$month)
  group_by(month_char)%>%
  summarise(ave_var1==mean(var1), sd_var1==sd(var1), 
            ave_var2==mean(var2), sd_var2==sd(var2),
            ave_var3==mean(var3), sd_var3==sd(var3),
            ave_var4==mean(var4), sd_var4==sd(var4),
            ave_var5==mean(var5), sd_var5==sd(var5))
}

#PLOTTING
#Proportion Plots

#proportion of photoperiod spanned by first and last
diff_photo<-ggplot(Allsnakes, aes(x = day_number, y = prop_diffvphoto, colour=name) )+
  #axis labels
  labs(x="Number of Days Since 01/01/2020",y="Active Period/Photoperiod")+
  #what you want to plot and any customization of points
  geom_point(aes(shape=sex),size=3)+#geom_line()+
  #shades active season
  annotate("rect", xmin = -Inf, xmax = 90, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 305, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+
  #adds 2020/2021 boundary
  geom_vline(xintercept=366)+
  #hide the x-axis title and labels and replace with the legend, it looks nicer if you stack it with another plot
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(),legend.position="bottom")+
  #If you want x-axis title and labels use the line below, you should also specify legend here
  #theme(axis.text.x = element_text(angle=270, vjust=0), legend.position = "none")+
  #set position for and label axis breaks, ***cancelled out by line above)
  scale_x_continuous(breaks=c(10,31,60,91,121,152,182,213,244,274,305,335,366,397),
                     labels=c("10"="JAN20","31"="FEB20","60"="MAR20","91"="APR20","121"="MAY20",
                              "152"="JUN20","182"="JUL20","213"="AUG20","244"="SEP20","274"="OCT20",
                              "305"="NOV20","335"="DEC20","366"="JAN21","397"="FEB21"))+
  #set the title to whatever you want
  ggtitle("(A) Proportion of Period (First-Last) Active Relative to Photoperiod")
#see the plot by itself
print(diff_photo)

active_photo<-ggplot(Allsnakes, aes(x = day_number, y = prop_activevphoto, colour=name) )+
  labs(x="Month",y="Active Time/Photoperiod")+geom_point(aes(shape=sex),size=3)+#geom_line()+
  annotate("rect", xmin = -Inf, xmax = 90, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 305, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+geom_vline(xintercept=366)+
  
  theme(axis.text.x = element_text(angle=270, vjust=0), legend.position = "none")+
  scale_x_continuous(breaks=c(10,31,60,91,121,152,182,213,244,274,305,335,366,397),
                     labels=c("10"="JAN20","31"="FEB20","60"="MAR20","91"="APR20","121"="MAY20",
                              "152"="JUN20","182"="JUL20","213"="AUG20","244"="SEP20","274"="OCT20",
                              "305"="NOV20","335"="DEC20","366"="JAN21","397"="FEB21"))+
  ggtitle("(B) Proportion of Minutes Active Relative to Photoperiod")
print(active_photo)

#plot the figures together
grid.arrange(diff_photo,active_photo)

#Start.End Plots

#delay between first activity and sunrise
first_rise<-ggplot(Allsnakes, aes(x = day_number, y = quant_tfirst.trise_min/60, colour=name) )+
  labs(x="Month",y="Time of First Activity-Sunrise (hours)")+geom_point(aes(shape=sex),size=3)+#geom_line()+
  annotate("rect", xmin = -Inf, xmax = 90, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 305, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+geom_vline(xintercept=366)+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), legend.position = "bottom")+
  scale_x_continuous(breaks=c(10,31,60,91,121,152,182,213,244,274,305,335,366,397))+
  ggtitle("(C) Hours From Sunrise To First Activity")
print(first_rise)

#delay between last activity and sunset
set_last<-ggplot(Allsnakes, aes(x = day_number, y = -quant_tset.tlast_min/60, colour=name) )+
  labs(x="Month",y="Time of Last Activity-Sunset (hours)")+geom_point(aes(shape=sex),size=3)+#geom_line()+
  annotate("rect", xmin = -Inf, xmax = 90, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 305, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+geom_vline(xintercept=366)+
  theme(axis.text.x = element_text(angle=270,vjust = 0), legend.position = "none")+
  scale_x_continuous(breaks=c(10,31,60,91,121,152,182,213,244,274,305,335,366,397),
                     labels=c("10"="JAN20","31"="FEB20","60"="MAR20","91"="APR20","121"="MAY20",
                              "152"="JUN20","182"="JUL20","213"="AUG20","244"="SEP20","274"="OCT20",
                              "305"="NOV20","335"="DEC20","366"="JAN21","397"="FEB21"))+
  #reverses the y-axis to be more intuitive
  scale_y_reverse()+
  ggtitle("(D) Hours From Sunset to Last Activity")
print(set_last)

grid.arrange(first_rise, set_last) 


#Plotting by month without averaging across month
#apply the general changes to produce monthly versions of other graphs
monthly_active_photo<-ggplot(Allsnakes, aes(x = month, y = prop_activevphoto, colour=name) )+
labs(x="Month",y="Active Time/Photo-Period")+geom_point(aes(shape=sex),size=3)+#geom_line()+
##Highlight the active season and mark annual boundary
annotate("rect", xmin = -Inf, xmax = 4, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 11, xmax =Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+
theme(axis.text.x = element_text(angle=270, vjust=0), legend.position = "top")+
##scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
labels=c("1"="JAN","2"="FEB","3"="MAR","4"="APR","5"="MAY",
        "6"="JUN","7"="JUL","8"="AUG","9"="SEP","10"="OCT",
       "11"="NOV","12"="DEC"))+
ggtitle("(B) Proportion of Minutes Active Relative to Photo-Period")
print(monthly_active_photo)

#monthly_diff_photo<-ggplot(Allsnakes, aes(x = month, y = prop_diffvphoto, colour=name) )+
# labs(x="Number of Days Since 01/01/2020",y="Active Period/Photo-Period")+geom_point(aes(shape=sex),size=3)+#geom_line()+
#annotate("rect", xmin = -Inf, xmax = 4, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 11, xmax =Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+
#  theme(axis.title.x=element_blank(),axis.text.x = element_blank(), legend.position = "bottom")+
# scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
#                   labels=c("1"="JAN","2"="FEB","3"="MAR","4"="APR","5"="MAY",
#                           "6"="JUN","7"="JUL","8"="AUG","9"="SEP","10"="OCT",
#                          "11"="NOV","12"="DEC"))+
#ggtitle("(A) Proportion of Period (First-Last) Active Relative to Photo-Period")
#print(monthly_diff_photo)

#grid.arrange(monthly_diff_photo,monthly_active_photo)



#Monthly boxplots
#NOTE: This is for all snakes, use individual snake daily files to control for individual
box_active<-ggplot(Allsnakes, aes(x= reorder(month_char, month), y = prop_activevphoto) )+
  #option to add colored legend commented out
  labs(x="Month",y="Minutes Active/Photo Period Minutes")+ #, color="Month")+
  #option to add colored legend commented out
  geom_boxplot()+ #aes(color=reorder(month_char,month)))+
  annotate("rect", xmin = -Inf, xmax = 4, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 11, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+
  #option to add colored legend, just set legend.position to "top", "bottom", "left" or "right"
  theme(axis.title.x=element_text("Month"),legend.position = "none")+
  scale_x_discrete(breaks=c(1,2,3,4, 5, 6, 7, 8, 9,10,11,12),
                    labels=c("1"="JAN","2"="FEB","3"="MAR","4"="APR","5"="MAY", "6"="JUN","7"="JUL",
                             "8"="AUG","9"="SEP","10"="OCT", "11"="NOV","12"="DEC"))+
  ggtitle("Proportion of Photo-Period Spent Active")
print(box_active)

#can be repeated for similar metrics


#doesn't quite work yet
#grid.arrange(box_first, box_last, box_active)





























#removing duplicate data
my_data[!duplicated(my_data$Sepal.Width), ]

#turning time into quantity of hours
quant_diff<-hour(as.POSIXlt.character(daily_times$diff, format='%H:%M'))+minute(as.POSIXlt.character(daily_times$diff, format='%H:%M'))/60

#exporting table to csv
write.csv(daily_times, "C:/Users/ethan/Desktop/Analysis for SEPARC/Indigos_daily_conditions.csv")

#importing csv to table
daily_times<-read.csv("C:/Users/ethan/Desktop/Analysis for SEPARC/Indigos_daily_conditions.csv")

#commonly imported csv
localizations<-read.csv("C:/Users/ethan/Dropbox/Flat Tub/Data/SS/!Results/1T_012820-020121_trimmed.csv")
localizations<-read.csv("C:/Users/ethan/Dropbox/Flat Tub/Data/SS/!Results/1T_031320-020121_trimmed_solar.csv")

#MAKING INDIVIDUAL SNAKE FILES

#Splitting off

elm<-subset(localizations, localizations$TagId=="61333366")
elm<-elm[!duplicated(elm$id),]
wiley<-subset(localizations, localizations$TagId=="61333300")
wiley<-wiley[!duplicated(wiley$id),]
bramble<-subset(localizations, localizations$TagId=="61333378")
bramble<-bramble[!duplicated(bramble$id),]
jack<-subset(localizations, localizations$TagId=="61520007")
frodo<-subset(localizations, localizations$TagId=="61526619")

houdini<-subset(localizations, localizations$TagId=="66665578")
rocky<-subset(localizations, localizations$TagId=="1E337819")
madonna<-subset(localizations, localizations$TagId=="78342A66")
bond<-subset(localizations, localizations$TagId=="1E4B2A66")


#Reducing to first and last of day
#first
#take first of each day
houdini_firsts<-houdini %>% group_by(date) %>% slice_min(time_of_day_EST)
#make a df with date and first
houdini_first_time<-data.frame(houdini_firsts$date, houdini_firsts$time_of_day)
#remove straggler duplicate dates
houdini_first_time_final<-houdini_first_time[!duplicated(houdini_first_time$houdini_firsts.date),]

#last
#take last of each day
houdini_lasts<-houdini %>% group_by(date) %>% slice_max(time_of_day_EST)
#make a df with date and last
houdini_last_time<-data.frame(houdini_lasts$date, houdini_lasts$time_of_day)
#remove straggler duplicate dates
houdini_last_time_final<-houdini_last_time[!duplicated(houdini_last_time$houdini_lasts.date),]

#merging into one df
houdini_diff<-data.frame(houdini_first_time_final$houdini_firsts.date, houdini_first_time_final$houdini_firsts.time_of_day, houdini_last_time_final$houdini_lasts.time_of_day)
#setting column names
names(houdini_diff)<-c("date","first","last")
#setting date as a date variable rather than character
houdini_diff$date<-as.POSIXct(houdini_diff$date, format="%m/%d/%Y")
#ordering the df by date
houdini_diff<-houdini_diff[order(houdini_diff$date,decreasing=FALSE),]

#firsts
elm_firsts<-elm %>% group_by(date) %>% slice_min(time_of_day)
elm_first_time<-data.frame(elm_firsts$date, elm_firsts$time_of_day)
elm_first_time_final<-elm_first_time[!duplicated(elm_first_time$elm_firsts.date),]

#lasts
elm_lasts<-elm %>% group_by(date) %>% slice_max(time_of_day)
elm_last_time<-data.frame(elm_lasts$date, elm_lasts$time_of_day)
elm_last_time_final<-elm_last_time[!duplicated(elm_last_time$elm_lasts.date),]


#last and first
elm_daily<-data.frame(elm_first_time_final$elm_firsts.date, elm_first_time_final$elm_firsts.time_of_day, elm_last_time_final$elm_lasts.time_of_day)
names(elm_daily)<-c("date","first","last")
elm_daily$date<-as.POSIXct(elm_daily$date, format="%m/%d/%Y")
elm_daily<-elm_daily[order(elm_daily$date,decreasing=FALSE),]

#export
write.csv(elm_daily, "C:/Users/ethan/Desktop/Analysis for SEPARC/elm/elm_daily_master.csv")




elm_diff<-as.POSIXlt.character(elm_daily$last, format="%H:%M")-as.POSIXlt.character(elm_daily$first, format="%H/%m")


#last-first hour
quant_elm_diff_hr<-(hour(as.POSIXlt.character(
  elm_daily$diff, format='%H:%M'))+minute(as.POSIXlt.character(
    elm_daily$diff, format='%H:%M'))/60)

#last-first min
quant_elm_diff_min<-(hour(as.POSIXlt.character(
  elm_daily$diff, format='%H:%M'))*60+minute(as.POSIXlt.character(
    elm_daily$diff, format='%H:%M')))

#minutes active
elm_minutes<-elm%>%group_by(date)%>%summarise(n())
elm_minutes$date<-as.POSIXlt(elm_minutes$date, format="%m/%d/%Y")
elm_minutes<-elm_minutes[order(elm_minutes$date,decreasing=FALSE),]
elm_daily<-data.frame(elm_daily, elm_minutes[2])
names(elm_minutes)<-c("date","first", "last", "minutes_active")

#quantifying difference between first and last
quant_houdini_diff<-(hour(as.POSIXlt.character(houdini_diff$last, format='%H:%M'))+minute(as.POSIXlt.character(houdini_diff$last, format='%H:%M'))/60)-(hour(as.POSIXlt.character(houdini_diff$first, format='%H:%M'))+minute(as.POSIXlt.character(houdini_diff$first, format='%H:%M'))/60)
prop_houdini_diff<-quant_houdini_diff/daily$quant_photo
#bringing it together
houdini_diff<-data.frame(houdini_diff, quant_houdini_diff, prop_houdini_diff)


#quantifying diff in hours
quant_houdini_diff_hr<-(hour(as.POSIXlt.character(
  houdini_daily$diff, format='%H:%M'))+minute(as.POSIXlt.character(
    houdini_daily$diff, format='%H:%M'))/60)
#diff in hours/photo_period
prop_houdini_diffvphoto<-
  quant_houdini_diff_hr/
  houdini_daily$quant_photo
#quantifying diff in min
quant_houdini_diff_min<-(hour(as.POSIXlt.character(
  houdini_daily$diff, format='%H:%M'))*60+minute(as.POSIXlt.character(
    houdini_daily$diff, format='%H:%M')))
#active min/diff min
prop_houdini_activevdiff<-
  houdini_daily$minutes_active/
  quant_houdini_diff_min

houdini_daily<-data.frame(
  houdini_daily, 
  quant_houdini_diff_hr,
  prop_houdini_diffvphoto, 
  quant_houdini_diff_min, 
  prop_houdini_activevdiff)

#EXPORTING
write.csv(bond_daily, "C:/Users/ethan/Desktop/Analysis for SEPARC/bond/bond_daily_master.csv")
write.csv(bramble_daily, "C:/Users/ethan/Desktop/Analysis for SEPARC/bramble/bramble_daily_master.csv")
write.csv(elm_daily, "C:/Users/ethan/Desktop/Analysis for SEPARC/elm/elm_daily_master.csv")
write.csv(frodo_daily, "C:/Users/ethan/Desktop/Analysis for SEPARC/frodo/frodo_daily_master.csv")
write.csv(houdini_daily, "C:/Users/ethan/Desktop/Analysis for SEPARC/houdini/houdini_daily_master.csv")
write.csv(jack_daily, "C:/Users/ethan/Desktop/Analysis for SEPARC/jack/jack_daily_master.csv")
write.csv(madonna_daily, "C:/Users/ethan/Desktop/Analysis for SEPARC/madonna/madonna_daily_master.csv")
write.csv(rocky_daily, "C:/Users/ethan/Desktop/Analysis for SEPARC/rocky/rocky_daily_master.csv")
write.csv(wiley_daily, "C:/Users/ethan/Desktop/Analysis for SEPARC/wiley/wiley_daily_master.csv")

#IMPORTING
bond_daily<-read.csv("C:/Users/ethan/Desktop/Analysis for SEPARC/bond/bond_daily_master.csv")
bramble_daily<-read.csv("C:/Users/ethan/Desktop/Analysis for SEPARC/bramble/bramble_daily_master.csv")
elm_daily<-read.csv("C:/Users/ethan/Desktop/Analysis for SEPARC/elm/elm_daily_master.csv")
frodo_daily<-read.csv("C:/Users/ethan/Desktop/Analysis for SEPARC/frodo/frodo_daily_master.csv")
houdini_daily<-read.csv("C:/Users/ethan/Desktop/Analysis for SEPARC/houdini/houdini_daily_master.csv")
jack_daily<-read.csv("C:/Users/ethan/Desktop/Analysis for SEPARC/jack/jack_daily_master.csv")
madonna_daily<-read.csv("C:/Users/ethan/Desktop/Analysis for SEPARC/madonna/madonna_daily_master.csv")
rocky_daily<-read.csv("C:/Users/ethan/Desktop/Analysis for SEPARC/rocky/rocky_daily_master.csv")
wiley_daily<-read.csv("C:/Users/ethan/Desktop/Analysis for SEPARC/wiley/wiley_daily_master.csv")
#adding individual snake data to the whole


#NUMBER OF TOTAL MINUTES ACTIVE
beep_count<-localizations%>%group_by(date)%>%summarise(n())
beep_count$date<-as.POSIXlt(beep_count$date, format="%m/%d/%Y")
beep_count<-beep_count[order(beep_count$date,decreasing=FALSE),]

#NUMBER MIN FOR EACH SNAKE
houdini_minutes<-houdini%>%group_by(date)%>%summarise(n())
houdini_minutes$date<-as.POSIXlt(houdini_minutes$date, format="%m/%d/%Y")
houdini_minutes<-houdini_minutes[order(houdini_minutes$date,decreasing=FALSE),]

#Hours to Min
names(houdini_daily)[11]<-"quant_photo_hour" #usually 10
houdini_daily<-houdini_daily%>%
  + mutate(quant_photo_min=quant_photo_hour*60)

#Proportion:MIN ACTIVE/MIN PHOTO PERIOD
houdini_daily1<-houdini_daily%>%
  mutate(prop_houdini_activevphoto_min=minutes_active/quant_photo_min)
View(houdini_daily1)
houdini_daily<-houdini_daily1

#JOINING BY DATE
houdini_daily<- left_join(houdini_daily, abiotic, by=c("date"))

#day_number
day_number<- strftime(jack_daily$date, format = "%j")
as.numeric(day_number)

#scraps
localizations<-read.csv("C:/Users/ethan/Dropbox/Flat Tub/Data/SS/!Results/1T_031320-020121_trimmed_solar.csv")

daily<-read.csv("C:/Users/ethan/Desktop/Analysis for SEPARC/Indigos_daily.csv", na.strings = "")

firsts<-localizations %>% group_by(date) %>% slice_min(time_of_day_EST)
first_time<-data.frame(firsts$date, firsts$time_of_day)
first_time1<-first_time[!duplicated(first_time$firsts.date),]
write_xlsx(first_time1,"C:/Users/ethan/Desktop/Indigos_first_time.xlsx")

lasts<-localizations %>% group_by(date) %>% slice_max(time_of_day_EST)
last_time<-data.frame(lasts$date, lasts$time_of_day)
last_time1<-last_time[!duplicated(last_time$lasts.date),]
write_xlsx(last_time1,"C:/Users/ethan/Desktop/Indigos_last_time.xlsx")



quant_tset.tlast_min<-as.numeric(as.POSIXct(Allsnakes$sunset_UTC, format="%H:%M")-as.POSIXct(Allsnakes$last, format="%H:%M"))


#transfer 0:00 to 24:00
quant_last<-ifelse(quant_last<1,quant_last+24,quant_last)


ggplot(Allsnakes, aes(x=day_number,y=quant_last))+geom_point()+geom_line()
ggplot(Allsnakes, aes(x=day_number,y=quant_first))+geom_point()+geom_line()










#FIGURES
#Bond, Bramble, Elm, Frodo, Houdini, Jack, Madonna, Rocky, Wiley

bond_SEPARC$name<-"bond"
bramble_SEPARC$name<-"bramble"
elm_SEPARC$name<-"elm"
frodo_SEPARC$name<-"frodo"
houdini_SEPARC$name<-"houdini"
jack_SEPARC$name<-"jack"
madonna_SEPARC$name<-"madonna"
wiley_SEPARC$name<-"wiley"
rocky_SEPARC$name<-"rocky"

bond_SEPARC$sex<-"male"
bramble_SEPARC$sex<-"female"
elm_SEPARC$sex<-"male"
frodo_SEPARC$sex<-"male"
houdini_SEPARC$sex<-"male"
jack_SEPARC$sex<-"male"
madonna_SEPARC$sex<-"female"
wiley_SEPARC$sex<-"female"
rocky_SEPARC$sex<-"male"



#standardize:
snake_SEPARC<-subset(snake_daily, select=c(2:9,11,16:19))
snake_SEPARC<-subset(snake_daily, select=c(2:14))

#non-looping dates
startdate<-as.Date("01/01/2020", "%m/%d/%Y")
num_days<-as.numeric(difftime(bond_daily$date,startdate ,units="days"))
bond_daily$day_number<-num_days
bond_SEPARC$day_number<-num_days

Allsnakes <- rbind(bond_SEPARC, bramble_SEPARC, elm_SEPARC, frodo_SEPARC, houdini_SEPARC, jack_SEPARC, madonna_SEPARC, rocky_SEPARC, wiley_SEPARC)
write.csv(Allsnakes, "C:/Users/ethan/Desktop/Analysis for SEPARC/All_Snakes_master.csv")

#AllDf_l <- AllDf %>% gather(Var,Value, A:C)

#proportion of photoperiod spent active
active_photo<-ggplot(Allsnakes, aes(x = day_number, y = prop_activevphoto, colour=name) )+
  labs(x="Month",y="Active Time/Photoperiod")+geom_point(aes(shape=sex),size=3)+#geom_line()+
  #Highlight the active season and mark annual boundary
  annotate("rect", xmin = -Inf, xmax = 90, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 305, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+geom_vline(xintercept=366)+
  theme(axis.text.x = element_text(angle=0, vjust=0), legend.position = "none")+
  #scale_x_continuous(breaks=c(1,31,60,91,121,152,182,213,244,274,305,335,366,397))+
  scale_x_continuous(breaks=c(10,31,60,91,121,152,182,213,244,274,305,335,366,397),
                     labels=c("10"="JAN20","31"="FEB20","60"="MAR20","91"="APR20","121"="MAY20",
                              "152"="JUN20","182"="JUL20","213"="AUG20","244"="SEP20","274"="OCT20",
                              "305"="NOV20","335"="DEC20","366"="JAN21","397"="FEB21"))+
  ggtitle("Proportion of Minutes Active Relative to Photoperiod")
print(active_photo)

#proportion of photoperiod spanned by first and last
diff_photo<-ggplot(Allsnakes, aes(x = day_number, y = prop_diffvphoto, colour=name) )+
  labs(x="Month",y="Active Period/Photoperiod")+geom_point(aes(shape=sex),size=3)+#geom_line()+
  annotate("rect", xmin = -Inf, xmax = 90, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 305, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+geom_vline(xintercept=366)+
  #blank out x axis
  #theme(axis.title.x=element_blank(), axis.text.x = element_blank(),legend.position="bottom")+
  theme(axis.text.x = element_text(angle=0, vjust=0), legend.position = "none")+
  scale_x_continuous(breaks=c(10,31,60,91,121,152,182,213,244,274,305,335,366,397),
        labels=c("10"="JAN20","31"="FEB20","60"="MAR20","91"="APR20","121"="MAY20",
                              "152"="JUN20","182"="JUL20","213"="AUG20","244"="SEP20","274"="OCT20",
                              "305"="NOV20","335"="DEC20","366"="JAN21","397"="FEB21"))+  
  ggtitle("Proportion of Period Active (First-Last) Relative to Photoperiod")
print(diff_photo)

#proportion of time between first and last spent active
#active_diff<-ggplot(Allsnakes, aes(x = day_number, y = prop_activevdiff, colour=name) )+
  #labs(x="Number of Days Since 01/01/2020",y="Active Time/Activity Period")+geom_point(aes(shape=sex),size=3)+
  #annotate("rect", xmin = -Inf, xmax = 90, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 305, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+geom_vline(xintercept=366)+
  #ggtitle("Proportion of Activity Period Spent Active")
#print(active_diff)

grid.arrange(diff_photo,active_photo)
#-----------------------------

#monthly_active_photo<-ggplot(Allsnakes, aes(x = month, y = prop_activevphoto, colour=name) )+
 # labs(x="Month",y="Active Time/Photo-Period")+geom_point(aes(shape=sex),size=3)+#geom_line()+
  ##Highlight the active season and mark annual boundary
  #annotate("rect", xmin = -Inf, xmax = 4, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 11, xmax =Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+
  #theme(axis.text.x = element_text(angle=270, vjust=0), legend.position = "none")+
  ##scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  #scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                     #labels=c("1"="JAN","2"="FEB","3"="MAR","4"="APR","5"="MAY",
                      #        "6"="JUN","7"="JUL","8"="AUG","9"="SEP","10"="OCT",
                       #       "11"="NOV","12"="DEC"))+
  #ggtitle("(B) Proportion of Minutes Active Relative to Photo-Period")
#print(monthly_active_photo)

#monthly_diff_photo<-ggplot(Allsnakes, aes(x = month, y = prop_diffvphoto, colour=name) )+
 # labs(x="Number of Days Since 01/01/2020",y="Active Period/Photo-Period")+geom_point(aes(shape=sex),size=3)+#geom_line()+
  #annotate("rect", xmin = -Inf, xmax = 4, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 11, xmax =Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+
#  theme(axis.title.x=element_blank(),axis.text.x = element_blank(), legend.position = "bottom")+
 # scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
  #                   labels=c("1"="JAN","2"="FEB","3"="MAR","4"="APR","5"="MAY",
   #                           "6"="JUN","7"="JUL","8"="AUG","9"="SEP","10"="OCT",
    #                          "11"="NOV","12"="DEC"))+
  #ggtitle("(A) Proportion of Period (First-Last) Active Relative to Photo-Period")
#print(monthly_diff_photo)

#grid.arrange(monthly_diff_photo,monthly_active_photo)
#----------------------------------
#delay between sunrise and first activity
first_rise<-ggplot(Allsnakes, aes(x = day_number, y = quant_tfirst.trise_min/60, colour=name) )+
  labs(x="Month",y="Time of First Activity-Sunrise (hours)")+geom_point(aes(shape=sex),size=3)+#geom_line()+
  annotate("rect", xmin = -Inf, xmax = 90, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 305, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+geom_vline(xintercept=366)+
  #theme(axis.title.x=element_blank(), axis.text.x = element_blank(), legend.position = "bottom")+
  theme(axis.text.x = element_text(angle=0,vjust = 0), legend.position = "none")+
  scale_x_continuous(breaks=c(10,31,60,91,121,152,182,213,244,274,305,335,366,397),
                     labels=c("10"="JAN20","31"="FEB20","60"="MAR20","91"="APR20","121"="MAY20",
                              "152"="JUN20","182"="JUL20","213"="AUG20","244"="SEP20","274"="OCT20",
                              "305"="NOV20","335"="DEC20","366"="JAN21","397"="FEB21"))+
  ggtitle("Hours From Sunrise To First Activity")
print(first_rise)

#delay between last activity and sunset
set_last<-ggplot(Allsnakes, aes(x = day_number, y = -quant_tset.tlast_min/60, colour=name) )+
  labs(x="Month",y="Time of Last Activity-Sunset (hours)")+geom_point(aes(shape=sex),size=3)+#geom_line()+
  annotate("rect", xmin = -Inf, xmax = 90, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+annotate("rect", xmin = 305, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = .1, color = NA)+geom_vline(xintercept=366)+
  theme(axis.text.x = element_text(angle=0,vjust = 0), legend.position = "none")+
  #scale_x_continuous(breaks=c(1,31,60,91,121,152,182,213,244,274,305,335,366,397))+
  scale_x_continuous(breaks=c(10,31,60,91,121,152,182,213,244,274,305,335,366,397),
                     labels=c("10"="JAN20","31"="FEB20","60"="MAR20","91"="APR20","121"="MAY20",
                            "152"="JUN20","182"="JUL20","213"="AUG20","244"="SEP20","274"="OCT20",
                            "305"="NOV20","335"="DEC20","366"="JAN21","397"="FEB21"))+
  scale_y_reverse()+
  ggtitle("Hours From Sunset to Last Activity")
print(set_last)


grid.arrange(first_rise, set_last) 
#grid.arrange(diff_photo,first_rise, set_last, active_photo, ncol=2)





#monthly box plots
box_last<-ggplot(Allsnakes, aes(group = month_char, y = quant_tset.tlast_min) )+
  labs(x="Month",y="Time of Last Activity-Sunset (-min)")+geom_boxplot(aes(color=month_char))+
  #scale_x_continuous(labels=c("JAN20","FEB20","MAR20","APR20","MAY20","JUN20","JUL20","AUG20","SEP20","OCT20","NOV20","DEC20","JAN21","FEB21"))+
  ggtitle("How Long Before Sunset Does Snake Activity Cease? (-Minutes)")
print(box_last)

box_first<-ggplot(Allsnakes, aes(group = month, y = quant_tfirst.trise_min) )+
  labs(x="Month",y="Time of First Activity-Sunrise (min)")+geom_boxplot()+
  ggtitle("How Long After Sunrise Does Snake Activity Begin? (Minutes)")
print(box_first)

box_active<-ggplot(Allsnakes, aes(group = month, y = prop_activevphoto) )+
  labs(x="Month",y="Minutes Active")+geom_boxplot()+
  ggtitle("Proportion of Photo-Period Spent Active")
print(box_active)

#doesn't quite work yet
grid.arrange(box_first, box_last, box_active)

#---------------------
 #making the table
month<-month(as.POSIXlt.character(Allsnakes$date, format = "%m/%d/%Y"))
Allsnakes<-data.frame(Allsnakes,month)

ave_first<-Allsnakes%>%group_by(month)%>%summarise(ave_begin=mean(quant_tfirst.trise_min))
ave_first_hour<-Allsnakes%>%group_by(month)%>%summarise(ave_begin=mean(quant_tfirst.trise_min/60))

ave_last<-Allsnakes%>%group_by(month)%>%summarise(ave_cease=mean(-quant_tset.tlast_min))
ave_last_hour<-Allsnakes%>%group_by(month)%>%summarise(ave_cease=mean(-quant_tset.tlast_min/60))


#in relation to sunrise rather than set
ave_last_rise<-Allsnakes%>%group_by(month)%>%summarise(ave_cease_sunrise=mean(quant_tset.tlast_min+quant_photo_min))

#automatically takes different snakes into account
ave_active<-Allsnakes%>%group_by(month)%>%summarise(ave_active=mean(minutes_active))
ave_active_hour<-Allsnakes%>%group_by(month)%>%summarise(ave_active=mean(minutes_active/60))

ave_prop_active<-Allsnakes%>%group_by(month)%>%summarise(ave_prop_active=mean(prop_activevphoto))

SEPARC_table<-data.frame(ave_first, ave_last$ave_cease, ave_active$ave_active)
names(SEPARC_table)<-c("Month", "Average Time of First Activity From Sunrise", "Average Time of Last Activity From Sunset", "Average Active Minutes")
SEPARC_table

SEPARC_table_hour<-data.frame(ave_first_hour, ave_last_hour$ave_cease, ave_active_hour$ave_active)
names(SEPARC_table_hour)<-c("Month", "Average Hours From Sunrise To First Activity", "Average Hours From Last Activity To Sunset", "Average Total Active Hours", "Average Active Proportion of Photo-Period")
SEPARC_table_hour



par(mfrow=c(1,3))
barplot(SEPARC_table$`Average Time of First Activity`~SEPARC_table$Month)
barplot(SEPARC_table$`Average Time of Last Activity`~SEPARC_table$Month)
barplot(SEPARC_table$`Average Active Minutes`~SEPARC_table$Month)


round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

SEPARC_table_hour_rounded<-round_df(SEPARC_table_hour, 4)

datatable(SEPARC_table_hour_rounded, rownames = FALSE)%>%
  formatStyle(columns = "Month", 
              background = styleInterval(c(4,10), c("lightgray", "white","lightgray")))%>%
  formatStyle(columns = "Average Hours From Sunrise To First Activity", 
              background = styleEqual(c(4.55,4.614,3.399,3.125,4.014,3.093), c("lightgray", "lightgray","lightgray", "lightgray","lightgray","lightgray")))%>%
  formatStyle(columns = "Average Hours From Last Activity To Sunset", 
            background = styleEqual(c(2.736,2.826,4.722,4.685,3.65,3.467), c("lightgray", "lightgray","lightgray", "lightgray","lightgray","lightgray")))%>%
  formatStyle(columns = "Average Total Active Hours", 
              background = styleEqual(c(1.655,1.859,1.425,2.898,2.436,1.434), c("lightgray", "lightgray","lightgray", "lightgray","lightgray","lightgray")))
  
