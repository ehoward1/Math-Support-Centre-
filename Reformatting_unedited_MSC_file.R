rm(list=ls()) #clear space

# This code will take the unedited file and create additional variables

require(lubridate)
require(magrittr)
require(car)
require(directlabels)
require(reshape2)
require(viridis)
require(dplyr)


# read in file and summary
MSC = read.csv("Original.csv", header=TRUE)
head(MSC) # looking at the data
summary(MSC)
str(MSC)
colnames(MSC) = c("Date", "wait_time", "with_tutor", "pause_tutor")

# Reformat file to include extra columns for Time variables
MSC$Date=strptime(as.character(MSC$Date), "%d/%m/%Y %H:%M", tz="GMT") 

MSC$Month = month(MSC$Date) 
MSC$Year = year(MSC$Date) %>% factor(levels=c("2015","2016","2017","2018", "2019"),
                                     ordered=TRUE) 
MSC$Day = weekdays(MSC$Date) %>% factor(levels=c("Monday","Tuesday","Wednesday",
                                                 "Thursday","Friday"),
                                        ordered=TRUE) 
MSC$Hour = hour(MSC$Date) %>% as.numeric() # Note: while the offical start ime is 10am, the log-in may open early e.g., 9.58am
MSC$Minute = minute(MSC$Date) %>% as.numeric()
MSC$Semester = ifelse(MSC$Month < 6, 2, 1) %>% factor(levels=c("1","2"), ordered=TRUE)
MSC$Week=week(MSC$Date)

# Binary Variables
MSC$Hour_9 = ifelse(MSC$Hour==9,1,0)
MSC$Hour_10 = ifelse(MSC$Hour==10,1,0)
MSC$Hour_11 = ifelse(MSC$Hour==11,1,0)
MSC$Hour_12 = ifelse(MSC$Hour==12,1,0)
MSC$Hour_13 = ifelse(MSC$Hour==13,1,0)
MSC$Hour_14 = ifelse(MSC$Hour==14,1,0)
MSC$Hour_15 = ifelse(MSC$Hour==15,1,0)
MSC$Hour_16 = ifelse(MSC$Hour==16,1,0)
MSC$Hour_17 = ifelse(MSC$Hour==17,1,0)
MSC$Hour_18 = ifelse(MSC$Hour==18,1,0)
MSC$Hour_19 = ifelse(MSC$Hour==19,1,0)
MSC$Hour_20 = ifelse(MSC$Hour==20,1,0)

# Might need to add extra years
MSC$Year_15 = ifelse(MSC$Year==2015,1,0)
MSC$Year_16 = ifelse(MSC$Year==2016,1,0)
MSC$Year_17 = ifelse(MSC$Year==2017,1,0)
MSC$Year_18 = ifelse(MSC$Year==2018,1,0)
MSC$Year_19 = ifelse(MSC$Year==2019,1,0) 

MSC$Month_Sep = ifelse(MSC$Month==9,1,0)
MSC$Month_Oct = ifelse(MSC$Month==10,1,0)
MSC$Month_Nov = ifelse(MSC$Month==11,1,0)
MSC$Month_Dec = ifelse(MSC$Month==12,1,0)
MSC$Month_Jan = ifelse(MSC$Month==1,1,0)
MSC$Month_Feb = ifelse(MSC$Month==2,1,0)
MSC$Month_Mar = ifelse(MSC$Month==3,1,0)
MSC$Month_Apr = ifelse(MSC$Month==4,1,0)
MSC$Month_May = ifelse(MSC$Month==5,1,0)

MSC$Day_Mon = ifelse(MSC$Day=="Monday",1,0)
MSC$Day_Tue = ifelse(MSC$Day=="Tuesday",1,0)
MSC$Day_Wed = ifelse(MSC$Day=="Wednesday",1,0)
MSC$Day_Thur = ifelse(MSC$Day=="Thursday",1,0)
MSC$Day_Fri = ifelse(MSC$Day=="Friday",1,0)


# Remove any "HOT TOPICS" - these are hour long tutorials/workshops and
# should not be included in waiting list results
MSC$Date1 = as.Date(MSC$Date)
MSC = MSC[(which(MSC$Date1 != "2019-10-01" | MSC$Hour!=18 | MSC$Minute != 47)),]
MSC = MSC[(which(MSC$Date1 != "2019-11-13" | MSC$Hour!=18 | MSC$Minute != 54)),]
MSC = MSC[(which(MSC$Date1 != "2019-11-12" | MSC$Hour!=18 | MSC$Minute != 49)),]
MSC = MSC[(which(MSC$Date1 != "2019-11-19" | MSC$Hour!=19 | MSC$Minute != 1)),]
MSC = MSC[(which(MSC$Date1 != "2019-11-19" | MSC$Hour!=18 | MSC$Minute != 47)),]
MSC = MSC[(which(MSC$Date1 != "2019-11-19" | MSC$Hour!=18 | MSC$Minute != 54)),]
MSC = MSC[(which(MSC$Date1 != "2019-11-19" | MSC$Hour!=18 | MSC$Minute != 58)),]
MSC = MSC[(which(MSC$Date1 != "2019-11-20" | MSC$Hour!=19 | MSC$Minute != 1)),]
MSC = MSC[(which(MSC$Date1 != "2019-11-20" | MSC$Hour!=18 | MSC$Minute != 50)),]
MSC = MSC[(which(MSC$Date1 != "2019-02-25" | MSC$Hour!=18 | MSC$Minute != 5)),]
MSC = MSC[(which(MSC$Date1 != "2019-03-25" | MSC$Hour!=18 | MSC$Minute != 0)),]
MSC = MSC[(which(MSC$Date1 != "2018-11-12" | MSC$Hour!=19 | MSC$Minute != 1)),]
MSC = MSC[(which(MSC$Date1 != "2018-11-13" | MSC$Hour!=18 | MSC$Minute != 45)),]
MSC = MSC[(which(MSC$Date1 != "2018-10-10" | MSC$Hour!=18 | MSC$Minute != 37)),]
MSC = MSC[(which(MSC$Date1 != "2017-10-12" | MSC$Hour!=9 | MSC$Minute !=27)),]
MSC = MSC[(which(MSC$Date1 != "2017-11-21" | MSC$Hour!=19 | MSC$Minute !=3)),]
MSC = MSC[(which(MSC$Date1 != "2017-11-29" | MSC$Hour!=19 | MSC$Minute !=6)),]
MSC = MSC[(which(MSC$Date1 != "2016-04-06" | MSC$Hour!=18 | MSC$Minute !=59)),]
MSC = MSC[(which(MSC$Date1 != "2017-10-05" | MSC$Hour!=19 | MSC$Minute !=3)),]
MSC = MSC[(which(MSC$Date1 != "2017-10-05" | MSC$Hour!=19 | MSC$Minute !=22)),]
MSC = MSC[(which(MSC$Date1 != "2016-02-08" | MSC$Hour!=19 | MSC$Minute !=2)),]
MSC = MSC[(which(MSC$Date1 != "2016-02-23" | MSC$Hour!=18 | MSC$Minute !=32)),]
MSC = MSC[(which(MSC$Date1 != "2016-03-07" | MSC$Hour!=19 | MSC$Minute !=16)),]
MSC = MSC[(which(MSC$Date1 != "2016-03-09" | MSC$Hour!=18 | MSC$Minute !=3)),]
MSC = MSC[(which(MSC$Date1 != "2016-04-04" | MSC$Hour!=19 )),]
MSC = MSC[(which(MSC$Date1 != "2016-04-05" | MSC$Hour!=18 )),]
MSC = MSC[(which(MSC$Date1 != "2016-04-06" | MSC$Hour!=18 )),]
MSC = MSC[(which(MSC$Date1 != "2015-10-12" | MSC$Hour!=17 | MSC$Minute !=29)),]
MSC = MSC[(which(MSC$Date1 != "2015-10-12" | MSC$Hour!=19 | MSC$Minute !=25)),]
MSC = MSC[(which(MSC$Date1 != "2015-10-21" | MSC$Hour!=18 | MSC$Minute !=21)),]
MSC = MSC[(which(MSC$Date1 != "2015-10-27" | MSC$Hour!=17 | MSC$Minute !=34)),]
MSC = MSC[(which(MSC$Date1 != "2015-11-16" | MSC$Hour!=18 | MSC$Minute !=4)),]
MSC = MSC[(which(MSC$Date1 != "2015-11-23" | MSC$Hour!=17 | MSC$Minute !=58)),]
MSC = MSC[(which(MSC$Date1 != "2015-11-17" | MSC$Hour!=17 | MSC$Minute !=36)),]
MSC = MSC[(which(MSC$Date1 != "2015-04-20" | MSC$Hour!=19 | MSC$Minute !=21)),]

# Calculating how many students in the room at time
MSC$tutor_start_time = MSC$Date + dseconds(MSC$wait_time) # Start time = entry + wait times
MSC$tutor_finish_time = MSC$Date + dseconds(MSC$wait_time) + dseconds(MSC$with_tutor) 

# Convert time in seconds to time in minutes
MSC$wait_time = round(MSC$wait_time/60,2) 
# Entry where student waited longer than two hours is likely an error so remove
MSC = subset(MSC, wait_time<91)

# Ordering date with queueing variable
MSC = MSC[order(MSC$Date, decreasing = FALSE ),]

# how many people are waiting and with tutors? checking whether the prior 42 
# students were queueing should be enough!
MSC$queue1 = rep(0, nrow(MSC)) # Create a new variable
for(i in 2:nrow(MSC)){
  a=interval(MSC$Date[max(1,i-42):i-1], MSC$tutor_finish_time[max(1,i-42):i-1])
  b=interval(MSC$Date[i], MSC$Date[i] + dseconds(1))
  MSC$queue1[i] = int_overlaps(a, b) %>% sum()
}

# Old System versus New System (The UCD MSC experienced a system change 
# with who could access the MSC. The MSC was limited
# to level 0, 1 and 2 modules owing to volume of users)

change = ymd_hms("2015-10-19 08:00:00", tz = "GMT")
MSC$change_over = ifelse(MSC$Date < change, 1, 0)

# Code for number of tutors and to make the code run quicker, smaller
# subsets (by semester) have been created which will be combined again later

MSC15_1 = subset(MSC, Semester=="1")
MSC15_1 = subset(MSC15_1, Year=="2015")
MSC15_2 = subset(MSC, Semester=="2")
MSC15_2 = subset(MSC15_2, Year=="2015")
MSC16_1 = subset(MSC, Semester=="1")
MSC16_1 = subset(MSC16_1, Year=="2016")
MSC16_2 = subset(MSC, Semester=="2")
MSC16_2 = subset(MSC16_2, Year=="2016")
MSC17_1 = subset(MSC, Semester=="1")
MSC17_1 = subset(MSC17_1, Year=="2017")
MSC17_2 = subset(MSC, Semester=="2")
MSC17_2 = subset(MSC17_2, Year=="2017")
MSC18_1 = subset(MSC, Semester=="1")
MSC18_1 = subset(MSC18_1, Year=="2018")
MSC18_2 = subset(MSC, Semester=="2")
MSC18_2 = subset(MSC18_2, Year=="2018")
MSC19_2 = subset(MSC, Semester=="2")
MSC19_2 = subset(MSC19_2, Year=="2019")
MSC19_1 = subset(MSC, Semester=="1")
MSC19_1 = subset(MSC19_1, Year=="2019")

################################################################
################################################################
# Run "other file called "Reformatting term weeks" file 
# Then run the remaining code here
################################################################
################################################################

colnames(MSC_152) = colnames(MSC_151)
colnames(MSC_162) = colnames(MSC_151)
colnames(MSC_172) = colnames(MSC_151)
colnames(MSC_182) = colnames(MSC_151)
colnames(MSC_192) = colnames(MSC_151)
colnames(MSC_191) = colnames(MSC_151)
colnames(MSC_181) = colnames(MSC_151)
colnames(MSC_171) = colnames(MSC_151)
colnames(MSC_161) = colnames(MSC_151)
MSC  =rbind.data.frame(MSC_151, MSC_152, MSC_161, MSC_162, 
                           MSC_171, MSC_172, MSC_181, MSC_182, MSC_192, MSC_191)
head(MSC) # check data

#########################################################

# Reformatting done, now remove NA values owing to outside regular times

MSC = subset(MSC, term_week != "NA")
MSC = subset(MSC, number_tutors != "NA") ##
MSC = subset(MSC, Day != "NA")
MSC = subset(MSC, Hour>8)
MSC = subset(MSC, Hour<21)

# No longer need the selected variables
MSC = subset(MSC, select=(-c(Date, with_tutor, pause_tutor, tutor_start_time, tutor_finish_time, Date1)))

dim(MSC) # Should give 25,702 rows and 43 columns

# Save results
# setwd()
write.csv(MSC, "MSC.csv", row.names = FALSE)

# The MSC file should now be ready for analysing
