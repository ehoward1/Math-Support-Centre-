# Recommended to run this code as the third file
# First format the original data with "Reformatting_unedited_MSC_file.R" and 
# "Reformatting_term_week_and_the_number_of_tutors.R"

rm(list=ls()) #clear space

# Set working directory

install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("readr")
install.packages("viridis")
require(dplyr)
require(magrittr)
require(ggplot2)
require(readr)
require(viridis)


MSC = read.csv("MSC.csv", header=TRUE)

# Make sure the variables are coded correctly
MSC$Year = MSC$Year %>% factor(levels=c("2015","2016","2017","2018","2019"), ordered=TRUE) 
MSC$Hour = as.numeric(MSC$Hour) 
MSC$Minute = as.numeric(MSC$Minute)
MSC$number_tutors = as.numeric(as.character(MSC$number_tutors))
MSC$Semester = MSC$Semester %>% factor(levels=c("1","2"), ordered=TRUE)
MSC$term_week = MSC$term_week %>% factor(levels=c("1","2","3","4","5","6","7","8",
                                                  "9","10","11","12","rev","exam"), labels=c("1","2","3","4","5","6","7","8","9","10","11","12","rev","exam"), ordered=TRUE) 
MSC$Day = MSC$Day %>% factor(levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), ordered=TRUE)

############### Descriptive Statistics for Section 3.4 of the paper ###################

table(MSC$Semester) %>% prop.table() # prop.table converts counts to proportion
subset(MSC, Semester==1) %>% summary()
subset(MSC, Semester==2) %>% summary()
table(MSC$term_week) %>% prop.table()
table(MSC$Hour) %>% prop.table()
MSC %>% group_by(Day) %>% summarise(mean = mean(wait_time), sum=sum(wait_time))


################### Histogram of waiting times (Figure 1 in paper) ######################
ggplot(data=MSC, aes(x=wait_time)) + 
  geom_histogram(aes(y=..density..),breaks=seq(0, 100, by = 5), fill="blue", colour = "black")+
  #geom_histogram(breaks=seq(0, 100, by = 5), fill="blue", colour = "black")+
  stat_function(fun=dexp, geom="line", colour="red", size=1.2, args=list(rate=0.0688))+
  scale_x_continuous(breaks=seq(0, 90, 10))+ 
  theme_bw()+ 
  #ylim(0,12000)+
  labs(x="Waiting Time (minutes)", y="Distribution\nof Student\nVisits") + 
  theme(
    plot.title = element_text(lineheight=0, color='black', face="bold",size=13),
    axis.title.x=element_text(angle=0, color='black',face='bold', size=13),
    axis.title.y=element_text(angle=0, vjust = 1, color='black',face='bold', size=13),
    axis.text.x=element_text(angle=0, color='black', size=13),
    axis.text.y=element_text(angle=0, color='black',  size=13)
  )  +   theme(legend.position = "none")


# Wait Time by Year and Semester 

# Split MSC dataset by semester 
MSC_S1 = subset(MSC, Semester=="1") # Data only for semester 1
MSC_S2 = subset(MSC, Semester=="2") # Data only for semester 2

#Plot function
Waiting_time_plot = function(dataset, factor_of_interest, title, x_lab_name){ 
  name = enquo(factor_of_interest)
  
  dataMedian <- summarise(group_by(dataset, !!name), MD = length(wait_time)) # To calculate 'n='
  
  ggplot(data = dataset, aes(x=factor(!!name), y=wait_time)) +
    geom_boxplot(aes(fill=factor(!!name))) + coord_flip() +
    theme_bw() + ylab('Wait Time for Students (minutes)')+xlab(x_lab_name)+
    scale_y_continuous(breaks=seq(0, 100, 20))+ 
    geom_text(data = dataMedian, aes(!!name, 1, label = paste('n=', MD)),
             position = position_dodge(width = 1), size = 4, vjust = -3.7)+ # need to adjust position of n or can delete this to have no 'n='
    theme(
      plot.title = element_blank(),
      axis.title.x=element_text(angle=0, color='black',face='bold', size=13),
      axis.title.y=element_text(angle=0, vjust = 1, color='black',face='bold', size=13),
      axis.text.x=element_text(angle=0, color='black', size=13),
      axis.text.y=element_text(angle=0, color='black',  size=13)
    )  +   theme(legend.position = "none") +
    scale_fill_viridis(discrete=TRUE, option="D") 
}

# Example boxplots with various variable and datasets
Waiting_time_plot(dataset = MSC,    factor_of_interest = Semester, title = "Wait Time Given by Semester",
                  x_lab_name = "Semester")
Waiting_time_plot(dataset = MSC_S1, factor_of_interest = Year,     
                  title = "Wait Time Given by Year for Semester 1", x_lab_name = "Year")
Waiting_time_plot(dataset = MSC_S2, factor_of_interest = Year, 
                  title = "Wait Time Given by Year for Semester 2", x_lab_name = "Year")
Waiting_time_plot(dataset = MSC, factor_of_interest = number_tutors, 
                  title = "Wait Time Given by Number of Tutors", x_lab_name = "Number\nof\nTutors")
Waiting_time_plot(dataset = MSC, factor_of_interest = term_week, 
                  title = "Wait Time Given by Term Week", x_lab_name = "Term Week")
Waiting_time_plot(dataset = MSC, factor_of_interest = Hour, 
                  title = "Wait Time Given by Hour", x_lab_name = "Hour")


############## Investigate different years of MSC data ###################
MSC_Y15 = subset(MSC, Year=="2015")
MSC_Y15_1 = subset(MSC_Y15, Semester==1)
MSC_Y15_2 = subset(MSC_Y15, Semester==2)
MSC_Y16 = subset(MSC, Year=="2016")
MSC_Y16_1 = subset(MSC_Y16, Semester==1)
MSC_Y16_2 = subset(MSC_Y16, Semester==2)
MSC_Y17 = subset(MSC, Year=="2017")
MSC_Y17_1 = subset(MSC_Y17, Semester==1)
MSC_Y17_2 = subset(MSC_Y17, Semester==2)
MSC_Y18 = subset(MSC, Year=="2018")
MSC_Y18_2 = subset(MSC_Y18, Semester==2)
MSC_Y18 = subset(MSC, Year=="2018")
MSC_Y18_1 = subset(MSC_Y18, Semester==1)
MSC_Y19 = subset(MSC, Year=="2019")
MSC_Y19_2 = subset(MSC_Y19, Semester==2)
MSC_Y19 = subset(MSC, Year=="2019")
MSC_Y19_1 = subset(MSC_Y19, Semester==1)

Waiting_time_plot(dataset = MSC_Y15_1, factor_of_interest = Day, title = "Wait Time in 2015/16 Sem 1" ,
                  x_lab_name = "Day")
Waiting_time_plot(dataset = MSC_Y16_1, factor_of_interest = Day, title = "Wait Time in 2016/17 Sem 1",
                  x_lab_name = "Day")


#Diagrams for Paper:
#Figure 2:
MSC = subset(MSC, number_tutors>0) # In revision and exam weeks there was an extra couple of hours
# open for the MSC. Since it was outside normal hours, tutors = 0 according to the timetable (n=90)
Waiting_time_plot(dataset = MSC, factor_of_interest = number_tutors, title = "Wait Time Given by Number of Tutors", x_lab_name = "Number\nof\nTutors")

#Figure 
# Remove revision and exam weeks and see a specific term waiting times then
MSCnorev_18_1  =subset(MSC_Y18_1, term_week !="rev")
Waiting_time_plot(dataset = MSCnorev_18_1, factor_of_interest = Day, 
                  title = "Wait Time in 2018/19 Sem 1 without revision week", x_lab_name = "Day")

#Figure 3
# Choose two semesters of data to compare each other with
MSCnorev_18_1 = subset(MSC_Y18_1, term_week !="rev")
MSCnorev_19_1 = subset(MSC_Y19_1, term_week !="rev")
MSC_compare = rbind.data.frame(MSCnorev_18_1, MSCnorev_19_1)

Waiting_time_plot_comparisons = function(dataset, y_factor_of_interest, l_factor_of_interest, title, x_lab_name){ 
  yname = enquo(y_factor_of_interest)
  lname = enquo(l_factor_of_interest)
  
  dataMedian <- summarise(group_by(dataset, !!yname, !!lname), MD = length(wait_time)) # To calculate 'n='
  
  print(dataMedian)
  
  ggplot(data = dataset, aes(x=factor(!!yname), y=wait_time)) +
    geom_boxplot(aes(fill=factor(!!lname))) + coord_flip() +
    theme_bw() + labs(y='Wait Time for Students (minutes)', x=x_lab_name, fill= 'Year')+
    scale_y_continuous(breaks=seq(0, 100, 20))+ 
    geom_text(data = dataMedian, aes(!!yname, -2, label = paste('n=', MD)),
              position = position_dodge(width = 1), size = 3.5, vjust = c(rep(c(1.9,-2.4),5)))+ # need to adjust position of n or can delete this to have no 'n='
    theme(
      plot.title = element_blank(),
      axis.title.x=element_text(angle=0, color='black',face='bold', size=13),
      axis.title.y=element_text(angle=0, vjust = 1, color='black',face='bold', size=13),
      axis.text.x=element_text(angle=0, color='black', size=13),
      axis.text.y=element_text(angle=0, color='black',  size=13)
    )  +   #theme(legend.position = "none") +
    scale_fill_viridis(discrete=TRUE, option="C")+guides(fill = guide_legend(reverse = TRUE)) 
}

Waiting_time_plot_comparisons(dataset = MSC_compare, y_factor_of_interest = Day, 
                  l_factor_of_interest = Year,
                  title = "Wait Time in for 2018/19 and 2019/20 Sem 1 without revision week", x_lab_name = "Day")

############### Discussion Statistics Section 5 #######################

# Prediction models did not consider revision or exam weeks so
# remove these weeks before cnducting descriptive statistics

MSCr = subset(MSC, term_week !="rev") #remove revision week
MSCr = subset(MSCr, term_week !="exam") #remove exam week

MSC_Y15 = subset(MSCr, Year=="2015")
MSC_Y15_1 = subset(MSC_Y15, Semester==1)
MSC_Y15_2 = subset(MSC_Y15, Semester==2)
MSC_Y16 = subset(MSCr, Year=="2016")
MSC_Y16_1 = subset(MSC_Y16, Semester==1)
MSC_Y16_2 = subset(MSC_Y16, Semester==2)
MSC_Y17 = subset(MSCr, Year=="2017")
MSC_Y17_1 = subset(MSC_Y17, Semester==1)
MSC_Y17_2 = subset(MSC_Y17, Semester==2)
MSC_Y18 = subset(MSCr, Year=="2018")
MSC_Y18_2 = subset(MSC_Y18, Semester==2)
MSC_Y18 = subset(MSCr, Year=="2018")
MSC_Y18_1 = subset(MSC_Y18, Semester==1)
MSC_Y19 = subset(MSCr, Year=="2019")
MSC_Y19_2 = subset(MSC_Y19, Semester==2)
MSC_Y19 = subset(MSCr, Year=="2019")
MSC_Y19_1 = subset(MSC_Y19, Semester==1)

Academic_15_16 = rbind.data.frame(MSC_Y15_1, MSC_Y16_2)
Academic_16_17 = rbind.data.frame(MSC_Y16_1, MSC_Y17_2)
Academic_17_18 = rbind.data.frame(MSC_Y17_1, MSC_Y18_2)
Academic_18_19 = rbind.data.frame(MSC_Y18_1, MSC_Y19_2)

summary(Academic_15_16$wait_time) # gives median/mean waiting time for academic year
summary(Academic_16_17$wait_time)
summary(Academic_17_18$wait_time)
summary(Academic_18_19$wait_time)

dim(Academic_15_16) # gives number of students for each academic year
dim(Academic_16_17)
dim(Academic_17_18)
dim(Academic_18_19)
