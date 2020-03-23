# Code to create term week variable and calculate number of tutors 
# The specific week dates are based on the UCD calender year.
# The dates would need to be adjusted for the institution in question
# 2015 sem 2 which is semester 2 of the 2014/15 calender year

term_week_1_Start = as.Date('2015-01-19')
term_week_1_End = as.Date('2015-01-25')

term_week_2_Start = as.Date('2015-01-26')
term_week_2_End = as.Date('2015-02-01')

term_week_3_Start = as.Date('2015-02-02')
term_week_3_End = as.Date('2015-02-08')

term_week_4_Start = as.Date('2015-02-09')
term_week_4_End = as.Date('2015-02-15')

term_week_5_Start = as.Date('2015-02-16')
term_week_5_End = as.Date('2015-02-22')

term_week_6_Start = as.Date('2015-02-23')
term_week_6_End = as.Date('2015-03-01')

term_week_7_Start = as.Date('2015-03-02')
term_week_7_End = as.Date('2015-03-08')

term_week_F_Start = as.Date('2015-03-09')
term_week_F_End = as.Date('2015-03-22')

term_week_8_Start = as.Date('2015-03-23')
term_week_8_End = as.Date('2015-03-29')

term_week_9_Start = as.Date('2015-03-30')
term_week_9_End = as.Date('2015-04-05')

term_week_10_Start = as.Date('2015-04-06')
term_week_10_End = as.Date('2015-04-12')

term_week_11_Start = as.Date('2015-04-13')
term_week_11_End = as.Date('2015-04-19')

term_week_12_Start = as.Date('2015-04-20')
term_week_12_End = as.Date('2015-04-26')

term_week_rev_Start = as.Date('2015-04-27')
term_week_rev_End = as.Date('2015-05-03')

term_week_exam_Start = as.Date('2015-05-04')
term_week_exam_End = as.Date('2015-05-16')


MSC15_2$term_week = lapply(MSC15_2$Date,FUN = function(x){
  x = as.Date(x)
  if(is.na(x)){
    return(NA)
  }else if(term_week_1_Start <= x & x <= term_week_1_End){
    return("1")
  }else if(term_week_2_Start <= x & x <= term_week_2_End){
    return("2")
  }else if(term_week_3_Start <= x & x <= term_week_3_End){
    return("3")
  }else if(term_week_4_Start <= x & x <= term_week_4_End){
    return("4")
  }else if(term_week_5_Start <= x & x <= term_week_5_End){
    return("5")
  }else if(term_week_6_Start <= x & x <= term_week_6_End){
    return("6")
  }else if(term_week_7_Start <= x & x <= term_week_7_End){
    return("7")
  }else if(term_week_8_Start <= x & x <= term_week_8_End){
    return("8")
  }else if(term_week_9_Start <= x & x <= term_week_9_End){
    return("9")
  }else if(term_week_10_Start <= x & x <= term_week_10_End){
    return("10")
  }else if(term_week_11_Start <= x & x <= term_week_11_End){
    return("11")
  }else if(term_week_12_Start <= x & x <= term_week_12_End){
    return("12")
  }else if(term_week_rev_Start <= x & x <= term_week_rev_End){
    return("rev")
  }else if(term_week_exam_Start <= x & x <= term_week_exam_End){
    return("exam")
  }else{
    return("Other")
  }
})

MSC15_2$term_week = factor(MSC15_2$term_week ,levels=c("1","2","3","4","5","6","7","8","9",
                                                       "10","11","12","rev","exam",NA),
                           ordered=TRUE, exclude = NULL)

##############################################################################
##############################################################################
##############################################################################

# 2015 sem 1 which is 2015/16 calender year

term_week_1_Start = as.Date('2015-09-07')
term_week_1_End = as.Date('2015-09-13')

term_week_2_Start = as.Date('2015-09-14')
term_week_2_End = as.Date('2015-09-20')

term_week_3_Start = as.Date('2015-09-21')
term_week_3_End = as.Date('2015-09-27')

term_week_4_Start = as.Date('2015-09-28')
term_week_4_End = as.Date('2015-10-04')

term_week_5_Start = as.Date('2015-10-05')
term_week_5_End = as.Date('2015-10-11')

term_week_6_Start = as.Date('2015-10-12')
term_week_6_End = as.Date('2015-10-18')

term_week_7_Start = as.Date('2015-10-19')
term_week_7_End = as.Date('2015-10-25')

term_week_8_Start = as.Date('2015-10-26')
term_week_8_End = as.Date('2015-11-01')

term_week_9_Start = as.Date('2015-11-02')
term_week_9_End = as.Date('2015-11-08')

term_week_10_Start = as.Date('2015-11-09')
term_week_10_End = as.Date('2015-11-15')

term_week_11_Start = as.Date('2015-11-16')
term_week_11_End = as.Date('2015-11-22')

term_week_12_Start = as.Date('2015-11-23')
term_week_12_End = as.Date('2015-11-29')

term_week_rev_Start = as.Date('2015-11-30')
term_week_rev_End = as.Date('2015-12-06')

term_week_exam_Start = as.Date('2015-12-07')
term_week_exam_End = as.Date('2015-12-18')


MSC15_1$term_week = lapply(MSC15_1$Date,FUN = function(x){
  x = as.Date(x)
  if(is.na(x)){
    return(NA)
  }else if(term_week_1_Start <= x & x <= term_week_1_End){
    return("1")
  }else if(term_week_2_Start <= x & x <= term_week_2_End){
    return("2")
  }else if(term_week_3_Start <= x & x <= term_week_3_End){
    return("3")
  }else if(term_week_4_Start <= x & x <= term_week_4_End){
    return("4")
  }else if(term_week_5_Start <= x & x <= term_week_5_End){
    return("5")
  }else if(term_week_6_Start <= x & x <= term_week_6_End){
    return("6")
  }else if(term_week_7_Start <= x & x <= term_week_7_End){
    return("7")
  }else if(term_week_8_Start <= x & x <= term_week_8_End){
    return("8")
  }else if(term_week_9_Start <= x & x <= term_week_9_End){
    return("9")
  }else if(term_week_10_Start <= x & x <= term_week_10_End){
    return("10")
  }else if(term_week_11_Start <= x & x <= term_week_11_End){
    return("11")
  }else if(term_week_12_Start <= x & x <= term_week_12_End){
    return("12")
  }else if(term_week_rev_Start <= x & x <= term_week_rev_End){
    return("rev")
  }else if(term_week_exam_Start <= x & x <= term_week_exam_End){
    return("exam")
  }else{
    return("Other")
  }
})

MSC15_1$term_week = factor(MSC15_1$term_week ,levels=c("1","2","3","4","5","6","7","8","9",
                                                       "10","11","12","rev","exam",NA),
                           ordered=TRUE,exclude = NULL)

table(MSC15_1$Week)%>% sum()
table(MSC15_1$term_week) %>% sum () # 29 diff from before term time

##################################################################
##################################################################
##################################################################

## next do 2016...

# 2016 sem 2 which is 2015/16 calender year

term_week_1_Start = as.Date('2016-01-25')
term_week_1_End = as.Date('2016-01-31')

term_week_2_Start = as.Date('2016-02-01')
term_week_2_End = as.Date('2016-02-07')

term_week_3_Start = as.Date('2016-02-08')
term_week_3_End = as.Date('2016-02-14')

term_week_4_Start = as.Date('2016-02-15')
term_week_4_End = as.Date('2016-02-21')

term_week_5_Start = as.Date('2016-02-22')
term_week_5_End = as.Date('2016-02-28')

term_week_6_Start = as.Date('2016-02-29')
term_week_6_End = as.Date('2016-03-06')

term_week_7_Start = as.Date('2016-03-07')
term_week_7_End = as.Date('2016-03-13')

term_week_F_Start = as.Date('2016-03-14')
term_week_F_End = as.Date('2016-03-27')

term_week_8_Start = as.Date('2016-03-28')
term_week_8_End = as.Date('2016-04-03')

term_week_9_Start = as.Date('2016-04-04')
term_week_9_End = as.Date('2016-04-10')

term_week_10_Start = as.Date('2016-04-11')
term_week_10_End = as.Date('2016-04-17')

term_week_11_Start = as.Date('2016-04-18')
term_week_11_End = as.Date('2016-04-24')

term_week_12_Start = as.Date('2016-04-25')
term_week_12_End = as.Date('2016-05-01')

term_week_rev_Start = as.Date('2016-05-02')
term_week_rev_End = as.Date('2016-05-08')

term_week_exam_Start = as.Date('2016-05-09')
term_week_exam_End = as.Date('2016-05-21')


MSC16_2$term_week = lapply(MSC16_2$Date,FUN = function(x){
  x = as.Date(x)
  if(is.na(x)){
    return(NA)
  }else if(term_week_1_Start <= x & x <= term_week_1_End){
    return("1")
  }else if(term_week_2_Start <= x & x <= term_week_2_End){
    return("2")
  }else if(term_week_3_Start <= x & x <= term_week_3_End){
    return("3")
  }else if(term_week_4_Start <= x & x <= term_week_4_End){
    return("4")
  }else if(term_week_5_Start <= x & x <= term_week_5_End){
    return("5")
  }else if(term_week_6_Start <= x & x <= term_week_6_End){
    return("6")
  }else if(term_week_7_Start <= x & x <= term_week_7_End){
    return("7")
  }else if(term_week_F_Start <= x & x <= term_week_F_End){
    return("F")
  }else if(term_week_8_Start <= x & x <= term_week_8_End){
    return("8")
  }else if(term_week_9_Start <= x & x <= term_week_9_End){
    return("9")
  }else if(term_week_10_Start <= x & x <= term_week_10_End){
    return("10")
  }else if(term_week_11_Start <= x & x <= term_week_11_End){
    return("11")
  }else if(term_week_12_Start <= x & x <= term_week_12_End){
    return("12")
  }else if(term_week_rev_Start <= x & x <= term_week_rev_End){
    return("rev")
  }else if(term_week_exam_Start <= x & x <= term_week_exam_End){
    return("exam")
  }else{
    return("Other")
  }
})

MSC16_2$term_week = factor(MSC16_2$term_week ,levels=c("1","2","3","4","5","6","7","8","9",
                                                       "10","11","12","rev","exam",NA),
                           ordered=TRUE,exclude = NULL)

table(MSC16_2$Week)%>% sum()
table(MSC16_2$term_week) %>% sum () 

##################################################################
##################################################################
##################################################################

## next do 2016...

# 2016 sem 1 which is 2016/17 calender year


term_week_1_Start = as.Date('2016-09-12')
term_week_1_End = as.Date('2016-09-18')

term_week_2_Start = as.Date('2016-09-19')
term_week_2_End = as.Date('2016-09-25')

term_week_3_Start = as.Date('2016-09-26')
term_week_3_End = as.Date('2016-10-02')

term_week_4_Start = as.Date('2016-10-03')
term_week_4_End = as.Date('2016-10-09')

term_week_5_Start = as.Date('2016-10-10')
term_week_5_End = as.Date('2016-10-16')

term_week_6_Start = as.Date('2016-10-17')
term_week_6_End = as.Date('2016-10-23')

term_week_7_Start = as.Date('2016-10-24')
term_week_7_End = as.Date('2016-10-30')

term_week_8_Start = as.Date('2016-10-31')
term_week_8_End = as.Date('2016-11-06')

term_week_9_Start = as.Date('2016-11-07')
term_week_9_End = as.Date('2016-11-13')

term_week_10_Start = as.Date('2016-11-14')
term_week_10_End = as.Date('2016-11-20')

term_week_11_Start = as.Date('2016-11-21')
term_week_11_End = as.Date('2016-11-27')

term_week_12_Start = as.Date('2016-11-28')
term_week_12_End = as.Date('2016-12-04')

term_week_rev_Start = as.Date('2016-12-05')
term_week_rev_End = as.Date('2016-12-11')

term_week_exam_Start = as.Date('2016-12-12')
term_week_exam_End = as.Date('2016-12-22')


MSC16_1$term_week = lapply(MSC16_1$Date,FUN = function(x){
  x = as.Date(x)
  if(is.na(x)){
    return(NA)
  }else if(term_week_1_Start <= x & x <= term_week_1_End){
    return("1")
  }else if(term_week_2_Start <= x & x <= term_week_2_End){
    return("2")
  }else if(term_week_3_Start <= x & x <= term_week_3_End){
    return("3")
  }else if(term_week_4_Start <= x & x <= term_week_4_End){
    return("4")
  }else if(term_week_5_Start <= x & x <= term_week_5_End){
    return("5")
  }else if(term_week_6_Start <= x & x <= term_week_6_End){
    return("6")
  }else if(term_week_7_Start <= x & x <= term_week_7_End){
    return("7")
  }else if(term_week_8_Start <= x & x <= term_week_8_End){
    return("8")
  }else if(term_week_9_Start <= x & x <= term_week_9_End){
    return("9")
  }else if(term_week_10_Start <= x & x <= term_week_10_End){
    return("10")
  }else if(term_week_11_Start <= x & x <= term_week_11_End){
    return("11")
  }else if(term_week_12_Start <= x & x <= term_week_12_End){
    return("12")
  }else if(term_week_rev_Start <= x & x <= term_week_rev_End){
    return("rev")
  }else if(term_week_exam_Start <= x & x <= term_week_exam_End){
    return("exam")
  }else{
    return("Other")
  }
})

MSC16_1$term_week = factor(MSC16_1$term_week ,levels=c("1","2","3","4","5","6","7","8","9",
                                                       "10","11","12","rev","exam",NA),
                           ordered=TRUE,exclude = NULL)

table(MSC16_1$Week)%>% sum()
table(MSC16_1$term_week) %>% sum () # 29 diff from before term time

##################################################################
##################################################################
##################################################################

## next do 2017...

# 2017 sem 1 which is 2017/18 calender year


term_week_1_Start = as.Date('2017-09-11')
term_week_1_End = as.Date('2017-09-17')

term_week_2_Start = as.Date('2017-09-18')
term_week_2_End = as.Date('2017-09-24')

term_week_3_Start = as.Date('2017-09-25')
term_week_3_End = as.Date('2017-10-01')

term_week_4_Start = as.Date('2017-10-02')
term_week_4_End = as.Date('2017-10-08')

term_week_5_Start = as.Date('2017-10-09')
term_week_5_End = as.Date('2017-10-15')

term_week_6_Start = as.Date('2017-10-16')
term_week_6_End = as.Date('2017-10-22')

term_week_7_Start = as.Date('2017-10-23')
term_week_7_End = as.Date('2017-10-29')

term_week_8_Start = as.Date('2017-10-30')
term_week_8_End = as.Date('2017-11-05')

term_week_9_Start = as.Date('2017-11-06')
term_week_9_End = as.Date('2017-11-12')

term_week_10_Start = as.Date('2017-11-13')
term_week_10_End = as.Date('2017-11-19')

term_week_11_Start = as.Date('2017-11-20')
term_week_11_End = as.Date('2017-11-26')

term_week_12_Start = as.Date('2017-11-27')
term_week_12_End = as.Date('2017-12-03')

term_week_rev_Start = as.Date('2017-12-04')
term_week_rev_End = as.Date('2017-12-10')

term_week_exam_Start = as.Date('2017-12-11')
term_week_exam_End = as.Date('2017-12-24')


MSC17_1$term_week = lapply(MSC17_1$Date,FUN = function(x){
  x = as.Date(x)
  if(is.na(x)){
    return(NA)
  }else if(term_week_1_Start <= x & x <= term_week_1_End){
    return("1")
  }else if(term_week_2_Start <= x & x <= term_week_2_End){
    return("2")
  }else if(term_week_3_Start <= x & x <= term_week_3_End){
    return("3")
  }else if(term_week_4_Start <= x & x <= term_week_4_End){
    return("4")
  }else if(term_week_5_Start <= x & x <= term_week_5_End){
    return("5")
  }else if(term_week_6_Start <= x & x <= term_week_6_End){
    return("6")
  }else if(term_week_7_Start <= x & x <= term_week_7_End){
    return("7")
  }else if(term_week_8_Start <= x & x <= term_week_8_End){
    return("8")
  }else if(term_week_9_Start <= x & x <= term_week_9_End){
    return("9")
  }else if(term_week_10_Start <= x & x <= term_week_10_End){
    return("10")
  }else if(term_week_11_Start <= x & x <= term_week_11_End){
    return("11")
  }else if(term_week_12_Start <= x & x <= term_week_12_End){
    return("12")
  }else if(term_week_rev_Start <= x & x <= term_week_rev_End){
    return("rev")
  }else if(term_week_exam_Start <= x & x <= term_week_exam_End){
    return("exam")
  }else{
    return("Other")
  }
})

MSC17_1$term_week = factor(MSC17_1$term_week ,levels=c("1","2","3","4","5","6","7","8","9",
                                                       "10","11","12","rev","exam",NA),
                           ordered=TRUE,exclude = NULL)

table(MSC17_1$Week)%>% sum()
table(MSC17_1$term_week) %>% sum () # 29 diff from before term time


##################################################################
##################################################################
##################################################################

## next do 2017...

# 2017 sem 2 which is 2016/17 calender year


term_week_1_Start = as.Date('2017-01-23')
term_week_1_End = as.Date('2017-01-29')

term_week_2_Start = as.Date('2017-01-30')
term_week_2_End = as.Date('2017-02-05')

term_week_3_Start = as.Date('2017-02-06')
term_week_3_End = as.Date('2017-02-12')

term_week_4_Start = as.Date('2017-02-13')
term_week_4_End = as.Date('2017-02-19')

term_week_5_Start = as.Date('2017-02-20')
term_week_5_End = as.Date('2017-02-26')

term_week_6_Start = as.Date('2017-02-27')
term_week_6_End = as.Date('2017-03-05')

term_week_7_Start = as.Date('2017-03-06')
term_week_7_End = as.Date('2017-03-12')

term_week_F_Start = as.Date('2017-03-13')
term_week_F_End = as.Date('2017-03-26')

term_week_8_Start = as.Date('2017-03-27')
term_week_8_End = as.Date('2017-04-02')

term_week_9_Start = as.Date('2017-04-03')
term_week_9_End = as.Date('2017-04-09')

term_week_10_Start = as.Date('2017-04-10')
term_week_10_End = as.Date('2017-04-16')

term_week_11_Start = as.Date('2017-04-17')
term_week_11_End = as.Date('2017-04-23')

term_week_12_Start = as.Date('2017-04-24')
term_week_12_End = as.Date('2017-04-30')

term_week_rev_Start = as.Date('2017-05-01')
term_week_rev_End = as.Date('2017-05-07')

term_week_exam_Start = as.Date('2017-05-08')
term_week_exam_End = as.Date('2017-05-19')


MSC17_2$term_week = lapply(MSC17_2$Date,FUN = function(x){
  x = as.Date(x)
  if(is.na(x)){
    return(NA)
  }else if(term_week_1_Start <= x & x <= term_week_1_End){
    return("1")
  }else if(term_week_2_Start <= x & x <= term_week_2_End){
    return("2")
  }else if(term_week_3_Start <= x & x <= term_week_3_End){
    return("3")
  }else if(term_week_4_Start <= x & x <= term_week_4_End){
    return("4")
  }else if(term_week_5_Start <= x & x <= term_week_5_End){
    return("5")
  }else if(term_week_6_Start <= x & x <= term_week_6_End){
    return("6")
  }else if(term_week_7_Start <= x & x <= term_week_7_End){
    return("7")
  }else if(term_week_F_Start <= x & x <= term_week_F_End){
    return("F")
  }else if(term_week_8_Start <= x & x <= term_week_8_End){
    return("8")
  }else if(term_week_9_Start <= x & x <= term_week_9_End){
    return("9")
  }else if(term_week_10_Start <= x & x <= term_week_10_End){
    return("10")
  }else if(term_week_11_Start <= x & x <= term_week_11_End){
    return("11")
  }else if(term_week_12_Start <= x & x <= term_week_12_End){
    return("12")
  }else if(term_week_rev_Start <= x & x <= term_week_rev_End){
    return("rev")
  }else if(term_week_exam_Start <= x & x <= term_week_exam_End){
    return("exam")
  }else{
    return("Other")
  }
})

MSC17_2$term_week = factor(MSC17_2$term_week ,levels=c("1","2","3","4","5","6","7","8","9",
                                                       "10","11","12","rev","exam",NA),
                           ordered=TRUE,exclude = NULL)

table(MSC17_2$Week)%>% sum()
table(MSC17_2$term_week) %>% sum () 

## now 2018

# 2018 sem 2 which is 2017/18 calender year


term_week_1_Start = as.Date('2018-01-22')
term_week_1_End = as.Date('2018-01-28')

term_week_2_Start = as.Date('2018-01-29')
term_week_2_End = as.Date('2018-02-04')

term_week_3_Start = as.Date('2018-02-05')
term_week_3_End = as.Date('2018-02-11')

term_week_4_Start = as.Date('2018-02-12')
term_week_4_End = as.Date('2018-02-18')

term_week_5_Start = as.Date('2018-02-19')
term_week_5_End = as.Date('2018-02-25')

term_week_6_Start = as.Date('2018-02-26')
term_week_6_End = as.Date('2018-03-04')

term_week_7_Start = as.Date('2018-03-05')
term_week_7_End = as.Date('2018-03-11')

term_week_F_Start = as.Date('2018-03-12')
term_week_F_End = as.Date('2018-03-25')

term_week_8_Start = as.Date('2018-03-26')
term_week_8_End = as.Date('2018-04-01')

term_week_9_Start = as.Date('2018-04-02')
term_week_9_End = as.Date('2018-04-08')

term_week_10_Start = as.Date('2018-04-09')
term_week_10_End = as.Date('2018-04-15')

term_week_11_Start = as.Date('2018-04-16')
term_week_11_End = as.Date('2018-04-22')

term_week_12_Start = as.Date('2018-04-23')
term_week_12_End = as.Date('2018-04-29')

term_week_rev_Start = as.Date('2018-04-30')
term_week_rev_End = as.Date('2018-05-06')

term_week_exam_Start = as.Date('2018-05-07')
term_week_exam_End = as.Date('2018-05-19')


MSC18_2$term_week = lapply(MSC18_2$Date,FUN = function(x){
  x = as.Date(x)
  if(is.na(x)){
    return(NA)
  }else if(term_week_1_Start <= x & x <= term_week_1_End){
    return("1")
  }else if(term_week_2_Start <= x & x <= term_week_2_End){
    return("2")
  }else if(term_week_3_Start <= x & x <= term_week_3_End){
    return("3")
  }else if(term_week_4_Start <= x & x <= term_week_4_End){
    return("4")
  }else if(term_week_5_Start <= x & x <= term_week_5_End){
    return("5")
  }else if(term_week_6_Start <= x & x <= term_week_6_End){
    return("6")
  }else if(term_week_7_Start <= x & x <= term_week_7_End){
    return("7")
  }else if(term_week_F_Start <= x & x <= term_week_F_End){
    return("F")
  }else if(term_week_8_Start <= x & x <= term_week_8_End){
    return("8")
  }else if(term_week_9_Start <= x & x <= term_week_9_End){
    return("9")
  }else if(term_week_10_Start <= x & x <= term_week_10_End){
    return("10")
  }else if(term_week_11_Start <= x & x <= term_week_11_End){
    return("11")
  }else if(term_week_12_Start <= x & x <= term_week_12_End){
    return("12")
  }else if(term_week_rev_Start <= x & x <= term_week_rev_End){
    return("rev")
  }else if(term_week_exam_Start <= x & x <= term_week_exam_End){
    return("exam")
  }else{
    return("Other")
  }
})

MSC18_2$term_week = factor(MSC18_2$term_week ,levels=c("1","2","3","4","5","6","7","8","9",
                                                       "10","11","12","rev","exam",NA),
                           ordered=TRUE,exclude = NULL)

table(MSC18_2$Week)%>% sum()
table(MSC18_2$term_week) %>% sum () 


## now 2018

# 2018 sem 1 which is 2018/19 calender year


term_week_1_Start = as.Date('2018-09-10')
term_week_1_End = as.Date('2018-09-16')

term_week_2_Start = as.Date('2018-09-17')
term_week_2_End = as.Date('2018-09-23')

term_week_3_Start = as.Date('2018-09-24')
term_week_3_End = as.Date('2018-09-30')

term_week_4_Start = as.Date('2018-10-01')
term_week_4_End = as.Date('2018-10-07')

term_week_5_Start = as.Date('2018-10-08')
term_week_5_End = as.Date('2018-10-14')

term_week_6_Start = as.Date('2018-10-15')
term_week_6_End = as.Date('2018-10-21')

term_week_7_Start = as.Date('2018-10-22')
term_week_7_End = as.Date('2018-10-28')

term_week_8_Start = as.Date('2018-10-29')
term_week_8_End = as.Date('2018-11-04')

term_week_9_Start = as.Date('2018-11-05')
term_week_9_End = as.Date('2018-11-11')

term_week_10_Start = as.Date('2018-11-12')
term_week_10_End = as.Date('2018-11-18')

term_week_11_Start = as.Date('2018-11-19')
term_week_11_End = as.Date('2018-11-25')

term_week_12_Start = as.Date('2018-11-26')
term_week_12_End = as.Date('2018-12-02')

term_week_rev_Start = as.Date('2018-12-03')
term_week_rev_End = as.Date('2018-12-09')

term_week_exam_Start = as.Date('2018-12-10')
term_week_exam_End = as.Date('2018-12-21')


MSC18_1$term_week = lapply(MSC18_1$Date,FUN = function(x){
  x = as.Date(x)
  if(is.na(x)){
    return(NA)
  }else if(term_week_1_Start <= x & x <= term_week_1_End){
    return("1")
  }else if(term_week_2_Start <= x & x <= term_week_2_End){
    return("2")
  }else if(term_week_3_Start <= x & x <= term_week_3_End){
    return("3")
  }else if(term_week_4_Start <= x & x <= term_week_4_End){
    return("4")
  }else if(term_week_5_Start <= x & x <= term_week_5_End){
    return("5")
  }else if(term_week_6_Start <= x & x <= term_week_6_End){
    return("6")
  }else if(term_week_7_Start <= x & x <= term_week_7_End){
    return("7")
  }else if(term_week_F_Start <= x & x <= term_week_F_End){
    return("F")
  }else if(term_week_8_Start <= x & x <= term_week_8_End){
    return("8")
  }else if(term_week_9_Start <= x & x <= term_week_9_End){
    return("9")
  }else if(term_week_10_Start <= x & x <= term_week_10_End){
    return("10")
  }else if(term_week_11_Start <= x & x <= term_week_11_End){
    return("11")
  }else if(term_week_12_Start <= x & x <= term_week_12_End){
    return("12")
  }else if(term_week_rev_Start <= x & x <= term_week_rev_End){
    return("rev")
  }else if(term_week_exam_Start <= x & x <= term_week_exam_End){
    return("exam")
  }else{
    return("Other")
  }
})

MSC18_1$term_week = factor(MSC18_1$term_week ,levels=c("1","2","3","4","5","6","7","8","9",
                                                       "10","11","12","rev","exam",NA),
                           ordered=TRUE,exclude = NULL)

table(MSC18_1$Week)%>% sum()
table(MSC18_1$term_week) %>% sum () 


## now 2019

# 2019 sem 2 which is 2018/19 calender year


term_week_1_Start = as.Date('2019-01-21')
term_week_1_End = as.Date('2019-01-27')

term_week_2_Start = as.Date('2019-01-28')
term_week_2_End = as.Date('2019-02-03')

term_week_3_Start = as.Date('2019-02-04')
term_week_3_End = as.Date('2019-02-10')

term_week_4_Start = as.Date('2019-02-11')
term_week_4_End = as.Date('2019-02-17')

term_week_5_Start = as.Date('2019-02-18')
term_week_5_End = as.Date('2019-02-24')

term_week_6_Start = as.Date('2019-02-25')
term_week_6_End = as.Date('2019-03-03')

term_week_7_Start = as.Date('2019-03-04')
term_week_7_End = as.Date('2019-03-10')

term_week_8_Start = as.Date('2019-03-25')
term_week_8_End = as.Date('2019-03-31')

term_week_9_Start = as.Date('2019-04-01')
term_week_9_End = as.Date('2019-04-07')

term_week_10_Start = as.Date('2019-04-08')
term_week_10_End = as.Date('2019-04-14')

term_week_11_Start = as.Date('2019-04-15')
term_week_11_End = as.Date('2019-04-21')

term_week_12_Start = as.Date('2019-04-22')
term_week_12_End = as.Date('2019-04-28')

term_week_rev_Start = as.Date('2019-04-29')
term_week_rev_End = as.Date('2019-05-05')

term_week_exam_Start = as.Date('2019-05-06')
term_week_exam_End = as.Date('2019-05-18')


MSC19_2$term_week = lapply(MSC19_2$Date,FUN = function(x){
  x = as.Date(x)
  if(is.na(x)){
    return(NA)
  }else if(term_week_1_Start <= x & x <= term_week_1_End){
    return("1")
  }else if(term_week_2_Start <= x & x <= term_week_2_End){
    return("2")
  }else if(term_week_3_Start <= x & x <= term_week_3_End){
    return("3")
  }else if(term_week_4_Start <= x & x <= term_week_4_End){
    return("4")
  }else if(term_week_5_Start <= x & x <= term_week_5_End){
    return("5")
  }else if(term_week_6_Start <= x & x <= term_week_6_End){
    return("6")
  }else if(term_week_7_Start <= x & x <= term_week_7_End){
    return("7")
  }else if(term_week_8_Start <= x & x <= term_week_8_End){
    return("8")
  }else if(term_week_9_Start <= x & x <= term_week_9_End){
    return("9")
  }else if(term_week_10_Start <= x & x <= term_week_10_End){
    return("10")
  }else if(term_week_11_Start <= x & x <= term_week_11_End){
    return("11")
  }else if(term_week_12_Start <= x & x <= term_week_12_End){
    return("12")
  }else if(term_week_rev_Start <= x & x <= term_week_rev_End){
    return("rev")
  }else if(term_week_exam_Start <= x & x <= term_week_exam_End){
    return("exam")
  }else{
    return("Other")
  }
})

MSC19_2$term_week = factor(MSC19_2$term_week ,levels=c("1","2","3","4","5","6","7","8","9",
                                                       "10","11","12","rev","exam",NA),
                           ordered=TRUE,exclude = NULL)

table(MSC19_2$Week)%>% sum()
table(MSC19_2$term_week) %>% sum () 


##########################################################
###########################################################
##########################################################


#function to create resource number of tutors column
resourcesfunction <- function(tutor_term, MSC_general){
  tutor_term<-rbind.data.frame(tutor_term, colnames(tutor_term))
  MSC_general$number_tutors =rep(NA, nrow(MSC_general))
  for(k in 1:(nrow(MSC_general))){        #loop for each student 
    for(j in 1:(ncol(tutor_term))){   # loop for each day
      for(i in 1:(nrow(tutor_term))){   # loop for every row in for time of day
        if(match(MSC_general$Day[k], tutor_term[nrow(tutor_term),j],nomatch=0,incomparables = NULL) && match(MSC_general$Hour[k],tutor_term[i,1],nomatch = 0,incomparables = NULL)){  
          MSC_general$number_tutors[k] <- tutor_term[i,j] #total number of tutors for that hour
        } 
      } 
    }}   
  return(MSC_general)
}

#create each resource count matrix using resourcesfunction
setwd("/Tutor timetables")

# Create number of tutors variable
tutor19_2=read.csv("Tutors 2019 2.csv", header=TRUE)
MSC_192 <- resourcesfunction(tutor19_2, MSC19_2)

tutor18_1=read.csv("Tutors 2018 1.csv", header=TRUE)
MSC_181 <- resourcesfunction(tutor18_1, MSC18_1)

tutor18_2=read.csv("Tutors 2018 2.csv", header=TRUE)
MSC_182 <- resourcesfunction(tutor18_2, MSC18_2)

tutor17_2=read.csv("Tutors 2017 2.csv", header=TRUE)
MSC_172 <- resourcesfunction(tutor17_2, MSC17_2)

tutor17_1=read.csv("Tutors 2017 1.csv", header=TRUE)
MSC_171 <- resourcesfunction(tutor17_1, MSC17_1)

tutor16_2=read.csv("Tutors 2016 2.csv", header=TRUE)
MSC_162 <- resourcesfunction(tutor16_2, MSC16_2)

tutor16_1=read.csv("Tutors 2016 1.csv", header=TRUE)
MSC_161 <- resourcesfunction(tutor16_1, MSC16_1)

tutor15_2=read.csv("Tutors 2015 2.csv", header=TRUE)
MSC_152 <- resourcesfunction(tutor15_2, MSC15_2)

tutor15_1=read.csv("Tutors 2015 1.csv", header=TRUE)
MSC_151 <- resourcesfunction(tutor15_1, MSC15_1)

# Back to main file "Reformatting unedited"
