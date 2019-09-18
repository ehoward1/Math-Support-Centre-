require(lubridate)
require(magrittr)
require(bartMachine)
require(car)
require(caret)
require(directlabels)
require(randomForest)
require(earth)
require(kernlab)
require(kknn)
require(nnet)
require(pls)
require(reshape2)
require(viridis)
require(magrittr)
require(R.utils)
require(dplyr)

MSC = read.csv("MSC.csv",header=TRUE)
# Remove columns that are not relevant for predicting wait time
MSC = subset(MSC, select=-c(X, tutor_start_time, tutor_finish_time, Date, Week, with_tutor, pause_tutor))
#Check formatting
MSC = subset(MSC, term_week != "NA", Day != "NA")
MSC = subset(MSC, number_tutors != 0)  
MSC$Year = MSC$Year %>% factor(levels=c("2015","2016","2017","2018", "2019"), ordered=TRUE) 
MSC$Day = MSC$Day %>% factor(levels=c("Monday","Tuesday",
                                      "Wednesday","Thursday",
                                      "Friday"), ordered=TRUE) 
MSC$Hour = as.numeric(MSC$Hour)
MSC = subset(MSC, wait_time<90, Hour<21)
MSC$Minute = as.numeric(MSC$Minute)
MSC$Semester = MSC$Semester %>% factor(levels=c("1","2"), ordered=TRUE)
MSC$term_week = MSC$term_week %>% factor(levels=c("1", "2", "3", "4", "5",
                                                  "6", "7", "8", "9", "10",
                                                  "11", "12", "rev", "exam"),
                                         ordered=TRUE)
MSC$number_tutors = as.numeric(MSC$number_tutors)
MSC$Month = MSC$Month %>% factor(levels=c("1", "2", "3", "4", "5",
                                          "6", "7", "8", "9", "10",
                                          "11", "12"),
                                 ordered=TRUE)



###################full prediction model####################

prediction_function <- function(dataset){ # dataset boost is for xgboost 
  
  set.seed(123)
  folds = createFolds(1:nrow(dataset), k = 10, list = FALSE)
  pb <- txtProgressBar(min = 0, max = 10, style = 3)
  
  # Vectors to store error for other prediction methods
  pred_bm = vector("numeric")
  pred_rf = vector("numeric")
  pred_pcr = vector("numeric")
  pred_kknn = vector("numeric")
  pred_svm = vector("numeric")
  pred_nnet = vector("numeric")
  pred_earth = vector("numeric")
  grades = vector("numeric")
  
  # Loop through the folds for Random Forest, PC regression etc
  for(i in 1:10){
    
    Sys.sleep(0.1)
    setTxtProgressBar(pb, i)
    
    # Setting up data
    train = dataset[folds!=i,] %>% data.frame %>% na.omit
    test = dataset[folds==i,] %>% data.frame %>% na.omit
    
    # BART
    #bm = bartMachine(train[,-1], train[,1], seed = 123, alpha = 0.95, num_burn_in = 400,
    #                num_tree = 100, num_rand_samps_in_library = 20000, k = 2, q = 0.9, nu = 3)
    #pred_bm = c(pred_bm, predict(bm, test[,-1]))
    
    # Random Forest (RF)
    rf = randomForest(train[,-1], train[,1], ntree = 100)
    pred_rf = c(pred_rf, predict(rf, test[,-1]))
    
    # Principle Components Regression (PCR)
    pcr = pcr(wait_time~., data = train)
    var_exp = compnames(pcr, explvar = TRUE)
    var_e = unlist(strsplit(var_exp, "[ (]")) %>% as.numeric() %>%  setdiff(c(1:150, NA))
    var_total = 0
    
    # Calculating number of variables to include based on variation explained
    for(j in 1:length(var_e))
    {
      var_total = var_total + var_e[j]
      if(var_e[j] < 1 || var_total > 90)
      {
        n_comp = j
        break
      }
    }
    pred_pcr = c(pred_pcr, predict(pcr, test[,-1], ncomp = n_comp))
    
    # K-Nearest Neighbours (KNN)
    kknn = train.kknn(wait_time ~., kmax = 15, distance = 1, data = train)
    pred_kknn = c(pred_kknn, predict(kknn, test[,-1]))
    
    # Neural Network (NN)
    my.grid = expand.grid(.decay = c(0.05, 0.5, 0.75), .size = c(4, 9))
    nnet = train(wait_time~., data = train, linout = 1, 
                 method = "nnet", maxit = 500, tuneGrid = my.grid, trace = FALSE) 
    pred_nnet = c(pred_nnet, predict(nnet, test[,-1]))
    
    # Support Vector Machine (SVM)
    svm = ksvm(wait_time ~., data = train, C = 5)
    pred_svm = c(pred_svm, predict(svm, test[,-1]))
    
    # Multivariate Adaptive Regression Splines
    earth = train(wait_time~., data = train, method = "earth",
                  tuneGrid = data.frame(degree = c(1,2), nprune = 5)) 
    pred_earth = c(pred_earth, predict(earth, test[,-1]))
    
    grades = c(grades, test$wait_time)
  }
  
  # Calculating the error for each method
  error_rf = sum(abs(pred_rf - grades))/nrow(dataset) 
  error_pcr = sum(abs(pred_pcr - grades))/nrow(dataset) 
  error_bm = sum(abs(pred_bm - grades))/nrow(dataset) 
  error_earth = sum(abs(pred_earth - grades))/nrow(dataset) 
  error_kknn = sum(abs(pred_kknn - grades))/nrow(dataset) 
  error_nnet = sum(abs(pred_nnet - grades))/nrow(dataset) 
  error_svm = sum(abs(pred_svm - grades))/nrow(dataset) 
  
  # Returning Values
  my_list <- list("MAE_bm" = error_bm, "MAE_rf" = error_rf, "MAE_pcr" = error_pcr, 
                  "MAE_kknn" = error_kknn, "MAE_nnet" = error_nnet, 
                  "MAE_svm" = error_svm, "MAE_earth" = error_earth)   
  
  return(my_list)
}
close(pb)

####################################################
hist(MSC$wait_time)
# MSC wait time is exponentially distributed 
# Confirm using
require(DescTools)
BoxCoxLambda(MSC$wait_time)
# 0 is a log transformation
# Convert to normal distribution for running prediction modelling


MSC$wait_time = MSC$wait_time + 1
MSC$wait_time=log(MSC$wait_time)
hist(MSC$wait_time)

# Create two new datasets based on term weeks 1-12 and the second set only revision and exam weeks
MSC1_12 = filter(MSC, term_week!= "rev" & term_week !="exam")
MSC1_12 = na.omit(MSC1_12) # some methods do not work with Nas
# MSCrev = filter(MSC, term_week== "rev" | term_week =="exam") # Revision and exam weeks are hard to predict

#######################################
# Run full prediction to see what is the best method 
# This takes a long time to run
# full_pred = prediction_function(MSC)
# full_pred

#########################################

# For a quicker prediction....randomforest works well
rf = randomForest(MSC1_12[,-1], MSC1_12[,1], ntree = 100)

#Convert wait time and predicted wait time back
pred = exp(rf$predicted)-1
actual_times = exp(rf$y)-1

# Gives error in min (converted back from the log transformation)
sum(abs(actual_times-pred))/nrow(MSC1_12)
cor(pred, actual_times)

# Which variables were most important in the prediction 
imp=importance(rf)
varImpPlot(rf)

# Plot actual versus predicted waiting times
df = as.data.frame(pred)
df = cbind.data.frame(df, actual_times, Year = MSC1_12$Year, term = MSC1_12$term_week)

ggplot(df, aes(pred, actual_times)) + geom_point(aes(col = Year)) +
  scale_x_continuous(breaks=seq(0, 100, 20)) + 
  theme_bw() + geom_abline(color="blue") +
  scale_y_continuous(breaks=seq(0, 100, 20)) + 
  labs(x="Predicted Waiting Times", y="Actual\nWaiting\nTimes") + 
  theme(
    plot.title = element_text(lineheight=0, color='black', face="bold",size=13),
    axis.title.x=element_text(angle=0, color='black',face='bold', size=11),
    axis.title.y=element_text(angle=0, vjust = 1, color='black',face='bold', size=11),
    axis.text.x=element_text(angle=0, color='black', size=11),
    axis.text.y=element_text(angle=0, color='black',  size=11)
  ) 

###########################################################
# Predict next semester's waiting times for every time point 
############################################################

# Create a dataset covering every time point of next semester

# Creats test dataset for one week
test = data.frame(wait_time = 0, Month = 9, Year = 2019, Day = rep(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), each = 660),
                  Hour = c(rep(10:20,each = 60)), Minute = rep(0:59,55), Semester = 1, Hour_9 = 0, Hour_10 = NA, Hour_11 = NA, Hour_12 = NA, Hour_13 = NA,
                  Hour_14 = NA, Hour_15 = NA, Hour_16 = NA, Hour_17 = NA, Hour_18 = NA, Hour_19 = NA, Hour_20 = 0, Year_15 = 0, Year_16 = 0, Year_17 = 0, Year_18 = 0, Year_19 = 1,
                  Month_Sep = 1, Month_Oct = 0, Month_Nov = 0, Month_Dec = 0, Month_Jan = 0, Month_Feb = 0, Month_Mar = 0, Month_Apr = 0, Month_May = 0,
                  Day_Mon = c(rep(1,660), rep(0,2640)), Day_Tue = c(rep(0,660), rep(1,660), rep(0,1980)), Day_Wed = c(rep(0,1320), rep(1,660), rep(0,1320)),
                  Day_Thur = c(rep(0,1980), rep(1,660), rep(0,660)), Day_Fri = c(rep(0,2640), rep(1,660)), change_over = 0, term_week = 1)

test$Hour_9 = ifelse(test$Hour==9,1,0)
test$Hour_10 = ifelse(test$Hour==10,1,0)
test$Hour_11 = ifelse(test$Hour==11,1,0)
test$Hour_12 = ifelse(test$Hour==12,1,0)
test$Hour_13 = ifelse(test$Hour==13,1,0)
test$Hour_14 = ifelse(test$Hour==14,1,0)
test$Hour_15 = ifelse(test$Hour==15,1,0)
test$Hour_16 = ifelse(test$Hour==16,1,0)
test$Hour_17 = ifelse(test$Hour==17,1,0)
test$Hour_18 = ifelse(test$Hour==18,1,0)
test$Hour_19 = ifelse(test$Hour==19,1,0)
test$Hour_20 = ifelse(test$Hour==20,1,0)

# Extend the dataset for all 12 term weeks
testb = test 
testb$term_week = 2
testc = test 
testc$term_week = 3
testd = test 
testd$term_week = 4
teste = test 
teste$term_week = 5
testf = test 
testf$term_week = 6
testg = test 
testg$term_week = 7
testh = test 
testh$term_week = 8
testi = test 
testi$term_week = 9
testj = test 
testj$term_week = 10
testk = test 
testk$term_week = 11
testl = test 
testl$term_week = 12

# Combine term weeks to create one test data for the semester
test_sem = rbind.data.frame(test, testb, testc, testd, teste, testf, testg, testh, testi, testj, testk, testl)

# Suggested tutor hours
tutor_term = read.csv("example tutor timetable.csv", header = TRUE)

#function to create resource number of tutors column
resourcesfunction = function(tutor_term, MSC_general){
  tutor_term = rbind.data.frame(tutor_term, colnames(tutor_term))
  MSC_general$number_tutors = rep(NA, nrow(MSC_general))
  for(k in 1:(nrow(MSC_general))){        #loop for each student 
    for(j in 1:(ncol(tutor_term))){   # loop for each day
      for(i in 1:(nrow(tutor_term))){   # loop for every row in for time of day
        if(match(MSC_general$Day[k],tutor_term[nrow(tutor_term),j],nomatch=0,incomparables = NULL) && match(MSC_general$Hour[k],tutor_term[i,1],nomatch = 0,incomparables = NULL)){  
          MSC_general$number_tutors[k] = tutor_term[i,j] # total number of tutors for that hour
        } 
      } 
    }}   
  return(MSC_general)
}

test_semester = resourcesfunction(tutor_term, test_sem)
head(test_semester)

#Match variable type to MSC
test_semester$term_week = test_semester$term_week %>% factor(levels=c("1", "2", "3", "4", "5",
                                                      "6", "7", "8", "9", "10",
                                                      "11", "12", "rev", "exam"),
                                             ordered=TRUE)

test_semester$Year = test_semester$Year %>% factor(levels=c("2015","2016","2017","2018", "2019"), ordered=TRUE) 
test_semester$Day = test_semester$Day %>% factor(levels=c("Monday","Tuesday",
                                          "Wednesday","Thursday",
                                          "Friday"), ordered=TRUE) 
test_semester$Semester = test_semester$Semester %>% factor(levels=c("1","2"), ordered=TRUE)
test_semester$number_tutors = as.numeric(test_semester$number_tutors)
test_semester$Month = test_semester$Month %>% factor(levels=c("1", "2", "3", "4", "5",
                                              "6", "7", "8", "9", "10",
                                              "11", "12"),
                                     ordered=TRUE)
test_semester$Minute = as.numeric(test_semester$Minute)
test_semester$Hour = as.numeric(test_semester$Hour)
test_semester = na.omit(test_semester)

# Can't predict with queue as we don't know future queue so remove and predict again
MSC1_12pred = subset(MSC1_12, select=-c(queue1))
rfpred = randomForest(MSC1_12pred[,-1], MSC1_12pred[,1], ntree = 100)
preds = predict(rfpred, test_semester[,-1])
test_semester$wait_time = exp(preds)-1

# Split the subset so we can view wee by week
test_1 = subset(test_semester, term_week == "1")
test_2 = subset(test_semester, term_week == "2")
test_3 = subset(test_semester, term_week == "3")
test_4 = subset(test_semester, term_week == "4")
test_5 = subset(test_semester, term_week == "5")
test_6 = subset(test_semester, term_week == "6")
test_7 = subset(test_semester, term_week == "7")
test_8 = subset(test_semester, term_week == "8")
test_9 = subset(test_semester, term_week == "9")
test_10 = subset(test_semester, term_week =="10")
test_11 = subset(test_semester, term_week == "11")
test_12 = subset(test_semester, term_week == "12")

prediction = function(data, term_week){
  ggplot(data, aes(factor(Hour), wait_time))+
    geom_boxplot(aes(fill=factor(Hour))) + 
    facet_wrap(~Day, dir="v", strip.position = "right")+
    xlab(paste("Time of Day for Week", term_week)) + 
    ylab("Wait\nTime")+
    theme_bw()+
    theme(
      axis.title.x=element_text(angle=0, color='black', face='bold', size=10),
      axis.title.y=element_text(angle=0, color='black', face='bold', size=10),
      axis.text.x=element_text(angle=0, color='black', size=8, vjust=0.5),
      axis.text.y=element_text(angle=0, color='black',  size=8)
    ) +
    theme(legend.position = "none")
}

# See each term week's predicted waiting time
prediction(test_1, 1)
prediction(test_2, 2)
prediction(test_3, 3)
prediction(test_4, 4)
prediction(test_5, 5)
prediction(test_6, 6)
prediction(test_7, 7)
prediction(test_8, 8)
prediction(test_9, 9)
prediction(test_10, 10)
prediction(test_11, 11)
prediction(test_12, 12)
