run_analysis <- function(){
  #Loads useful libraries 
  library(plyr)
  library(dplyr)  
  library(stringr)
  library(tidyr)
  
  #Loads external scripts
  source("getTidyData.R")
  source("getTidyFeatures.R")
  
  #Saves the current directory
  oldwd <- getwd()
  #Goes to the directory with the original data
  setwd("UCI HAR Dataset")

  #Sets useful variables 
  featureFile <- "features.txt"
  activityFile <- "activity_labels.txt"
  xTrainFile <- "X_train.txt"
  yTrainFile <- "y_train.txt" 
  xTestFile <- "X_test.txt"
  yTestFile <- "y_test.txt"
  subjectTrainFile <- "subject_train.txt"
  subjectTestFile <- "subject_test.txt"
  dirTrain <- "train"
  dirTest <- "test"
  dirBack <- ".."
  xTrainData <- NULL
  xTestData <- NULL
  tidyData <- NULL  
  features <- c()
  
  tryCatch({
    if(file.exists(featureFile)){
      #Loads the 'features' data and makes it tidy 
      features <- getTidyFeatures(featureFile)
    }
    
    if(file.exists(activityFile)){
      #Loads the information from the file activity_labels.txt to the 'activities' variable
      activities <- read.table(activityFile, sep=" ", col.names = c("id","description"))
    }
    
    if(dir.exists(dirTrain)){
      #Go to the "train" directory
      setwd(dirTrain)
      if(file.exists(yTrainFile) && file.exists(xTrainFile) && file.exists(subjectTrainFile)){
        #Loads the 'X_train.txt' file into a data frame and makes it tidy 
        xTrainData <- getTidyData(xTrainFile,yTrainFile,subjectTrainFile, activities, features)
        #Adds a column to make it possible knowing the data type (test/train)
        xTrainData <- cbind(datatype="train",xTrainData)
      }
      #Sets current directory to previous directory
      setwd(dirBack)
    }
    
    if(dir.exists(dirTest)){
      #Go to the "test" directory
      setwd(dirTest)
      if(file.exists(yTestFile) && file.exists(xTestFile) && file.exists(subjectTestFile)){
        #Loads the 'X_test.txt' file into a data frame and makes it tidy 
        xTestData <- getTidyData(xTestFile,yTestFile,subjectTestFile, activities, features)
        #Adds a column to make it possible knowing the data type (test/train)
        xTestData <- cbind(datatype="test",xTestData)
      }
      #Sets current directory to previous directory
      setwd(dirBack)
    }
    
    #Merges training and test sets (requirement 1)
    tidyData<-rbind(xTrainData,xTestData)
    #Creates a new tidy data set with the average of each feature for each activity and each subject
    tidyDataSummarised <- ddply(tidyData,.(volunteernumber,activity,feature),summarise,mean=mean(value))
    
    #Sets the current directory to root directory
    setwd(dirBack)
    
    #Writes the data for submission
    write.table(tidyDataSummarised,"tidydata.txt",row.names = FALSE)
    
  }, finally={
    #In any case sets the current directory back to its initial value
    setwd(oldwd)
  })
  
  #Returns the tidy data set required for analysis
  tidyDataSummarised
}