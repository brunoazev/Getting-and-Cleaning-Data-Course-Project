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
        xTrainData <- getTidyData(xTrainFile,yTrainFile,subjectTrainFile, activities, features)
        xTrainData <- cbind(datatype="train",xTrainData)
      }
      
      setwd(dirBack)
    }
    
    if(dir.exists(dirTest)){
      #Go to the "test" directory
      setwd(dirTest)
      if(file.exists(yTestFile) && file.exists(xTestFile) && file.exists(subjectTestFile)){
        xTestData <- getTidyData(xTestFile,yTestFile,subjectTestFile, activities, features)
        xTestData <- cbind(datatype="test",xTestData)
      }
      
      setwd(dirBack)
    }
    
    tidyData<-rbind(xTrainData,xTestData)
    tidyDataSummarised <- ddply(tidyData,.(volunteernumber,activity,feature),summarise,mean=mean(value))
    
    setwd(dirBack)
    
    #Code used to generate the data in csv format if it is the case.
    #----------------------------------------------------------------------------
    #if(!dir.exists("tidydata"))
    #  dir.create("tidydata")
    #setwd("tidydata")
    #write.table(tidyData,paste("tidydata_",gsub("[[:punct:]]","",format(Sys.time(), "%x%X")),".csv",sep=""),sep=";")
    #
    #write.table(tidyDataSummarised,paste("tidydata_summarised_",gsub(":","",format(Sys.time(), "%X")),".csv",sep=""),sep=";")
    #setwd(dirback)
    #----------------------------------------------------------------------------
    
    #Writes the data for submission
    write.table(tidyDataSummarised,"tidydata.txt",row.names = FALSE)
    
  }, finally={
    setwd(oldwd)
  })
  
  tidyDataSummarised
}