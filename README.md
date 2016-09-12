# **Getting and Cleaning Data - Course Project**
*This course is part of the Data Science Specialization offered by Johns Hopkins through Coursera.*

The purpose of this project is to demonstrate my ability to collect, work with, and clean a data set.  The goal was to prepare tidy data that can be used for later analysis. 
### Introduction
---

Coursera intro about wearable computing:
> One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

A quick overview about the experiment in which the raw data has been collected:

>The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

### Repo included files
---
- **UCI HAR Dataset**: folder containing the raw data about the experiment described above;
- **run_analysis.R**: main script developed in R to perform analysis on the raw data and generate a tidy data text file;
- **getTidyFeatures.R**: script sourced in run_analysis.R, containing a function for transforming the data (features.txt) describing the features;
- **getTidyData.R**: script sourced in run_analysis.R, containing a function for transforming the data corresponding to the values measured in the experiment;
- **codebook.md**: the code book, a dictionary which indicates all the variables and relevant information to understand the data;
- **README.md**: this file you are reading, which explains the analysis files and all processes involved;
- **tidydata.txt**: the tidy data generated by the run_analysis.R script.

### UCI HAR Dataset (Folder)
---
For more information about the raw data, please consider reading the README.txt included in this folder.

### run_analysis.R (File)
---
This is the main script for generating the tidy data. 

It is intended to perform the following steps on the raw data:

1. Merges the training and the test sets to create one data set.
2. Extract only the measurements on the mean and standard deviation for each measurement.
3. Use descriptive activity names to name the activities in the data set
4. Appropriately label the data set with descriptive variable names.
5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

To sum up, the goal of this script is to merge the X_train and X_test data generating a single tidy dataset. 

##### Fixing the features data from features.txt file
The first part of the script fixes the features data, loaded from the file *features.txt*.
The data was loaded with the following command, where the variable "featureFile" is the path of features.txt:
```R
features <- read.table(featureFile, sep=" ", col.names = c("featureid","feature"))
```
 To make the features data become more descriptive and easier to understand, it was created a function in the **getTidyFeatures.R** file which is sourced in the beggining of the code. This function gets the second column of the data in *features.txt* which contains the name of the feature and divides it into 4 new columns, according to the information that could be extracted considering the information of the features_info.txt file included in the UCI HAR Dataset folder.
*More information of how the data was exctracted are described in the code section. All code was comented in order to make it clear and understandable for the reader*.


Let's see the first four rows of the data before and after the transformation:

Before
```R
head(features,4)
```
<table>
<thead>
<tr><td></td><td>featureid</td><td>feature</td></tr>
</thead>
<tr><td>1</td><td>1</td><td>tBodyAcc-mean()-X</td></tr>
<tr><td>2</td><td>2</td><td>tBodyAcc-mean()-Y</td></tr>
<tr><td>2</td><td>3</td><td> tBodyAcc-mean()-Z</td></tr>
<tr><td>2</td><td>4</td><td>tBodyAcc-std()-X</td></tr>
</table>

After

<table>
<thead>
<tr><td></td><td>featureid</td><td>feature</td><td>measurement</td><td>description</td><td>calculation</td><td>direction</td></tr>
</thead>
<tr><td>1</td><td>1</td><td>tBodyAcc-mean()-X</td><td>time</td><td>Body Accelerometer - X - mean - time</td><td>mean</td><td>X</td></tr>
<tr><td>2</td><td>2</td><td>tBodyAcc-mean()-Y</td><td>time</td><td>Body Accelerometer - Y - mean - time</td><td>mean</td><td>Y</td></tr>
<tr><td>2</td><td>3</td><td> tBodyAcc-mean()-Z</td><td>time</td><td>Body Accelerometer - Z - mean - time</td><td>mean</td><td>Z</td></tr>
<tr><td>2</td><td>4</td><td>tBodyAcc-std()-X</td><td>time</td><td>Body Accelerometer - X - standard deviation - time</td><td>standard deviation</td><td>X</td></tr>
</table>

##### Putting all together
The X_train.txt and X_test.txt files contain data of same structure with only numeric values corresponding to the observations for the different features measured on the experiment (e.g: the body gyroscope mean time calculated on X axis). Each column of these data represents the feature measured and each row corresponds to an observation for some volunteer activity performed (e.g: volunteer number 1 sitting at some period of time).
The following table shows part (first two rows and first two columns) of the X_train.txt file after loaded into R (assume we named the data frame as "xTrainData"):
```R
head(xTrainData[1:2,1:2])
```
<table>
<thead>
<tr><td></td><td>V1</td><td>V2</td></tr>
</thead>
<tr><td>1</td><td>0.2885845</td><td>-0.02029417</td></tr>
<tr><td>2</td><td>0.2784188</td><td>-0.01641057</td></tr>
</table>


The y_train.txt and y_test.txt contain labels for X_train and X_test that represent the activity performed on each observation (WALKING, SITTING, etc.). Finally, the subject_train.txt and subject_test.txt files contain representations for the volunteer who participated in the experiment. The **getTidyData.R** file has the function to get all these information gathered together producing a more readable data for both X_train and X_test. Besides that, as needed a new column was added to keep the number of record of each activity performed. All the process for transforming the X_train and X_test 
At this point, the data is presented as following, for the first two rows and first five columns for volunteer number one:

```R
head(xTrainData[1:2,1:5])
```
<table>
<thead>
<tr><td></td><td>volunteernumber</td><td>recordnumber</td><td>activity</td><td>V1</td><td>V2</td></tr>
</thead>
<tr><td>1</td><td>1</td><td>1</td><td>STANDING</td><td>0.2885845</td><td>-0.02029417</td></tr>
<tr><td>2</td><td>1</td><td>2</td><td>STANDING</td><td>0.2784188</td><td>-0.01641057</td></tr>
</table>

The same data for volunteer number three:

<table>
<thead>
<tr><td></td><td>volunteernumber</td><td>recordnumber</td><td>activity</td><td>V1</td><td>V2</td></tr>
</thead>
<tr><td>1</td><td>1</td><td>1</td><td>STANDING</td><td>0.2885845</td><td>-0.02029417</td></tr>
<tr><td>2</td><td>1</td><td>2</td><td>STANDING</td><td>0.2784188</td><td>-0.01641057</td></tr>
</table>

Another process in tyiding the data was transposing the features (V1:V561) columns to rows, using the gather function from tidyr library:
```R
xTrainData <- xTrainData %>% gather(featureid,value,V1:V561)
```
Look at how the data is now presented:

<table>
<thead>
<tr><td></td><td>volunteernumber</td><td>recordnumber</td><td>activity</td><td>featureid</td><td>value</td></tr>
</thead>
<tr><td>1</td><td>1</td><td>1</td><td>STANDING</td><td>V1</td><td>0.2885845</td></tr>
<tr><td>2</td><td>1</td><td>1</td><td>STANDING</td><td>V2</td><td>-0.02029417</td></tr>
</table>
*The data above was filtered to show the most relevant information to comprehension of the transpose process*.

Finally, the last process was replacing the column featureid with the respective feature description from the features data frame. For that, it was necessary to remove the 'V' character from it, cast it to integer, and then match the featureid of both data frames (xTrainData and features) using *left_join()* function. This process will join the two data frames, so the last code line in getTidyData function selects only the relevant columns.

All the process for making xTrainData tidy is also performed to xTestData (X_test.txt), and then the both data are merged into one data set. Then it is calculated the average of each variable for each activity and each subject, and the result is wrote to the file **tidydata.txt** for submission.

The final tidy data looks like this:

<table>
<thead>
<tr><td></td><td>volunteernumber</td><td>activity</td><td>feature</td><td>mean</td></tr>
</thead>
<tr><td>1</td><td>1</td><td>LAYING</td><td>Body Accelerometer - X - mean - frequency</td><td> -0.9390991</td></tr>
<tr><td>2</td><td>1</td><td>LAYING</td><td>Body Accelerometer - X - mean - time</td><td>0.2215982</td></tr>
</table>

### The code
---
Initialization:
```R
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
```

Processing, merging, and generating the required tidy data:
``` R
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
```
getTidyFeatures():
``` R
getTidyFeatures <- function(featureFile){
  #Loads the information from the file features.txt to the new 'features' dataframe
  features <- read.table(featureFile, sep=" ", col.names = c("featureid","feature"))
  #Creates a new column which extract the feature's measurement as described 
  #on the features_info.txt file. file
  features["measurement"] <- vector()
  #Get already defined measurement units in feature name
  measurmentIndexes <- grep("^[a-z]+[A-Z]+",features$feature)
  #Updates the measurement column with the respective measurement unit
  features$measurement[measurmentIndexes] <- ifelse(substr(features$feature[measurmentIndexes],1,1) == "f","frequency","time")
  #Changes the class of the 'measurement' column to 'factor'
  features$measurement <- as.factor(features$measurement)
  #Adds a new factor level for missing variables. This will be treated as "degree" unit
  #Missing values are intended to represent the angle measurement unit
  #First adds a new level named "degree" to make it able to update the missing values with this factor
  levels(features$measurement) <- c(levels(features$measurement),"degree")
  features$measurement[is.na(features$measurement)] <- "degree"
  #Creates new columns for the estimated variables and directions
  #   *directions will not be used, but it was decided to separate it
  #First splits the feature by the '-' character, returning three columns (description, calculation and direction)
  newColumns <- str_split_fixed(as.character(features$feature),"-",3)
  #Updates the columns names 
  colnames(newColumns) <- c("description","calculation","direction")
  #Adds the new columns created to 'features' dataframe
  features <- cbind(features,newColumns)
  #Changes the class of the 'description' variable
  features$description <- as.character(features$description)
  #Gets the main description of the feature in a descriptive form (eg: Body Accelerometer)
  features$description[grep("^[f|t][A-Z]",features$description)] <- sapply(str_split(features$description[grep("^[f|t][A-Z]",features$description)],"^t|f"),function(x){ retorno <- unlist(x); retorno[2] })
  #Removes repeated sequence of words. eg: BodyBody
  features$description <- str_replace(gsub('([[:upper:]])', ' \\1', features$description),"\\b(\\w+)\\b(?=.*\\b\\1\\b)\\s+","")
  #Updates the abbreviations to full names
  features$description <- gsub("Gyro","Gyroscope",features$description)
  features$description <- gsub("Mag","Magnitude",features$description)
  features$description <- gsub("Acc","Accelerometer",features$description)
  
  #Eliminates unnecessary text from 'calculation' variable
  features$calculation <- str_replace(features$calculation,"\\(.*\\)","")
  #Eliminates unnecessary text from 'direction' variable. Keeps only X, Y and Z.
  features$direction <- as.factor(str_replace(features$direction,"[^XYZ]+",""))
  
  #Fills blank variables with NA
  features$calculation[features$calculation == ""] <- NA
  #Set missing calculation values to the measurement method name extracted from original feature description
  features$calculation[is.na(features$calculation)] <- str_replace(features$feature[is.na(features$calculation)],"\\(.*\\)","")
  features$calculation <- gsub("meanfreq","mean",features$calculation)
  #Updates the class of 'classification' variable to be a 'factor'
  features$calculation <- as.factor(tolower(features$calculation))
  #Sets the final feature description as more descriptive as possible. Eg: Body Accelerometer - X - mean - time
  features$description <- paste(features$description,features$direction,features$calculation,features$measurement,sep=" - ")
  features$description <- gsub("-\\s*-","-",gsub("std","standard deviation",features$description))
  #Updates the class of 'description' variable to be a 'factor'
  features$description <- as.factor(str_trim(features$description))
  
  #Only keeps the mean and std measurements as described on the project requirements
  features <- subset(features,calculation == "mean" | calculation == "std")
  features
}
```
getTidyData():

``` R
getTidyData <- function(xFile,yFile,subjectFile, activities, features){
  #Loads the activities labels and associates them to their respective activities id
  labels <- read.table(yFile, sep=" ", col.names = c("id"))
  labels <- left_join(labels,activities, by="id")
  #Loads the subjects representing the volunteers 
  subjects <- read.table(subjectFile,col.names=c("volunteer"))
  #Creates a column for registering the number for each experiment (window) recorded per volunteer
  subjects <- subjects %>% group_by(volunteer) %>% mutate(record = seq_len(n()))
  #Loads the train data
  xData <- read.table(xFile)
  #Merges the data with other associated information and organizes it
  xData <- cbind("volunteernumber"=subjects$volunteer,"recordnumber"=subjects$record,"activity"=labels$description, xData)
  #Transforms the data to keep every row one observation
  #Each row will have a single measurement (feature) and its respective value
  xData <- xData %>% gather(featureid,value,V1:V561)
  #Updates the featureid so it can be linked to features data frame
  xData$featureid <- as.integer(gsub("[a-zA-Z]*","",xData$featureid))
  #Gets only data with the link with feature data frame
  xData <- subset(xData,featureid %in% features$featureid)
  #Merges the data with the feature data frame
  xData <- left_join(xData,features,by="featureid")
  #Organizes the name and the order of columns to make it tidy, and select only the relevant columns
  xData <- xData %>% select(volunteernumber,recordnumber,activity,"feature"=description,direction,calculation,"measurementunit"=measurement,value) %>% arrange(volunteernumber,recordnumber,activity,feature,direction,measurementunit,calculation)
}
```

### Running the analysis
---
To run this analysis and generate the tidy data on your own, you just need to follow these steps:

1. Clone the repository in your machine
2. Start RStudio or R
3. Set the folder where the repository was cloned in step 1 as current directory (see *?setwd* for help)
4. Load the run_analysis function in your enviroment. To do it just type 
	
	```R
	source("run_analysis.R")
	```
5. Call the run_analysis function typing
	
	```R
	run_analysis()
	```

### Reading the tidy data generated
---

Use the following function to read the tidy data generated into R:
``` R
data <- read.table("tidydata.txt",header=TRUE)
```

--------------------------------------------------------
--------------------------------------------------------

### Contact

For more information, please contact me:

<a href="mailto:brunoazev.c@gmail.com?Subject=Getting%and%Cleaning%Data%-%Course%Project" target="_top">brunoazev.c@gmail.com</a> 
