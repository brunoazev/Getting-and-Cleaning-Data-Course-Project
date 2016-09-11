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
  #Updates the class of 'classification' variable to be a 'factor'
  features$calculation <- as.factor(tolower(features$calculation))
  #Sets the final feature description as more descriptive as possible. Eg: Body Accelerometer - X - mean - time
  features$description <- paste(features$description,features$direction,features$calculation,features$measurement,sep=" - ")
  features$description <- gsub("-\\s*-","-",gsub("std","standard deviation",features$description))
  #Updates the class of 'description' variable to be a 'factor'
  features$description <- as.factor(str_trim(features$description))
  
  #Only keeps the mean and std measurements as described on the project requirements
  features <- subset(features,calculation == "meanfreq" | calculation == "mean" | calculation == "std")
  features$calculation <- gsub("std","standard deviation",features$calculation)
  features
}