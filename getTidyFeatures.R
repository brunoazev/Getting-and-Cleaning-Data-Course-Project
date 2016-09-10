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
  features$measurement <- as.factor(features$measurement)
  #Adds a new factor level for missing variables. This will be treated as "degree" unit
  #Missing values are intended to represent the angle measurement unit
  #First adds a new level named "degree" to make it able to update the missing values with this factor
  levels(features$measurement) <- c(levels(features$measurement),"degree")
  features$measurement[is.na(features$measurement)] <- "degree"
  #Creates new columns for the estimated variables and directions
  #Directions will not be used, but it is separated anyway
  newColumns <- str_split_fixed(as.character(features$feature),"-",3)
  colnames(newColumns) <- c("description","calculation","direction")
  #Adds new columns to 'features' dataframe
  features <- cbind(features,newColumns)
  features$description <- as.character(features$description)
  #Gets the main description of the feature
  features$description[grep("^[f|t][A-Z]",features$description)] <- sapply(str_split(features$description[grep("^[f|t][A-Z]",features$description)],"^t|f"),function(x){ retorno <- unlist(x); retorno[2] })
  #features$description[grep("^[a-zA-Z]*\(.*\)$",features$description)] <- sub("\\(.*\\)$","",features$description)
  #Removes repeated sequence of words. eg: BodyBody
  features$description <- str_replace(gsub('([[:upper:]])', ' \\1', features$description),"\\b(\\w+)\\b(?=.*\\b\\1\\b)\\s+","")
  features$description <- gsub("Gyro","Gyroscope",features$description)
  features$description <- gsub("Mag","Magnitude",features$description)
  features$description <- gsub("Acc","Accelerometer",features$description)
  
  #Eliminates unnecessary text from 'calculation' variable
  features$calculation <- str_replace(features$calculation,"\\(.*\\)","")
  #Eliminates unnecessary text from 'direction' variable
  features$direction <- as.factor(str_replace(features$direction,"[^XYZ]+",""))
  
  #Fills blank variables with NA
  features$direction[features$direction == ""] <- NA
  features$calculation[features$calculation == ""] <- NA
  #Set missing calculation values to the method name extracted from original feature description
  features$calculation[is.na(features$calculation)] <- str_replace(features$feature[is.na(features$calculation)],"\\(.*\\)","")
  features$calculation <- as.factor(features$calculation)
  
  features$description <- paste(paste(features$description,features$direction,features$calculation,sep=" - "),paste("(",features$measurement,")"))
  #Transforms the variable "description" to a factor
  features$description <- as.factor(str_trim(features$description))
  
  #Only keeps the mean and std measurements
  features <- subset(features,calculation == "mean" | calculation == "std")
  features$calculation <- gsub("std","standard deviation",features$calculation)
  features
}