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