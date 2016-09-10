getTidyData <- function(xFile,yFile,subjectFile, activities, features){
  #Loads the labels and associated them to their respective activities 
  labels <- read.table(yFile, sep=" ", col.names = c("id"))
  labels <- left_join(labels,activities, by="id")
  #Loads the subjects representing the volunteers 
  subjects <- read.table(subjectFile,col.names=c("volunteer"))
  subjects <- subjects %>% group_by(volunteer) %>% mutate(record = seq_len(n()))
  #Loads the train data
  xData <- read.table(xFile)
  #Merges x_train data and organizes it
  xData <- cbind("volunteernumber"=subjects$volunteer,"recordnumber"=subjects$record,"activity"=labels$description, xData)
  xData <- xData %>% gather(featureid,value,V1:V561)
  xData$featureid <- as.integer(gsub("[a-zA-Z]*","",xData$featureid))
  xData <- subset(xData,featureid %in% features$featureid)
  xData <- left_join(xData,features,by="featureid")
  xData <- xData %>% select(volunteernumber,recordnumber,activity,"feature"=description,direction,calculation,"measurementunit"=measurement,value) %>% arrange(volunteernumber,recordnumber,activity,feature,direction,measurementunit,calculation)
}