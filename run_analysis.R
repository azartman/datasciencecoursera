##run_analysis.R does the following:

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, tidy data set with the average of each variable for each activity and each subject.
## --------------------------------------------------------------------------

## 1. Merge the training and the test sets to create one data set
  
##Read in the data
featuresNames <- read.table("~/UCI HAR Dataset/features.txt", header=FALSE)
activityType <- read.table("~/UCI HAR Dataset/activity_labels.txt", header=FALSE)

train.features <- read.table("~/UCI HAR Dataset/train/X_train.txt", header=FALSE)
test.features <- read.table("~/UCI HAR Dataset/test/X_test.txt", header=FALSE)

train.subject <- read.table("~/UCI HAR Dataset/train/subject_train.txt", header=FALSE)
test.subject <- read.table("~/UCI HAR Dataset/test/subject_test.txt", header=FALSE)

train.activity <- read.table("~/UCI HAR Dataset/train/y_train.txt", header=FALSE)
test.activity <- read.table("~/UCI HAR Dataset/test/y_test.txt", header=FALSE)

## Merge the data by rows
data.features <- rbind(train.features, test.features)
data.subject <- rbind(train.subject, test.subject)
data.activity <- rbind(train.activity, test.activity)

## Name the variables
colnames(data.features) <- featuresNames$V2
colnames(data.subject) <- c("subjectID")
colnames(data.activity) <- c("activity")
colnames(activityType) <- c("activity", "activityType")

##Create combined data set
Data <- cbind(data.features, data.subject, data.activity)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

## Subset names of features
colNames = colnames(Data)

logicalVector = (grepl("activity",colNames) | grepl("subject..",colNames) | 
                 grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & 
                 !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

## Create new subset
Data = Data[logicalVector==TRUE]

## 3. Uses descriptive activity names to name the activities in the data set

## Merge new subset with the activityType table to include descriptive activity names
finalData = merge(Data, activityType, by='activity', all.x=TRUE)

## 4. Appropriately labels the data set with descriptive variable names. 

names(finalData)<-gsub("^t", "time", names(Data))
names(finalData)<-gsub("^f", "frequency", names(Data))
names(finalData)<-gsub("Acc", "Accelerometer", names(Data))
names(finalData)<-gsub("Gyro", "Gyroscope", names(Data))
names(finalData)<-gsub("Mag", "Magnitude", names(Data))
names(finalData)<-gsub("BodyBody", "Body", names(Data))

## 5. Creates a second, tidy data set with the average of each variable for each activity and each subject.

library(plyr)
finalData2 <- aggregate(. ~subjectID + activity, finalData, mean)
finalData2 <- finalData2[order(finalData2$subject, finalData2$activity),]
write.table(finalData2, file = "tidydata.txt",row.name=FALSE)

## Create a codebook

library(knitr)
knit2html("codebook.Rmd")