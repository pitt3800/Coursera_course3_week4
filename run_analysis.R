
rm(list=ls())
library(dplyr)
library(data.table)


# 1 Open data

featuresTest <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/test/x_test.txt")
activityTest <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/test/y_test.txt")
subjectTest <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/test/subject_test.txt")

featuresTrain <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/train/x_train.txt")
activityTrain <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/train/y_train.txt")
subjectTrain <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/train/subject_train.txt")

featureNames <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/features.txt")
activityLabels <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/activity_labels.txt")

# 2 Merge the training and the test sets to create one data set

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)



colnames(features) <- t(featureNames[2])

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

# 3 Extracts only the measurements on the mean and standard deviation for each measurement


columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)


requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)



extractedData <- completeData[,requiredColumns]
dim(extractedData)

# 4 Uses descriptive activity names to name the activities in the data set

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}



extractedData$Activity <- as.factor(extractedData$Activity

# 5 Appropriately labels the data set with descriptive variable names

names(extractedData)


names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))


names(extractedData)

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)


tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
