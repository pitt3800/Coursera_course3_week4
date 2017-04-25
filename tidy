rm(list=ls())
library(dplyr)

# Open data
x_test <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/test/y_test.txt")
s_test <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/test/subject_test.txt")

x_train <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/train/y_train.txt")
s_train <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/train/subject_train.txt")

feature <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/features.txt")
label <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/activity_labels.txt")

# Open data
x_test <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/test/y_test.txt")
s_test <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/test/subject_test.txt")

x_train <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/train/y_train.txt")
s_train <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/train/subject_train.txt")

feature <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/features.txt")
label <- read.table("/Users/YJL/Google 드라이브/My R/UCI HAR Dataset/activity_labels.txt")



# 1. Merges the training and the test sets to create one data set.
x <- rbind(x_test, x_train)
y <- rbind(y_test, y_train)
s <- rbind(s_test, s_train)

# naming
names(x) <- feature[[2]]
names(y) <- "Activity"
names(s) <- "Subject"

# merge data
m <- cbind(s,y,x)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
extract <- grep("[Mm]ean|[Ss]td",names(m))
extract <- c(1, 2, extract)
e_m <- m[,extract]


# 3. Uses descriptive activity names to name the activities in the data set
e_m <- mutate(e_m, Activity=factor(Activity, labels=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")))


# 4. Appropriately labels the data set with descriptive variable names.
names(e_m) <- gsub("^t", "Time", names(e_m))
names(e_m) <- gsub("^f", "Frequency", names(e_m))
names(e_m) <- gsub("BodyBody", "Body", names(e_m))


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy <- aggregate(.~Subject + Activity, e_m, mean)
write.table(tidy,"~/Desktop/output.txt", row.names=F)
