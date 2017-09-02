library(plyr)

# Downloading dataset
if(!file.exists("./data_cleaning")){dir.create("./data_cleaning")}
filepath <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(filepath,destfile="./data_cleaning/Dataset.zip")

# Unzip dataSet to /data directory
unzip(zipfile="./data_cleaning/Dataset.zip",exdir="./data_cleaning")


# 1. Merging the training and the test sets to create one data set:

# 1.1 Reading files

# 1.1.1 Reading trainings tables:
train_x <- read.table("./data_cleaning/UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./data_cleaning/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data_cleaning/UCI HAR Dataset/train/subject_train.txt")

# 1.1.2 Reading testing tables:
test_x <- read.table("./data_cleaning/UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("./data_cleaning/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data_cleaning/UCI HAR Dataset/test/subject_test.txt")

# 1.1.3 Reading feature vector:
features <- read.table('./data_cleaning/UCI HAR Dataset/features.txt')

# 1.1.4 Reading activity labels:
activityLabels = read.table('./data_cleaning/UCI HAR Dataset/activity_labels.txt')

# 1.2 Assigning column names:
colnames(train_x) <- features[,2] 
colnames(train_y) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(test_x) <- features[,2] 
colnames(test_y) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

# 1.3 Merging all data in one set:
merge_train <- cbind(train_y, subject_train, train_x)
merge_test <- cbind(test_y, subject_test, test_x)
setAllInOne <- rbind(merge_train, merge_test)

# 2. Extracting only the measurements on the mean and standard deviation for each measurement

# 2.1 Reading column names:
colNames <- colnames(setAllInOne)

# 2.2 Create vector for defining ID, mean and standard deviation:
mean_and_std <- (grepl("activityId" , colNames) | 
                                         grepl("subjectId" , colNames) | 
                                         grepl("mean.." , colNames) | 
                                         grepl("std.." , colNames) 
                 )

# 2.3 Making nessesary subset from setAllInOne:
setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]

# 3. Using descriptive activity names to name the activities in the data set:
setActivityNames <- merge(setForMeanAndStd, activityLabels,
                                           by='activityId',
                                                all.x=TRUE)

# 4. Appropriately labeling the data set with descriptive variable names.
# This step was made in previos steps => See 1.3, 2.2, 2.3.

# 5. Creating a second, independent tidy data set with the average of each variable for each activity and each subject:

# 5.1 Making second tidy data set 
TidyData <- aggregate(.~subjectId + activityId, setActivityNames, mean)
TidyData <- TidyData[order(TidyData$subjectId, TidyData$activityId),]

# 5.2 Writing second tidy data set in txt file
write.table(TidyData, "TidyData.txt", row.name=FALSE)