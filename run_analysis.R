#1. Merges the training and the test sets to create one data set.

## Import and download web data
fileURL <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, "activity.zip")
unzip("activity.zip")

##Load data 
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header=FALSE)
features <- read.table("UCI HAR Dataset/features.txt", header = FALSE)
View(features)
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE)

##Label
colnames(activityLabels)=c('activityId','activityType');
colnames(subject_train)="subjectId";
colnames(x_train )=features[,2]; 
colnames(y_train)="activityId";

##merge train
train<-cbind( subject_train, y_train, x_train)
View(train)

y_test <- read.table("UCI HAR Dataset/test/y_test.txt", header=FALSE)
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE)
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE)

##Label test
colnames(subject_test)="subjectId";
colnames(X_test)=features[,2]; 
colnames(y_test)="activityId";

###merge test
test <- cbind(subject_test, y_test, X_test)

##merge all together
TrainTest <-rbind(train, test)



# 2. Extracts only the measurements on the mean and standard deviation
##for each measurement.

##Logical vector which will have TRUE for ID, mean and stddev columns and FALSE for everything else
logicalVector = (grepl("activity..",colnames) | grepl("subject..",colnames) | grepl("-mean..",colnames) 
                 & !grepl("-meanFreq..",colnames) & !grepl("mean..-",colnames) | grepl("-std..",colnames) 
                 & !grepl("-std()..-",colnames))
##subset where TRUE

SelectData <-TrainTest[logicalVector==TRUE]


# 3. Uses descriptive activity names to name the activities in the data set

SelectData$activityId <-factor(SelectData$activityId, levels=activityLabels[,1], labels=activityLabels[,2])
View(SelectData)

# 4. Appropriately labels the data set with descriptive variable names.

names(SelectData)[names(SelectData)=="activityId"] <- "activity"
colnames(SelectData)<- gsub("\\()", "", colnames(SelectData))
colnames(SelectData)<- gsub("\\-", "", colnames(SelectData))

#5. From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

library(data.table)

SelectData.melted <-melt(SelectData, id = c("subjectId", "activity"))
SelectData.mean <-dcast(SelectData.melted, subjectId + activity ~ variable, mean)

write.table(SelectData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)