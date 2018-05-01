rm(list = ls())
cat("\014")
setwd("D:/Study/Coursera/R programming")

library(dplyr)

#-----------------------------------------------------------------#
# 1. Merges the training and the test sets to create one data set #
#-----------------------------------------------------------------#
# Step1: download
if(!file.exists("./data")) dir.create("./data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/projectData_getCleanData.zip")

# Step2: unzip
listZip <- unzip("./data/projectData_getCleanData.zip", exdir = "./data")

# Step3: load&merge
train.x <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
train.y <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
train.subject <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
test.x <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
test.y <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
test.subject <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

trainData <- cbind(train.subject, train.y, train.x)
testData <- cbind(test.subject, test.y, test.x)
fullData <- rbind(trainData, testData)


#-----------------------------------------------------------------#
# 2. Extracts only the measurements on the mean and               #
#    standard deviation for each measurement                      #
#-----------------------------------------------------------------#
# Step1: load features
featureName <- read.table("./data/UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
featureName <- featureName[,2]

# Step2: extract mean and stdv
featureIndex <- grep(("mean\\(\\)|std\\(\\)"), featureName)
finalData <- fullData[, c(1, 2, featureIndex+2)]
colnames(finalData) <- c("subject", 
                         "activity", 
                         featureName[featureIndex])

#-----------------------------------------------------------------#
# 3. Uses descriptive activity names                              #
#    to name the activities in the data set                       #
#-----------------------------------------------------------------#
activityName <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
finalData$activity <- factor(finalData$activity, levels = activityName[,1], labels = activityName[,2])


#----------------------------------------------------------------------#
# 4. Appropriately labels the data set with descriptive variable names #
#----------------------------------------------------------------------#
names(finalData) <- gsub("\\()", "", names(finalData))
names(finalData) <- gsub("^t", "time ", names(finalData))
names(finalData) <- gsub("^f", "frequence ", names(finalData))
names(finalData) <- gsub("-mean", " mean", names(finalData))
names(finalData) <- gsub("-std", " std", names(finalData))
names(finalData) <- gsub("  ", " ", names(finalData))
names(finalData) <- toupper(x = names(finalData))


#------------------------------------------------------------------------------#
# 5. From the data set in step 4, creates a second,  independent tidy data set #
#    with the average of each variable for each activity and each subject      # 
#------------------------------------------------------------------------------#
groupData <- finalData %>%
    group_by(SUBJECT, ACTIVITY) %>%
    summarise_all(funs(mean))
write.table(groupData, "Tidy_dataset.txt", row.names = FALSE)
