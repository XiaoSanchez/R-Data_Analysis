#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
#main. The goal of this exam is to get the data, 
#explore it and clean it.
#
#@author Cai Student
#@email caiy203@potsdam.edu
#@course CIS 235 Data Analysis and Visualization
#@Midterm 1
#@due 03/17/2021
#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
library(dplyr)

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- - 
#1. Download the data
#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
    if (!file.exists("UCI HAR Dataset.zip")) {
        fLink <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip"
        fZip <- "UCI HAR Dataset.zip"
        download.file(fLink, fZip)
        # Unzipping the file
        unzip(fZip)
    }

#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- - 
#2. Combine different files to get one data set
#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- - 
#Reading in the features from the subject
sTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
sTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)

# Reading in the features from the y
aTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
aTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)

# Reading in the features from the x
fTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
fTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

# Combine the Train and Test
sData <- rbind(sTrain, sTest)
aData <- rbind(aTrain, aTest)
fData <- rbind(fTrain, fTest)

# Replacing the column name to Subject and activity etc.
colnames(sData) <- "subject"
colnames(aData) <- "activity"
fDataNames <- read.table("UCI HAR Dataset/features.txt", head = FALSE)
colnames(fData) <- fDataNames$V2

# Merge to get the data frame Data for all data
mergeColData <- cbind(fData, aData, sData)

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- - 
#4. Only use mean measurements for you final data.
#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- - 
#Use only mean measurement in all features for the final data
mData <- grep(".*mean.*", names(mergeColData), ignore.case = TRUE)
requiredColumns <- c(mData, 562, 563)
extractedData <- mergeColData[, requiredColumns]

#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- - 
#5. For each activity plot the mean timeBodyAccelerometer - mean() - Y to show how these measurement are for all subjects
#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- - 
#Use tBodyAccMean - y for the final data
meanData <- grep(".*mean.*&.*Y.*", names(mergeColData), ignore.case = TRUE)
Colus <- c(meanData, 2, 563)
tBodyAcc <- mergeColData[, Colus]
# Reading the table from the activity labels
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
extractedData$activity <- head(activityLabels$V2[extractedData$activity], length(extractedData$activity))
eActivity <- extractedData$activity

# Get the dataset and split by the activity
Data <- aggregate(.~subject + eActivity, tBodyAcc, mean)
myData <- split(Data, Data$eActivity)

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- - 
#6. There are 6 activities and therefore you’ ll have 6 plots.Make sure you formatted correctly(labels, titles, etc.)
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- - 
#Create a 3 x 2 plotting matrix# The next 6 plots created will be plotted next to each other
par(mfrow = c(3, 2))

# Plot WALKING
plot(myData$WALKING %>% select(1, 3))
title("WALKING", line = 2)
# Plot WALKING_UPSTAIRS
plot(myData$WALKING_UPSTAIRS %>% select(1, 3))
title("WALKING_UPSTAIRS", line = 2)
# Plot WALKING_DOWNSTAIRS
plot(myData$WALKING_DOWNSTAIRS %>% select(1, 3))
title("WALKING_DOWNSTAIRS", line = 2)
# Plot SITTING
plot(myData$SITTING %>% select(1, 3))
title("SITTING", line = 2)
# Plot STANDING
plot(myData$STANDING %>% select(1, 3))
title("STANDING", line = 2)
# Plot LAYING
plot(myData$LAYING %>% select(1, 3))
title("LAYING", line = 2)