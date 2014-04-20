# This script, run_analysis.R,  will convert the training and test data from 
#
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
# Into a tidy data set which
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#
# The result of step 5 is saved into a file named "tidy.txt"
#
# The script in run by calling "extractAndSaveTidyData()"
#
# The script is documented by via documenting the individual functions of the script.


## The function, getMeanAndStdColumnIndexesAndNames(), will return all the features
## from the file features.txt which contains the text, mean(), and std().
## These features represents the mean and standard deviation features.
## The features will be returned as a data.frame where the first column
## represents the feature index and the second column represents the feature name.
getMeanAndStdColumnIndexesAndNames <- function(){
    filePath <- "features.txt"
    allFeatures <- read.table(filePath,header=FALSE)
    features <- allFeatures[grepl("mean\\(\\)", allFeatures[[2]])|grepl("std\\(\\)", allFeatures[[2]]),]
    colnames(features) <- c("FeatureIndex", "FeatureName")
    features 
}


## The function, getActivityLabelIndexes(), will return all the activity labels
## from the file activity_labels.txt.
## The activity labels will be returned as a data.frame where the first column
## represents the activity label index and the second column represents the activity label name.
getActivityLabelIndexes <- function(){
  filePath <- "activity_labels.txt"
  activityLabels <- read.table(filePath,header=FALSE)
  colnames(activityLabels) <- c("ActivityLabelIndex", "ActivityLabelName")
  activityLabels
}

## get user ids for observations
getSubjectIds <- function(subjectFile){
  subjects <- read.table(subjectFile,header=FALSE)
  subjects[[1]]
}

## The function getActivityLabels will convert the label ids from the file, activityIdFile,
## representing either  "./train/y_train.txt" or "./test/y_test.txt"
## into  a list of label names mapped from the file "./activity_labels.txt"
convertActivityIdsToActivityLabels <- function(activityIdFile){
  # get activities for each observation
  activityIds <- read.table(activityIdFile,header=FALSE)
  
  # get actvity labels (used for mapping activity ids to human readable names)
  activityLabels <- getActivityLabelIndexes()
  
  # map activity ids into label names and convert to factors
  as.factor(activityLabels$ActivityLabelName[activityIds[[1]]])
}

## The function getMeanAndStandardDeviationAccelerometerData will read
## the data from either "./train/X_train.txt" or "./test/X_test.txt"
## provided as accelerometerDataFile and read data
## into a data frame and return only those feature columns
## which represents the "mean" and "standard deviation" data. 
## The feature names will be added as column names.
getMeanAndStandardDeviationAccelerometerData <- function(accelerometerDataFile){
  # read accelerometer data
  accelerometerData <- read.table(accelerometerDataFile,header=FALSE)
  
  # get the feature column indexes to be used i.e. feature indexes for mean() and std() observations
  columnIndexes <- getMeanAndStdColumnIndexesAndNames()[[1]]
  
  # extract only those features columns to be used
  accelerometerData <- accelerometerData[,columnIndexes]
  
  # add human readable feature column names
  colnames(accelerometerData) <- as.character(getMeanAndStdColumnIndexesAndNames()[[2]])
  
  accelerometerData
}

## The function getTidyAccelerometerData will take either the data from
##
## "./train/X_train.txt", "./train/subject_train.txt", "./train/y_train.txt"
##
## or
##
## "./test/X_test.txt", "./test/subject_test.txt", "./test/y_test.txt"
##
## and convert them into a tidy data set.
##
## Activity data in respectively "./train/y_train.txt" and "./test/y_test.txt"
## will be represented with the activity labels in "activity_labels.txt"
getTidyAccelerometerData <- function(accelerometerDataFile, subjectFile, activityIdFile){
  
  # get accelerometer data
  accelerometerData <- getMeanAndStandardDeviationAccelerometerData(accelerometerDataFile)
  
  # get user ids for accelerometer data
  subjectIds <- getSubjectIds(subjectFile)
  
  # get activities for accelerometer data
  activities <- convertActivityIdsToActivityLabels(activityIdFile)
  
  # Add the UserIds and Activity labels to data frame with accelerometer data
  tidyData <- cbind(UserId=subjectIds, Activity=activities, accelerometerData)
  
  tidyData
}

# The function, getTrainTidyAccelerometerData, will get the tidy data set from the
# train data set
getTrainTidyAccelerometerData <- function(){
  getTidyAccelerometerData("./train/X_train.txt", "./train/subject_train.txt", "./train/y_train.txt")
}

# The function, getTestTidyAccelerometerData, will get the tidy data set from the
# test data set
getTestTidyAccelerometerData <- function(){
  getTidyAccelerometerData("./test/X_test.txt", "./test/subject_test.txt", "./test/y_test.txt")
}

# The function, mergeTrainAndTestTidyAccelerometerData, will merge the observations from
# the train, getTrainTidyAccelerometerData(),  and test,  getTestTidyAccelerometerData(),  data set?
mergeTrainAndTestTidyAccelerometerData <-  function() {
  trainData <- getTrainTidyAccelerometerData()
  testData <- getTestTidyAccelerometerData()
  rbind(trainData, testData)
}


# The function getAveragedDataGroupedByUserIdAndActivity(), will take the merged
# data sets returned by, mergeTrainAndTestTidyAccelerometerData(), and split into 
# subsets grouped by UserId and Activity and compute the
# mean for each data feature
getAveragedDataGroupedByUserIdAndActivity <- function(){
  mergedData <- mergeTrainAndTestTidyAccelerometerData()
  aggregate(mergedData[,3:30], list( UserId=mergedData$UserId, Activity=mergedData$Activity), mean)
}

# The function extractAndSaveTidyData will save the data returned by getAveragedDataGroupedByUserIdAndActivity
# into a file called "tidy.txt" located in the same folder as this script
extractAndSaveTidyData <- function(){
  tidy <- getAveragedDataGroupedByUserIdAndActivity()
  write.table(tidy, "tidy.txt", quote=FALSE, row.names=FALSE)
}
