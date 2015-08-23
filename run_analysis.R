rootFilePath <- "./GettingCleaningData/data"  ##This is the directory that points to where the data will be downloaded
## *Note: This variable may need to be edited.

## Download data
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", paste(rootFilePath, "/UCI HAR Dataset.zip", sep=""))
unzip(paste(rootFilePath, "/UCI HAR Dataset.zip", sep=""), exdir=rootFilePath)

library(data.table)  ##Load Data.Table package

## Create file paths for training and test sets
filePathTrain <- paste(rootFilePath, "/UCI HAR Dataset/train/X_train.txt", sep="")
filePathTest <- paste(rootFilePath, "/UCI HAR Dataset/test/X_test.txt", sep="")

## Read, modify and rewrite data to separate file to remove double spacing on lines
trainDataText <- readLines(filePathTrain)
trainDataText <- gsub("  ", " ", trainDataText)
filePathTrain <- gsub(".txt", "R.txt", filePathTrain)
writeLines(trainDataText, con=filePathTrain)
rm(trainDataText)
testDataText <- readLines(filePathTest)
testDataText <- gsub("  ", " ", testDataText)
filePathTest <- gsub(".txt", "R.txt", filePathTest)
writeLines(testDataText, con=filePathTest)
rm(testDataText)

## Read data in as data table
trainDataTable <- fread(filePathTrain)
testDataTable <-  fread(filePathTest)
## Each row corresponds to a particular subject found in subject_train.txt and subject_test.txt respectively
## Each row also corresponds to a particular activity found in y_train.txt and y_test.txt respectively

## Combine training and test data into single dataset
DT <- rbind(testDataTable, trainDataTable)

## Remove first column of dataset which contains unused logical vector
DT1 <- DT[,V1:=NULL]

## Load features vector into separate data table.  This contains descriptive column names
FilePathFeatures <- paste(rootFilePath, "/UCI HAR Dataset/features.txt", sep="")
featDT <- fread(FilePathFeatures)

##  Set names of DT1 to vector from features
featDT <- featDT[,V1:=NULL]
setnames(DT1,names(DT1),featDT$V2)
rm(featDT)

## Select a set of names that correspond to the mean and stdev of each measurement
names <- names(DT1)[grepl("mean()|std()", names(DT1))] ##Extract names that contain "mean()" and "std()" suffixes from names(DT1)
names <- names[!grepl("meanFreq()", names)] ##Remove mean frequency data
## *Note: Mean frequencies are removed because they are not averages of one of the 33 base measurements.  
##        Instead they are variables estimated using the base measurements.

## Create separate data table of reduced width containing only variables for mean and stdev of measurements
DT2 <- DT1[, names, with=FALSE]

## Set file paths for subject and activity data
filePathTestSubject <- paste(rootFilePath, "/UCI HAR Dataset/test/subject_test.txt", sep="")
filePathTrainSubject <- paste(rootFilePath, "/UCI HAR Dataset/train/subject_train.txt", sep="")
filePathTestActivity <- paste(rootFilePath, "/UCI HAR Dataset/test/y_test.txt", sep="")
filePathTrainActivity <- paste(rootFilePath, "/UCI HAR Dataset/train/y_train.txt", sep="")

## Read in subject and activity data as text
testSubjectText <- readLines(filePathTestSubject)
trainSubjectText <- readLines(filePathTrainSubject)
testActivityText <- readLines(filePathTestActivity)
trainActivityText <- readLines(filePathTrainActivity)

## Identify subject numbers as subjects
for (i in 1:30) {
  testSubjectText <- gsub(paste("^",i,"$",sep=""), paste("Subject", i, sep=" "),testSubjectText)
}
for (i in 1:30) {
  trainSubjectText <- gsub(paste("^",i,"$",sep=""), paste("Subject", i, sep=" "),trainSubjectText)
}
filePathTestSubject <- gsub(".txt", "R.txt", filePathTestSubject)
filePathTrainSubject <- gsub(".txt", "R.txt", filePathTrainSubject)
writeLines(testSubjectText, con=filePathTestSubject)
writeLines(trainSubjectText, con=filePathTrainSubject)
rm(testSubjectText, trainSubjectText)

## Give activities descriptive names
filePathActivities <- paste(rootFilePath, "/UCI HAR Dataset/activity_labels.txt", sep="")
activityLabels <- readLines(filePathActivities)
for (i in 1:6) {
  testActivityText <- gsub(paste(i), activityLabels[i], testActivityText)
}
for (i in 1:6) {
  trainActivityText <- gsub(paste(i), activityLabels[i], trainActivityText)
}
filePathTestActivity <- gsub(".txt", "R.txt", filePathTestActivity)
filePathTrainActivity <- gsub(".txt", "R.txt", filePathTrainActivity)
writeLines(testActivityText, con=filePathTestActivity)
writeLines(trainActivityText, con=filePathTrainActivity)
rm(testActivityText, trainActivityText)

## Create vectors for the modified subject and activity data
testSubjectVec <- fread(filePathTestSubject, sep="\n", header=FALSE)
trainSubjectVec <- fread(filePathTrainSubject, sep="\n", header=FALSE)
subjectVec <- rbind(testSubjectVec,trainSubjectVec)
rm(testSubjectVec, trainSubjectVec)
testActivityVec <- fread(filePathTestActivity, sep="\n", header=FALSE)
trainActivityVec <- fread(filePathTrainActivity, sep="\n", header=FALSE)
activityVec <- rbind(testActivityVec,trainActivityVec)
rm(testActivityVec, trainActivityVec)

## Create two new datasets one containing subject data and another containing activity data
DT3 <- copy(DT2)
DTS <- DT3[, subject_activity:=subjectVec]
DT3 <- copy(DT2)
DTA <- DT2[, subject_activity:=activityVec]

## Average variables by subject and by activity
DTS <- DTS[,lapply(.SD,mean),by=subject_activity]
DTA <- DTA[,lapply(.SD,mean),by=subject_activity]

## Combine averaged variables into single table
DTT <- rbind(DTS,DTA)
filePathTidy <- paste(rootFilePath, "/UCI HAR Dataset/tidyData.txt", sep="")
writeLines(DTT, con=filePathTidy, row.names=FALSE)
