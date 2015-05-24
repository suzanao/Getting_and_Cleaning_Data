# Course project for Coursera G&C Data



# 1. Merge the training and the test sets to create one data set.
#    Read training and test data sets from .txt files with read.table
#-------------------------------------------------------------------------------

# set the working directory where the data are downloaded
setwd("D:/Coursera/GETTING AND CLEANING DATA/UCI HAR Dataset")

# list of all features (in 1st column is index, in 2nd column names)
features <- read.table("features.txt") 
names <- c("Subject","Activity",as.character(features[,2]))


### read training files

trainSet <- read.table("train/X_train.txt")     
trainLabels <- read.table("train/y_train.txt")  
trainSubject <- read.table("train/subject_train.txt")

# Create one data set for training data by binding columns
train <- cbind(trainSubject, trainLabels, trainSet)

# give names to the columns of training dataset
colnames(train) <- names


### read test files

testSet <- read.table("test/X_test.txt")    
testLabels <- read.table("test/y_test.txt") 
testSubject <- read.table("test/subject_test.txt")

# Create one data set for test data by binding columns
test <- cbind(testSubject, testLabels, testSet)

# give names to the columns of test data set
colnames(test) <- names


### Merge the training and the test sets to create one data set
dataSet <- rbind(test,train)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# -------------------------------------------------------------------------------------------

# features that have mean() or std() (standard deviation) in their names, e.g.
# regex (mean|std)\\(\\) looks for mean() or std() in colnames of dataSet 
meanStd <- grep("(mean|std)\\(\\)", colnames(dataSet), value = TRUE)


# Subset of merged data that has only these features
dataSubset <- dataSet[,c("Subject","Activity",meanStd)] 
# 68 columns = Subject + Activity + 66 extracted features


# 3. Uses descriptive activity names to name the activities in the data set
#-------------------------------------------------------------------------------

# descriptive activity names are in activity labels
# numbers 1-6 with given activity names
activityLabels <- read.table("activity_labels.txt")     
colnames(activityLabels) <- c("activity","activityLabel")


# transform activity column from numeric to factor
transfrom <- activityLabels$activity
names(transfrom) <- activityLabels$activityLabel


dataSubset$Activity <- factor(dataSubset$Activity, levels=transfrom, labels = names(transfrom))
# now in column Activity in dataSubset instead of numbers 1-6 are written names of activities


# 4. Appropriately labels the data set with descriptive variable names.
#-------------------------------------------------------------------------------

# View(dataSubset) --> before substitution

colnames(dataSubset) <- gsub("\\(\\)", "", tolower(colnames(dataSubset)))
colnames(dataSubset) <- gsub("-", ".", colnames(dataSubset))

# View(dataSubset) --> after substitution 


# 5. From the data set in step 4, create a second, independent tidy data set with 
#    the average of each variable for each activity and each subject.
#---------------------------------------------------------------------------------
    
# install reshape2 package!    
library(reshape2)

# save column names in colId (activity and subject) and colVars
colId <- colnames(dataSubset)[1:2]
n=length(colnames(dataSubset))
colVars <- colnames(dataSubset)[3:n]

# Create tidy data by taking the mean/average of each variable by subject and activity
dataMelt <- melt(dataSubset, id=colId, measure.vars=colVars)
dataDcast <- dcast(dataMelt, subject + activity ~ variable, mean)

# Add avg prefix to the variable name where average is calculated
colnames(dataDcast) <- c(colId, gsub("(.*)", "avg.\\1", colVars))



### Save tidy data 

if(!file.exists("./ispis")){ dir.create("./ispis") }
write.table(dataDcast, file = "ispis/tidyDataAvg.txt", row.names = FALSE, quote = FALSE)

