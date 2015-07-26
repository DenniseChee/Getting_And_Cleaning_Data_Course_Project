# Load the library, choose dplyr to process this set of data
library(dplyr)

# Prepare the column and activities name
subjectsColumnName <- c("subjects")
activitiesColumnName <- c("activities")

featuresNames <- read.table('UCI HAR Dataset/features.txt', stringsAsFactors = F)[,2]
meanOrStdFeatures <- c(
  subjectsColumnName,
  activitiesColumnName,
  featuresNames[grepl("mean()", featuresNames, fixed=TRUE)],
  featuresNames[grepl("std()", featuresNames, fixed=TRUE)]
)

activitiesNames <- read.table('UCI HAR Dataset/activity_labels.txt', stringsAsFactors = F)

# Read the test dataset
subjectIds <- read.table('UCI HAR Dataset/test/subject_test.txt', col.names=subjectsColumnName)

activitiesIds <- read.table('UCI HAR Dataset/test/y_test.txt', col.names=activitiesColumnName)
features <- read.table('UCI HAR Dataset/test/X_test.txt', col.names=featuresNames, check.names = FALSE)

# Bind frames together
testData <- cbind(subjectIds, activitiesIds, features)

# Read the train dataset
subjectIds <- read.table('UCI HAR Dataset/train/subject_train.txt', col.names=subjectsColumnName)
activitiesIds <- read.table('UCI HAR Dataset/train/y_train.txt', col.names=activitiesColumnName)
features <- read.table('UCI HAR Dataset/train/X_train.txt', col.names=featuresNames, check.names = FALSE)

# Bind frames together
trainData <- cbind(subjectIds, activitiesIds, features)

# Bind train and test datasets together
dataSet <- rbind(trainData, testData)

# Extract only mean and standard deviation related observations
meanAndSDDataSet <- dataSet[, meanOrStdFeatures]

# Get tidy data
tidyData <- meanAndSDDataSet %>% group_by(subjects, activities) %>% summarise_each(funs(mean))

# Labels the activities in data-set with human-readable values
getActivityName <- function(activityCode) {
  activitiesNames[activityCode, ][, 2]
}
tidyData$activities <- sapply(tidyData$activities, getActivityName)

# Write the resulting dataset into file
write.table(tidyData, file='UCI HAR Dataset/tidy_data.txt', row.names=FALSE)