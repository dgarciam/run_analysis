## The assignment deals with 5 stages or tasks to be considered complete: 

## Task 1: Merges the training and the test sets to create one data set.

## This Section deals with the Train data
# First we read the train data 
trainData = read.table('train/X_train.txt', sep="")

# Loads the Train Data Labels
trainDataLabels = read.table('train/y_train.txt', sep="")
names(trainDataLabels)[1] = 'ActivityId' #Adds a name to the lone column of the set
trainData = cbind(trainData, trainDataLabels ) #Adds the Train Data Labels to the original Train DataSet

# Add the subjects
trainDataSubjects = read.table('train/subject_train.txt', sep="") #Loading the subjects
names(trainDataSubjects)[1] = 'Subject' # Adding Column Name
trainData = cbind(trainData, trainDataSubjects) # Merging with original

## This Section deals with the Test data
## Everything done previously is repeated, but to the test data.

# First load the test data
testData = read.table('test/X_test.txt', sep="")

# Add the labels
testDataLabels = read.table('test/y_test.txt', sep="")
names(testDataLabels)[1] = 'ActivityId'
testData = cbind(testData, testDataLabels)

# Add the subjects
testDataSubjects = read.table('test/subject_test.txt', sep="")
names(testDataSubjects)[1] = 'Subject'
testData = cbind(testData, testDataSubjects)

# Join both sets into a single one
allData = rbind(testData, trainData) ## This marks de END of Task 1

#Removing Stuff I don't need anymore
remove(testData, testDataLabels, testDataSubjects, trainData, trainDataLabels, trainDataSubjects)

# Task 3: Uses descriptive activity names to name the activities in the data set

## Setting the column names 
# Get the column names 
columnsFile = read.table('features.txt', sep="")

# Get a vector with only the column names
columnNames = as.character(columnsFile[,2])

#add the activityId + Subject column name, Since that was added by me at the beginning
columnNames = c(columnNames, "ActivityId")
columnNames = c(columnNames, "Subject")

names(allData) = columnNames # Apply de Columns Name to the whole set. Finishes Task 3

# Proceding to load the activity table
activityTable <- read.table('activity_labels.txt', sep="", col.names=c("ActivityId", "ActivityName"))

#Join the activity names to the data
allData <- merge(allData, activityTable, by="ActivityId")

#Task 2: Extracts only the measurements on the mean and standard deviation for each measurement.
#Selecting mean + std and tidying up data
featureIndex = grep("-mean\\(\\)|-std\\(\\)", columnsFile[,2])
featureIndex = featureIndex + 1
featureIndex = c(1, 563, 564, featureIndex) #Adding the first and last two column (Subject and ActivityName)

# Task 4: Appropriately labels the data set with descriptive variable names. 
cuteData = allData[,featureIndex]
names(cuteData) = gsub("\\(|\\)", "", names(cuteData))
names(cuteData) = tolower(names(cuteData))
cuteData$activityid = NULL
cuteData = cuteData[order(cuteData$subject,cuteData$activityname),]
row.names(cuteData) = NULL

write.table(cuteData,"tidy_up_data.txt")

# Task 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
#         for each activity and each subject.

uniqueSubjects = unique(cuteData$subject)
numSubjects = length(uniqueSubjects)
activities = read.table("activity_labels.txt")
numActivities = length(unique(cuteData$activityname))
numCols = dim(cuteData)[2]

results = cuteData[1:(numSubjects*numActivities), ] #Extract the first Subject

row = 1
for (numS in 1:numSubjects) {
  for (numA in 1:numActivities) {
    results[row, 1] = uniqueSubjects[numS]
    results[row, 2] = activities[numA, 2]
    tmp <- cuteData[cuteData$subject==numS & cuteData$activityname==activities[numA, 2], ]
    results[row, 4:numCols] <- colMeans(tmp[, 4:numCols])
    row = row+1
  }
}
results = results[order(results$subject,results$activityname),]
row.names(results) = NULL
results$activityname = tolower(results$activityname)
write.table(result, "merged_averages.txt")



