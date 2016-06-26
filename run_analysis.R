### Run Analysis.R ###
#Goals Per Website:
# 1- Merges the training and the test sets to create one data set.
# 2- Extracts only the measurements on the mean and standard deviation for each measurement.
# 3- Uses descriptive activity names to name the activities in the data set
# 4- Appropriately labels the data set with descriptive variable names.
# 5- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Goal 1- Merging training test sets to creat one data set
#Assume the R file is in in the main folder with all the locaitions as previous
features <- read.table("features.txt")
actLabels <- read.table("activity_labels.txt")
subTrain <- read.table("train/subject_train.txt")
xTrain <- read.table("train/X_train.txt")
yTrain <- read.table("train/y_train.txt")
subTest <- read.table("test/subject_test.txt")
xTest <- read.table("test/X_test.txt")
yTest <- read.table("test/y_test.txt")
#Now that we have read everything RowBind the train/test together as new variable; always in the correct order
subjects <- rbind(subTrain, subTest)
xVals <- rbind(xTrain,xTest)
yVals <- rbind(yTrain,yTest)
rm(subTrain,subTest,xTrain,xTest,yTrain,yTest)

##Goal 2 & 4- Extract Only Mean/Std features and then label them descriptively (ie with their name)
##To do this we will take the features with mean or std in the name and only grab those and then label them accordingly
titles<- features$V2
keepIndices <- grep("*mean|std*", titles) #Gives us the indices of features with mean or std in the name
xVals <- xVals[,keepIndices]
titles<- gsub("-","_",titles)
titles<- gsub("\\(","",titles)
titles<- gsub("\\)","",titles)
colnames(xVals) <- titles[keepIndices] ##Rename the colNames to the proper feature name

##Goal 3 - Use descriptive activty names
##The y-values contain the activity by a number (1-6); the associated labels are stored in activity_labels.txt
##We want to take yVals convert it to a factor then make those factors reflect activity labels
yVals <- as.factor(yVals$V1)
levels(yVals) <- levels(actLabels$V2)
data <- cbind(xVals, yVals)
colnames(data)[80] <- "Activity"
rm(yVals,xVals)

##Goal 3.5 Lets add the volunteer becauswe we will want it later; make it a factor like the previous one
subjects <- as.factor(subjects$V1)
data <- cbind(data,subjects)
colnames(data)[81] <- "Subject"

##We now have a dataframe called data that has all the entries with the additional info of the activty, and volunteer

subject_activity <- data.frame(stringsAsFactors = FALSE)
for(act in levels(data[,80])) {
  for(vol in as.character(levels(data[,81]))) {
    varMeans <- filter(data, Activity==act, Subject==vol) %>% select(-Activity, -Subject) %>% summarise_each(funs(mean))
    entry <- cbind(data.frame(vol),data.frame(act))
    entry <- cbind(entry, varMeans)
    subject_activity <- rbind(subject_activity, entry)
  }
}
colnames(subject_activity)[1] <- "Subject"
colnames(subject_activity)[2] <- "Activity"

print(subject_activity)
