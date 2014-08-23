#Training Data
trainData <- read.table("./UCIHARDataset/train/X_train.txt")
dim(trainData)
names(trainData)

trainLabel <- read.table("./UCIHARDataset/train/y_train.txt")  # 6 activity labels: 
dim(trainLabel)
names(trainLabel)
table(trainLabel)

trainSubject <- read.table("./UCIHARDataset/train/subject_train.txt")
dim(trainSubject)
names(trainSubject)
head(trainSubject)
table(trainSubject)

#Testing Data
testData <- read.table("./UCIHARDataset/test/X_test.txt")
dim(testData)
names(testData)

testLabel <- read.table("./UCIHARDataset/test/y_test.txt")
dim(testLabel)
names(testLabel)
table(testLabel)


testSubject <- read.table("./UCIHARDataset/test/subject_test.txt")
dim(testSubject)
names(testSubject)
table(testSubject)


joinData <- rbind(trainData, testData)
dim(joinData)
names(joinData)

joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel)

joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject)

features <- read.table("./UCIHARDataset/features.txt")
names(features)
dim(features)

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2]) #Extracts only the measurements on the mean and standard deviation for each measurement.
length(meanStdIndices)
names(meanStdIndices)
head(meanStdIndices)
head(features[meanStdIndices,],15)

joinData <- joinData[, meanStdIndices]
names(joinData)

names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names


activity <- read.table("./UCIHARDataset/activity_labels.txt")
names(activity)
head(activity,10) # containing "walking", "walkingupstairs", "walkingdownstairs", "sitting", "standing", "laying"

activity[, 2] <- tolower(gsub("_", "", activity[, 2]))  
names(activity)
head(activity)  # containing "walking", "walkingupstairs", "walkingdownstairs", "sitting", "standing", "laying"

substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
head(activity)
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
head(activity) # containing "walking", "walkingupstairs", "walkingdownstairs", "sitting", "standing", "laying"

activityLabel <- activity[joinLabel[, 1], 2]  # replace the number:1,2,3,4,5,6 with text: standing, sitting.... this is a new object, which will be used later in the dataset
head(activityLabel,29)
names(activityLabel)
dim(activityLabel)

joinLabel[, 1] <- activityLabel
names(joinLabel)
head(joinLabel,28)
names(joinLabel) <- "activity"

names(joinSubject) <- "subject"

cleanedData <- cbind(joinSubject, joinLabel, joinData)
names(cleanedData)
head(cleanedData,4)
tail(cleanedData,4)
dim(cleanedData)  


subjectLen <- length(table(joinSubject))
activityLen <- dim(activity)[1]
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen)
result <- as.data.frame(result)
names(result)
dim(result)

colnames(result) <- colnames(cleanedData)

row <- 1
for(i in 1:subjectLen) {
    for(j in 1:activityLen) {
        result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
        result[row, 2] <- activity[j, 2]
        bool1 <- i == cleanedData$subject
        bool2 <- activity[j, 2] == cleanedData$activity
        result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
        row <- row + 1
    }
}

head(result,10)

write.table(result, "tidyData_with_average.txt")
