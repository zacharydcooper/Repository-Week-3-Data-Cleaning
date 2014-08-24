#Merge data sets to create one working version
# Read training data first
subject_train = read.table("train/subject_train.txt", col.names=c("subject_id"))
# Row # = ID column value (chaching)
subject_train$ID <- as.numeric(rownames(subject_train))
# Read that training data
X_train = read.table("train/X_train.txt")
# Assign row numbers
X_train$ID <- as.numeric(rownames(X_train))
# Read activity data
y_train = read.table("train/y_train.txt", col.names=c("activity_id"))  # max = 6
# Assign row numbers for y_train
y_train$ID <- as.numeric(rownames(y_train))
# Merge files to train
train <- merge(subject_train, y_train, all=TRUE)
# Merge all
train <- merge(train, X_train, all=TRUE)

# Read subject training data            
subject_test = read.table("test/subject_test.txt", col.names=c("subject_id"))
# Assign row numbers as value
subject_test$ID <- as.numeric(rownames(subject_test))
# Read test data
X_test = read.table("test/X_test.txt")
# Assign row numbers as value
X_test$ID <- as.numeric(rownames(X_test))
# Read test data
y_test = read.table("test/y_test.txt", col.names=c("activity_id"))  # max = 6
# Assign row numbers
y_test$ID <- as.numeric(rownames(y_test))
# Merge to train
test <- merge(subject_test, y_test, all=TRUE) 
# Merge test files
test <- merge(test, X_test, all=TRUE) 

# Combine, train, test
Combination <- rbind(train, test)

# Extract only the measurements on the mean and standard deviation for each measurement
features = read.table("features.txt", col.names=c("feature_id", "feature_label"),)
# Extract only the measurements on the mean and standard deviation for each measurement
selected_features <- features[grepl("mean\\(\\)", features$feature_label) | grepl("std\\(\\)", features$feature_label), ]
Combination2 <- Combination[, c(c(1, 2, 3), selected_features$feature_id + 3) ]

#Provide more descriptive naming
activity_labels = read.table("activity_labels.txt", col.names=c("activity_id", "activity_label"),) #
Combination3 = merge(Combination2, activity_labels)

#Label data with descriptive names
selected_features$feature_label = gsub("\\(\\)", "", selected_features$feature_label)
selected_features$feature_label = gsub("-", ".", selected_features$feature_label)
for (i in 1:length(selected_features$feature_label)) {
        colnames(Combination3)[i + 3] <- selected_features$feature_label[i]
}
Combination4 = Combination3

#Tidy data set - can you say housekeeping?
Arrange <- c("ID","activity_label")
Combination5 <- Combination4[,!(names(Combination4) %in% drops)]
Agg <-aggregate(Combination5, by=list(subject = Combination5$subject_id, activity = Combination5$activity_id), FUN=mean, na.rm=TRUE)
Arrange <- c("subject","activity")
Agg <- aggregate[,!(names(Agg) %in% drops)]
Agg = merge(Aggregate, activity_labels)
write.table(file="submit.txt", x=Aggregate)

#Damn, that's some clean data