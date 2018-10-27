
# preparing

if("dplyr" %in% rownames(install.packages()) == FALSE){install.packages("dplyr")};library(dplyr)
if("tidyr" %in% rownames(install.packages()) == FALSE){install.packages("tidyr")};library(tidyr)

furl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if(!file.exists("UCI HAR DATASET")){
    if(!file.exists("dataH")){
      dir.create("data")
    }
  download.file(furl,"HAR_DATASET.zip",method ="curl")
}
unzip("HAR_DATASET.zip")

#train test and merges

#training

trainx<- read.table("UCI HAR Dataset//train/X_train.txt",comment.char = "")
train_sub <- read.table("UCI HAR Dataset//train/subject_train.txt", col.names=c("subject"))
train_y <- read.table("UCI HAR Dataset/train//y_train.txt", col.names=c("activity"))
train_data <- cbind(trainx, train_sub, train_y)
nrow(trainx)

#test

test_x <- read.table("UCI HAR Dataset//test/X_test.txt", nrows=2947, comment.char="")
test_sub <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject"))
test_y <- read.table("UCI HAR Dataset/test//y_test.txt", col.names=c("activity"))
test_data <- cbind(test_x, test_sub, test_y)

# merge 

data1<-rbind(train_data,test_data)

#Extracts only the measurements on the mean and standard deviation for each measurement.

feature_list <- read.table("UCI HAR Dataset//features.txt", col.names = c("id", "name"))
View(feature_list)
features <- c(as.vector(feature_list[, "name"]), "subject", "activity")
View(features)
filtered_feature_ids <- grepl("mean|std|subject|activity", features) & !grepl("meanFreq", features)
filtered_data = data1[, filtered_feature_ids]
View(filtered_data)
#Uses descriptive activity names to name the activities in the data set

activities <- read.table("UCI HAR Dataset//activity_labels.txt", col.names=c("id", "name"))

for (i in 1:nrow(activities)) {
  
  filtered_data$activity[filtered_data$activity == activities[i, "id"]] <- as.character(activities[i, "name"])
  
}
# Appropriately labels the data set with descriptive variable names.

filtered_feature_names <- features[filtered_feature_ids]
filtered_feature_names <- gsub("\\(\\)", "", filtered_feature_names)
filtered_feature_names <- gsub("Acc", "-acceleration", filtered_feature_names)
filtered_feature_names <- gsub("Mag", "-Magnitude", filtered_feature_names)
View(filtered_feature_names)
filtered_feature_names <- gsub("^t(.*)$", "\\1-time", filtered_feature_names)
filtered_feature_names <- gsub("^f(.*)$", "\\1-frequency", filtered_feature_names)
filtered_feature_names <- gsub("(Jerk|Gyro)", "-\\1", filtered_feature_names)
filtered_feature_names <- gsub("BodyBody", "Body", filtered_feature_names)
filtered_feature_names <- tolower(filtered_feature_names)
names(filtered_data) <- filtered_feature_names

View(filtered_data)
tidy_data <- tbl_df(filtered_data) %>%
  group_by('subject', 'activity') %>%
  summarise_all(funs(mean)) %>%
  gather(measurement, mean, -activity, -subject)
View(tidy_data)

write.table(tidy_data, file="tidy_data.txt", row.name=FALSE)
