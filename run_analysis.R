# Reading triaxial data from training dataset
data_train <- read.table("C:\\Users\\Ankita Das\\Documents\\Self Study\\R\\UCI HAR Dataset\\train\\X_train.txt",
                         sep="",header = FALSE)

# Reading Activity data from training dataset
activity_train <- read.table("C:\\Users\\Ankita Das\\Documents\\Self Study\\R\\UCI HAR Dataset\\train\\Y_train.txt",
                             header = FALSE)

# Reading subject data from training dataset
subject_train <- read.table("C:\\Users\\Ankita Das\\Documents\\Self Study\\R\\UCI HAR Dataset\\train\\subject_train.txt",
                           sep="",header = FALSE)

# Reading triaxial data from test dataset
data_test <- read.table("C:\\Users\\Ankita Das\\Documents\\Self Study\\R\\UCI HAR Dataset\\test\\X_test.txt",
                        sep="",header = FALSE)

# Reading Activity data from test dataset
activity_test <- read.table("C:\\Users\\Ankita Das\\Documents\\Self Study\\R\\UCI HAR Dataset\\test\\Y_test.txt",
                            header = FALSE)

#Reading subject data from test dataset
subject_test <- read.table("C:\\Users\\Ankita Das\\Documents\\Self Study\\R\\UCI HAR Dataset\\test\\subject_test.txt",
                           sep="",header = FALSE)

# Appending triaxial data from train and test
data <- rbind(data_train,data_test)

# Reading column names
data_label <- read.table("C:\\Users\\Ankita Das\\Documents\\Self Study\\R\\UCI HAR Dataset\\features.txt",header = FALSE)

# Replacing columns with hyphen
# For this stringr package needs to be installed
 install.packages("stringr")
 library("stringr")
Mean <- paste("mean_",data_label[,2])
data_label <- cbind(data_label, Mean)
data_label$V2 <- str_replace_all(data_label$V2, ",", " ")
data_label$V2 <- str_replace_all(data_label$V2, "-", " ")
data_label$V2 <- str_replace_all(data_label$V2, "\\(", " ")
data_label$V2 <- str_replace_all(data_label$V2, "\\)", " ")
data_label$V2 <- str_replace_all(data_label$V2, " ", "")

# Assigning column names
colnames(data) <- data_label[,2]

# Appending activity data from train and test
activity <- rbind(activity_train,activity_test)

# Assigning column names
colnames(activity) <- "activity"

# Appending subject data from train and test
subject <- rbind(subject_train,subject_test)

# Assigning column names
colnames(subject) <- "subject"

# Combining triaxial and activity data. This is the full dataset
full_dataset <- cbind(subject,activity,data)

# Extracting measurements on the mean and standard deviation for each measurement, along with the activity.
# For this dplyr package needs to be installed
 install.packages("dplyr")
 library("dplyr")
 install.packages("data.table")
 library("data.table")

nodup_dataset <- full_dataset[ , !duplicated(colnames(full_dataset))]
extract_Data <- select(nodup_dataset,starts_with("activity"),starts_with("subject"),matches("(mean|std).*"))
extract_headers <- data_label %>% dplyr::filter(data_label$V2 %like% "(mean|std|Mean).*")
extract_headers$Mean <- str_replace_all(extract_headers$Mean, " ", "")

# Extracting the activity description
activity_labels <- read.table("C:\\Users\\Ankita Das\\Documents\\Self Study\\R\\UCI HAR Dataset\\activity_labels.txt",
                              header = FALSE)

# Naming columns
colnames(activity_labels) = c("activity", "activity_description")

# giving meaningful names to activities in the full dataset
long_dataset <- merge(x = extract_Data, y = activity_labels, by = "activity", all.x = TRUE)

# Creating independent tidy data set with the average of each variable for each activity and each subject

tidy_dataset <- aggregate(x=long_dataset[,3],
                          by=list(long_dataset$activity_description,long_dataset$subject),
                          FUN=mean)
colnames(tidy_dataset) <- c("activity_description", "subject", extract_headers[1,3])

i <- 4
for(i in 4:88){
  temp1 <- aggregate(x=long_dataset[,i],
                 by=list(long_dataset$activity_description,long_dataset$subject),
                 FUN=mean)
  colnames(temp1) <- c("activity_description", "subject", extract_headers[i-2,3])
  temp2 <- merge(x = tidy_dataset, y = temp1, by=c("activity_description","subject"), all.x = TRUE)
  tidy_dataset <- temp2
  i+1
}

# Writing extracted data to a text file
write.table(tidy_dataset, file = "C:\\Users\\Ankita Das\\Documents\\Self Study\\R\\Proj3\\Course3ProjAssignment", sep = " ",
             eol = "\n", na = "NA", dec = ".", row.names = FALSE,
             col.names = TRUE, qmethod = c("escape", "double"),
             fileEncoding = "")
