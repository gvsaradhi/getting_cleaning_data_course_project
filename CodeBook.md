Creating a Tidy Data set from HAR Data-Set
========================================================
### Variables
- The variables in the tidy-dataset are listed in the table below.
- **Subject** - refers to the 30 volunteers who participated in the study
- **activity_description** - refers to the 6 activities performed by each of the participants during the study
- Measurements on the mean and standard deviation for each measurement are extracted from original data.
- Rest of the variables are aggregated values of mean & standard deviations of these variables, by subject & activity description.
- Since we are computing the mean & standard deviations of these variables, their units of measurement remain the same as what is mentioned in the features_info.txt & readme.txt.
- The details of these features are mentioned in features_info.txt



|colnames.tidy_data.             |
|:-------------------------------|
|subject                         |
|activity_description            |
|tBodyAcc.mean...X               |
|tBodyAcc.mean...Y               |
|tBodyAcc.mean...Z               |
|tBodyAcc.std...X                |
|tBodyAcc.std...Y                |
|tBodyAcc.std...Z                |
|tGravityAcc.mean...X            |
|tGravityAcc.mean...Y            |
|tGravityAcc.mean...Z            |
|tGravityAcc.std...X             |
|tGravityAcc.std...Y             |
|tGravityAcc.std...Z             |
|tBodyAccJerk.mean...X           |
|tBodyAccJerk.mean...Y           |
|tBodyAccJerk.mean...Z           |
|tBodyAccJerk.std...X            |
|tBodyAccJerk.std...Y            |
|tBodyAccJerk.std...Z            |
|tBodyGyro.mean...X              |
|tBodyGyro.mean...Y              |
|tBodyGyro.mean...Z              |
|tBodyGyro.std...X               |
|tBodyGyro.std...Y               |
|tBodyGyro.std...Z               |
|tBodyGyroJerk.mean...X          |
|tBodyGyroJerk.mean...Y          |
|tBodyGyroJerk.mean...Z          |
|tBodyGyroJerk.std...X           |
|tBodyGyroJerk.std...Y           |
|tBodyGyroJerk.std...Z           |
|tBodyAccMag.mean..              |
|tBodyAccMag.std..               |
|tGravityAccMag.mean..           |
|tGravityAccMag.std..            |
|tBodyAccJerkMag.mean..          |
|tBodyAccJerkMag.std..           |
|tBodyGyroMag.mean..             |
|tBodyGyroMag.std..              |
|tBodyGyroJerkMag.mean..         |
|tBodyGyroJerkMag.std..          |
|fBodyAcc.mean...X               |
|fBodyAcc.mean...Y               |
|fBodyAcc.mean...Z               |
|fBodyAcc.std...X                |
|fBodyAcc.std...Y                |
|fBodyAcc.std...Z                |
|fBodyAcc.meanFreq...X           |
|fBodyAcc.meanFreq...Y           |
|fBodyAcc.meanFreq...Z           |
|fBodyAccJerk.mean...X           |
|fBodyAccJerk.mean...Y           |
|fBodyAccJerk.mean...Z           |
|fBodyAccJerk.std...X            |
|fBodyAccJerk.std...Y            |
|fBodyAccJerk.std...Z            |
|fBodyAccJerk.meanFreq...X       |
|fBodyAccJerk.meanFreq...Y       |
|fBodyAccJerk.meanFreq...Z       |
|fBodyGyro.mean...X              |
|fBodyGyro.mean...Y              |
|fBodyGyro.mean...Z              |
|fBodyGyro.std...X               |
|fBodyGyro.std...Y               |
|fBodyGyro.std...Z               |
|fBodyGyro.meanFreq...X          |
|fBodyGyro.meanFreq...Y          |
|fBodyGyro.meanFreq...Z          |
|fBodyAccMag.mean..              |
|fBodyAccMag.std..               |
|fBodyAccMag.meanFreq..          |
|fBodyBodyAccJerkMag.mean..      |
|fBodyBodyAccJerkMag.std..       |
|fBodyBodyAccJerkMag.meanFreq..  |
|fBodyBodyGyroMag.mean..         |
|fBodyBodyGyroMag.std..          |
|fBodyBodyGyroMag.meanFreq..     |
|fBodyBodyGyroJerkMag.mean..     |
|fBodyBodyGyroJerkMag.std..      |
|fBodyBodyGyroJerkMag.meanFreq.. |


### Summary Choices:
- Measurements on the mean and standard deviation for each measurement are extracted from original data, assuming that the column names having 'mean' & 'std' in their column names are the only ones that correspond to measurements on the mean and standard deviation.

### Experimental Study Design:
The following R Markdown explains each step performed for generating the tidy data-set in detail with the appropriate r-code chunks.


```r
# create necessary folder structure to place the data
data_directory= './data'
file_to_download= 'data/getdata-projectfiles-UCI HAR Dataset.zip'
stormDataURL= 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
file_destination= './data/getdata-projectfiles-UCI HAR Dataset.zip'
download_time_stamp_file= 'data/rawFileDownloadTimeStamp.csv'

#create a data directory if it doesnot exist, in the current folder
if(!file.exists(data_directory))
{
     dir.create(data_directory)
}

#download the file if it doesnot exist and write the download time-stamp, in the current folder
if(!file.exists(file_to_download))
{
     download.file(url=stormDataURL,destfile=file_destination,method='curl')
     dateDownloaded= date()
     write.csv(dateDownloaded,download_time_stamp_file)
}
```

- Resulting data is unzipped to a *data* folder to get *UCI HAR Dataset* 


```r
#unzip the data - overwrites downloaded data to avoid any confusion
library(utils)
unzip(file_destination,exdir=data_directory,overwrite = TRUE)
```
- *features.txt* is read and will be used a column-names. We assume that the order 
of columns in the training & test datasets are in the same order as in this file.


```r
#read all the meta-data files
feature_names= read.csv('data/UCI HAR Dataset/features.txt',sep=' ',header=FALSE,stringsAsFactors=FALSE)
#extract the column names
column_names= feature_names[,2]
activity_tables= read.table('data/UCI HAR Dataset/activity_labels.txt',header=FALSE, sep=' ',stringsAsFactors=FALSE)
```
- *activity_labels.txt* is read in a list with activity number as key and description as value.


```r
# 3. Use descriptive activity names to name the activities in the data set
# map activity numbers to descriptions
activity_map= list()
for(i in 1:nrow(activity_tables))
{
     activity_map[[activity_tables[i,1]]]=activity_tables[i,2]
}
```
- The training & test data are read in using *readLines* function. Leading spaces are trimmed, and each line is split by space to extract the expected 561 columns


```r
#utility function to read the features
load_data= function(filename,column_names)
{
     
     indata=readLines(filename)
     outdata= data.frame(matrix(nrow=length(indata),ncol=length(column_names)))
     # 4. Appropriately labels the data set with descriptive variable names. 
     colnames(outdata)=column_names
     for(i in 1:length(indata))
     {
          curLine= indata[[i]]
          trimmed_line=sub("^\\s+", "", curLine)
          cur_row=lapply(unlist(strsplit(trimmed_line,"\\s+")),as.numeric)
          outdata[i,]= cur_row
          
     }
     return(outdata)
}
#read test data
testX= load_data('data/UCI HAR Dataset/test/X_test.txt',column_names)
testY= read.csv('data/UCI HAR Dataset/test/y_test.txt',header=FALSE)
subject_test= read.csv('data/UCI HAR Dataset/test/subject_test.txt',header=FALSE)


#read training data
trainX= load_data('data/UCI HAR Dataset/train/X_train.txt',column_names)
trainY= read.csv('data/UCI HAR Dataset/train/y_train.txt', header=FALSE)
subject_train= read.csv('data/UCI HAR Dataset/train/subject_train.txt', header=FALSE)
```


- Subjects corresponding to each of the observations in the training & test data sets are read from subject_train.txt & subject_test.txt respectively.
- The y_train.txt & y_test.txt files are also read and mapped to their descriptions using the activity_map created earlier.


```r
# 3. Use descriptive activity names to name the activities in the data set
# map activity numbers to descriptions
activity_map= list()
for(i in 1:nrow(activity_tables))
{
     activity_map[[activity_tables[i,1]]]=activity_tables[i,2]
}



# add description & subject as additional columns to test features
testY$desc= rep('',nrow(testY))
for(i in 1:nrow(subject_test))
{
     testY[i,2]= activity_map[testY[i,1]]
}
testX$activity_description= testY$desc
testX$subject= subject_test[,1]


# add description & subject as additional columns to training features
trainY$desc= rep('',nrow(trainY))
for(i in 1:nrow(subject_train))
{
     trainY[i,2]= activity_map[trainY[i,1]]
}
trainX$activity_description= trainY$desc
trainX$subject= subject_train[,1]
```

- We then merge the training & test datasets
- Only the columns have 'mean', 'std' strings are extracted from the merged datasets. The subject & activity_description columns are retained.
- The merged dataset is then aggregated by subject & activity description & written to an output file.


```r
# 1. Merges the training and the test sets to create one data set.
merged_data= rbind(trainX,testX)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_std_data= merged_data[, grepl("mean|std|subject|activity_description", names(merged_data))]
mean_std_data$subject= as.factor(mean_std_data$subject)
mean_std_data$activity_description= as.factor(mean_std_data$activity_description)

# 5. creates a second, independent tidy data set with the average of each variable for each activity and each subject.
avg_by_subject_data= aggregate(.~subject+activity_description,mean_std_data,mean)
```


