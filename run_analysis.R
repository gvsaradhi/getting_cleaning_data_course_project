
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

#unzip the data - overwrites downloaded data to avoid any confusion
library(utils)
unzip(file_destination,exdir=data_directory,overwrite = TRUE)


#read all the meta-data files
feature_names= read.csv('data/UCI HAR Dataset/features.txt',sep=' ',header=FALSE,stringsAsFactors=FALSE)
#extract the column names
column_names= feature_names[,2]
activity_tables= read.table('data/UCI HAR Dataset/activity_labels.txt',header=FALSE, sep=' ',stringsAsFactors=FALSE)


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

# 1. Merges the training and the test sets to create one data set.
merged_data= rbind(trainX,testX)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_std_data= merged_data[, grepl("mean|std|subject|activity_description", names(merged_data))]
mean_std_data$subject= as.factor(mean_std_data$subject)
mean_std_data$activity_description= as.factor(mean_std_data$activity_description)

# 5. creates a second, independent tidy data set with the average of each variable for each activity and each subject.
avg_by_subject_data= aggregate(.~subject+activity_description,mean_std_data,mean)

# write the final tidy data to a text file
write.table(avg_by_subject_data,file='tidy_activity_data.txt',row.name=FALSE)




