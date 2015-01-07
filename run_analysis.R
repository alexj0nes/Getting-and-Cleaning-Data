# Step 1
# Merges the training and the test sets to create one data set.

# load libraries
library(data.table)
library(plyr)
library(dplyr)
library(gdata)

# import features list (column names for training & test datasets)
features<-read.table('UCI HAR Dataset/features.txt')

# import training dataset
training<-read.table('UCI HAR Dataset/train/X_train.txt')

# apply column name labels to training dataset
colnames(training)<-features$V2

# import training labels datasaet
training_labels<-read.table('UCI HAR Dataset/train/y_train.txt')
colnames(training_labels)<-"ActivityCode"

# import activity labels datasaet
activity_labels<-read.table('UCI HAR Dataset/activity_labels.txt')

training_labels2<-merge(training_labels,activity_labels,by.x='ActivityCode',by.y='V1')

colnames(training_labels2)<-c("ActivityCode","ActivityName")

# import subject training dataset
subject_training<-read.table('UCI HAR Dataset/train/subject_train.txt')
colnames(subject_training)<-"SubjectCode"

# append subject training dataset to training dataset
training2<-cbind(subject_training,training_labels2,training)

# import test dataset
test<-read.table('UCI HAR Dataset/test/X_test.txt')

# apply column name labels to test dataset
colnames(test)<-features$V2

# import subject test dataset
subject_test<-read.table('UCI HAR Dataset/test/subject_test.txt')
colnames(subject_test)<-"SubjectCode"

# import test labels datasaet
test_labels<-read.table('UCI HAR Dataset/test/y_test.txt')
colnames(test_labels)<-"ActivityCode"

test_labels2<-merge(test_labels,activity_labels,by.x='ActivityCode',by.y='V1')

colnames(test_labels2)<-c("ActivityCode","ActivityName")

# append subject training and activity labels datasets to training dataset
test2<-cbind(subject_test,test_labels2,test)

# append test dataset to training dataset
training_test<-rbind(training2,test2)

# Step 2
# Extracts only the measurements on the mean and standard deviation for each measurement.

library(gdata)

# get list of column names that contain either mean or std
col_names<-matchcols(training_test,with=c('mean()','std()'),method='or')

# create data frames from column name lists
col_names_df_1<-data.frame(col_names$mean)
col_names_df_2<-data.frame(col_names$std)
col_names_df_3<-data.frame(c("SubjectCode","ActivityName"))

# filter meanFreq() from col_names_df_1
col_names_df_1_2<-filter(col_names_df_1,!grepl('Freq',col_names_df_1$col_names.mean))

colnames(col_names_df_1_2)<-'V1'
colnames(col_names_df_2)<-'V1'
colnames(col_names_df_3)<-'V1'

# append column name lists
col_names_df<-rbind(col_names_df_3,col_names_df_1_2,col_names_df_2)

# create new training_test data frame with only columns with mean or std in their name
training_test2<-training_test[as.character(col_names_df$V1)]

# Step 3
# Uses descriptive activity names to name the activities in the data set

#   Already taken care of in Step 1

# Step 4
# Appropriately labels the data set with descriptive variable names. 

#   Already taken care of in Step 1

# Step 5
# From the data set in step 4, creates a second, independent tidy data set with the average of each 
#   variable for each activity and each subject.

training_test_dt<-data.table(training_test2)

training_test_dt<-arrange(training_test_dt,ActivityName,SubjectCode)

tidy<-training_test_dt[,lapply(.SD,mean),by=c('ActivityName','SubjectCode')]

write.table(tidy,'tidy_data_set_aij.txt',row.name=FALSE) 
