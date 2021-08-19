#run_analysis.R
#Merge the training and the test sets to create one data set.
## load needed libraries
library(readr)
library(fs)
library(dplyr)

## define paths of all required data and labels
datafolder<-path(getwd(),"UCI HAR Dataset")
 activity_labels_file<-path(datafolder,"activity_labels.txt")
 features_file<-path(datafolder,"features.txt")
 testfolder<-path(datafolder,"test")
  test_data_file<-path(testfolder,"X_test.txt")
  test_activity_file<-path(testfolder,"y_test.txt")
  test_subject_file<-path(testfolder,"subject_test.txt")
 trainfolder<-path(datafolder,"train")
  train_data_file<-path(trainfolder,"X_train.txt")
  train_activity_file<-path(trainfolder,"y_train.txt")
  train_subject_file<-path(trainfolder,"subject_train.txt")
 
## set labels as list variable
features<-as.list(read.delim2(features_file,header=FALSE,sep=" ",strip.white = TRUE,)[,2])
####Generate list of features to extract
mean_features<-grep("mean",features,value=FALSE)
SD_features<-grep("std",features,value=FALSE)
select_feature_ids<-sort(c(mean_features,SD_features))

## set descriptive activity key as data table
activity_labels<-read.delim(activity_labels_file,header = FALSE,sep=" ",strip.white = TRUE)


## load both csv files and set as variables
test_data<-read.fwf(test_data_file,header=FALSE,widths=c(rep(16,561)),col.names=features)
test_subjects<-read.delim2(test_subject_file,header = FALSE,sep = " ",strip.white = TRUE,col.names="Subject")
test_activities<-read.delim2(test_activity_file,header = FALSE,sep = " ",strip.white = TRUE,col.names="A")

train_data<-read.fwf(train_data_file,header=FALSE,widths=c(rep(16,561)),col.names=features)
train_subjects<-read.delim2(train_subject_file,header = FALSE,sep = " ",strip.white = TRUE,col.names="Subject")
train_activities<-read.delim2(train_activity_file,header = FALSE,sep = " ",strip.white = TRUE,col.names="A")

###Extract only means and standard deviations
test_data<-select(test_data,select_feature_ids)
train_data<-select(train_data,select_feature_ids)


## create a tidy data set from "test" data
### already: Each observation is a row
### already: Each feature is a column
### already: Columns have descriptive labels - feature list used as column headers when loading data file into data frame
### qualitative variables are descriptive
#### numeric values in y_test.txt are matched with descriptive labels in activity_labels.txt
desc_test_activities<-left_join(test_activities,activity_labels,by=c("A"="V1"))
desc_test_activities<-rename(desc_test_activities,"Activity"="V2")
desc_test_activities<-select(desc_test_activities,Activity)
#### subject numbers are replaced with "SUBJECT XX" where X is the subject number
test_subjects<-mutate(test_subjects,Subject=paste("SUBJECT",formatC(Subject,width=2,flag=0)))
### each observation is in only one table - add subject and activity columns to test data table
test_data<-cbind(test_subjects,desc_test_activities,test_data)
## repeat these steps for the "train" data
### already: Each observation is a row
### already: Each feature is a column
### already: Columns have descriptive labels - feature list used as column headers when loading data file into data frame
### qualitative variables are descriptive
#### numeric values in y_train.txt are matched with descriptive labels in activity_labels.txt
desc_train_activities<-left_join(train_activities,activity_labels,by=c("A"="V1"))
desc_train_activities<-rename(desc_train_activities,"Activity"="V2")
desc_train_activities<-select(desc_train_activities,"Activity")
#### subject numbers are replaced with "SUBJECT XX" where X is the subject number
train_subjects<-mutate(train_subjects,Subject=paste("SUBJECT",formatC(Subject,width=2,flag=0)))
### each observation is in only one table - add subject and activity columns to test data table
train_data<-cbind(train_subjects,desc_train_activities,train_data)
## create a single data set from "test" and "train" data sets
###Add column to each set to note test/train
#test_data<-mutate(test_data,Phase="Test")
#train_data<-mutate(train_data,Phase="Train")
###Combine train and test data
data<-rbind(test_data,train_data)
#data<-relocate(data,Phase,.before=Subject)

#Create a second, independent tidy data set with the average of each variable for each activity and each subject.
##Create a grouped data set
grouped_data<-group_by(data,Activity,Subject)
##Summarize the grouped data set by averaging the variables by activity and subject
summary<-summarize_all(grouped_data,mean)
##Display grouped data set, write to file
print(summary)
write.csv(summary,file="Summary.csv")