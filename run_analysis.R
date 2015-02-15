
## Prepare the data 
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp,method="curl")
lsfls <- unzip(temp)
unlink(temp)
## Remove some files that does not have important information for the task
fls <- lsfls[-3]
fls <- fls[-3]
length(fls)

## 1. Merges the training and the test sets to create one data set.
## Attach rbind(X_test,X_train) and also rbind(y_test,y_train) 
## and merge both of them, and give them the names of the features 

Xtest <- read.table(fls[13])
Xtrain <- read.table(fls[25])
ytest <- read.table(fls[14])
ytrain <- read.table(fls[26])

Xmerge <- rbind(Xtest,Xtrain)
ymerge <- rbind(ytest,ytrain)
tidy1 <- cbind(ymerge,Xmerge) # Database that has the y's and the features
feat <- read.table(fls[2])
names <- as.character(feat[,2])
colnames(tidy1) <- c("Labels", names)
colnames(tidy1)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
 
## Extract the variables that have Mean and Standar Deviation on iis name
newnames <- ""
for(i in 1:length(names)){
  if(grepl("mean",names[i])||grepl("std",names[i])){
    newnames <- c(newnames,names[i])
  }
}
newnames <- newnames[-1]

tidy2 <- data.frame(matrix(0,dim(tidy1)[1],length(newnames)+1))
tidy2[,1] <- tidy1[,1]
for(i in 1:length(newnames)){
  tidy2[,i+1] <- tidy1[,newnames[i]]
}

## 3. Uses descriptive activity names to name the activities in the data set

labels <- read.table(fls[1]) ## where the activity_labels.txt is.
ActivityNames <- rep("",dim(tidy2)[1])
for(i in 1:length(tidy2[,1])){
  ActivityNames <- as.character(labels[tidy2[,i],2])
}
## Add it as a column to the data set.
tidy2[,1] <- ActivityNames


## 4. Appropriately labels the data set with descriptive variable names. 
desnames <- newnames
# Change the shorts for Long Names and CamelCase
for(i in 1:length(desnames)){
  desnames[i] <- gsub("-std","SDev",desnames[i])
  if(noquote(strsplit(desnames[i], NULL)[[1]])[1]=='t'){
  desnames[i] <- gsub("t","Time",desnames[i],ignore.case=T)}
  desnames[i] <- gsub("GraviTimey","Gravity",newnames[i],ignore.case=T)
  if(noquote(strsplit(desnames[i], NULL)[[1]])[1]=='f'){
  desnames[i] <- gsub("f","Frequency",desnames[i],ignore.case=F)}
  desnames[i] <- gsub("Acc","Accelerometer",desnames[i])
  desnames[i] <- gsub("-mean","Mean",desnames[i])
  desnames[i] <- gsub("Gyro","Gyroscope",desnames[i])
  desnames[i] <- gsub("Mag","Magnitude",desnames[i])
  desnames[i] <- gsub("-meanFreq()","MeanFrequency",desnames[i])
  desnames[i] <- gsub("-X","Xaxis",desnames[i])
  desnames[i] <- gsub("-Y","Yaxis",desnames[i])
  desnames[i] <- gsub("-Z","Zaxis",desnames[i])
  desnames[i] <- gsub("[()]","",desnames[i])
}
desnames
colnames(tidy2) <- c(colnames(tidy1)[1],desnames)
colnames(tidy2)

## 5. From the data set in step 4, creates a second, 
## independent tidy data set with the average of each variable for each activity and each subject.

subjectTest <- read.table(fls[12])
subjectTrain <- read.table(fls[24])
subject <- rbind(subjectTest, subjectTrain)
tidy3 <- cbind(subject,tidy2)
colnames(tidy3) <- c("Subject",colnames(tidy2))
library(dplyr)

tidy4 <- within(tidy3,  id <- paste(Subject,Labels, sep="."))
by_sl <- group_by(tidy4,id)
krass <- summarise_each(by_sl,funs(mean),-(Subject:Labels))
View(krass)
krass <- within(krass, Subject <- data.frame(do.call('rbind', strsplit(as.character(krass$id),'.',fixed=TRUE)))[,1])
krass <- within(krass, Label <- data.frame(do.call('rbind', strsplit(as.character(krass$id),'.',fixed=TRUE)))[,2])
krass <- krass[,-1]
View(krass)

tidy5 <- data.frame(matrix(0,dim(krass)[1],dim(krass)[2]))
tidy5[,1] <- krass[,80]
tidy5[,2] <- krass[,81]
for(i in 1:(dim(krass)[2]-2)){
  tidy5[,i+2] <- krass[,i]
} 
# Give the proper names to each of the columns-variables 
colnames(tidy5) <- c("Subject", "Activity",colnames(krass)[1:(dim(krass)[2]-2)])
View(tidy5)
# Write the file with the tidy data set 
write.table(tidy5,file="TidyDataSet.txt",row.name=FALSE)
