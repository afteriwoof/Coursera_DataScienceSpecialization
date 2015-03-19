initial.dir<-getwd()
setwd("~/Documents/Data_Science_Specialization_Coursera/Getting_and_cleaning_data/")

features<-read.table("UCI_HAR_Dataset/features.txt", header=FALSE, colClasses="character")

x_train<-read.table("UCI_HAR_Dataset/train/X_train.txt",col.names=features[,2])
y_train<-read.table("UCI_HAR_Dataset/train/Y_train.txt",col.names='labels')
subject_train<-read.table("UCI_HAR_Dataset/train/subject_train.txt",col.names='subject')

x_test<-read.table("UCI_HAR_Dataset/test/X_test.txt",col.names=features[,2])
y_test<-read.table("UCI_HAR_Dataset/test/Y_test.txt",col.names='labels')
subject_test<-read.table("UCI_HAR_Dataset/test/subject_test.txt",col.names='subject')

# 3)
activities <- read.table("./UCI_HAR_Dataset/activity_labels.txt",header=FALSE,colClasses="character")
y_train$labels<-factor(y_train$labels,levels=activities$V1,labels=activities$V2) 
y_test$labels<-factor(y_test$labels,levels=activities$V1,labels=activities$V2) 

# 4)
colnames(x_test)<-features$V2
colnames(x_train)<-features$V2
colnames(y_test)<-c("Activity")
colnames(y_train)<-c("Activity")
colnames(subject_test)<-c("Subject")
colnames(subject_train)<-c("Subject")

# 1)
train <- cbind(x_train, y_train, subject_train)
test <- cbind(x_test, y_test, subject_test)
merged_data <- rbind(train, test)
write.table(merged_data,file="merged_data.txt")

# 2)
ind <- features[grep("(mean|std)\\(", features[,2]),]
mean_std <- merged_data[,ind[,1]]

# 5)
library(plyr)
aves <- ddply(merged_data,.(Subject,Activity),function(x) colMeans(x[,1:60]))
write.table(aves,file="aves.csv",sep=",",row.names=FALSE)



