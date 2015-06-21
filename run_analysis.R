
###############################################################################
# Step 1 
# Download the dataset from the specified location, unzip and copy it to the 
# working directory that we want to work on 
###############################################################################

# Downloaded the folders from the specified location and copied it under 
# Documents/getdata


###############################################################################
#Step 2 
#Modify the working directory in rstudio either through GUI or using command. 
#Aslo need to create a folder to save results- called results folder
#Define the functions and create data frames using the functions created
###############################################################################


setwd("/Users/tomasantony/Documents/getdata")

datafolder <- "UCI HAR Dataset"  
resultsfolder <- "results"

if(!file.exists(resultsfolder)){
  
  print("create results folder")
  
  dir.create(resultsfolder)
  
} 


#Reading the  txt files and covnert those to data frame


gettables <- function (filename,cols = NULL){
  
  print(paste("Loading table:", filename))
              
              f <- paste(datafolder,filename,sep="/")
              
              data <- data.frame()
              
              if(is.null(cols)){
              
              data <- read.table(f,sep="",stringsAsFactors=F)
              
              } else {
              
              data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
              
              }
              
              data
              
}

              
        
             #Run and check loadtables
              
              
              features <- gettables("features.txt")
              
                        
              
              #Reading data and build database
              
              
              getdata <- function(type, features){
              
              print(paste("Loading data", type))
  
  subject_data <- gettables(paste(type,"/","subject_",type,".txt",sep=""),"id")
  
  y_data <- gettables(paste(type,"/","y_",type,".txt",sep=""),"activity")
  
  x_data <- gettables(paste(type,"/","X_",type,".txt",sep=""),features$V2)
  
  return (cbind(subject_data,y_data,x_data))
  
              }

# Run and check loaddata

test  <- getdata("test", features)

train <- getdata("train", features)


# Saving the resulting data in the resultsfolder


saveresults <- function (data,name){
  
  print(paste("saving results", name))
  
  file <- paste(resultsfolder, "/", name,".csv" ,sep="")
  
  write.csv(data,file)
  
}

###############################################################################
# Step 3
# Merge the training and test sets to create one data set
###############################################################################

library(plyr)

data <- rbind(train, test)

data <- arrange(data, id)

########################################################################################
# Step 4
# Extract only the measurements on the mean and standard deviation for each measurement
########################################################################################

mean_and_std <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]

saveresults(mean_and_std,"mean_and_std")


###############################################################################
# Step 5
# Use descriptive activity names to name the activities in the data set
###############################################################################


activity_labels <- gettables("activity_labels.txt")


###############################################################################  
# Step 6
# Appropriately label the data set with descriptive variable names
###############################################################################

data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

###############################################################################
# Step 7
# Create a second, independent tidy data set with the average of each variable
# for each activity and each subject
###############################################################################


tidy_dataset <- ddply(mean_and_std, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })

colnames(tidy_dataset)[-c(1:2)] <- paste(colnames(tidy_dataset)[-c(1:2)], "_mean", sep="")

saveresults(tidy_dataset,"tidy_dataset")

write.table(tidy_dataset, "averages_data.txt", row.name=FALSE)


