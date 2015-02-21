library(reshape2)

tidyDataFile <- "tidyData.txt"
labels <- data.frame()
features <- data.frame()

GetData <- function(dir,type) {

	#Read data table
	data <-read.table(file.path(dir,paste0(type,"/X_",type,".txt")))
	names(data) <- features$V2
	
	#Extract mean and std data 
	data <- data[sort(c(grep(x = features$V2,"mean()",fixed=TRUE),grep(x = features$V2,"std()",fixed=TRUE)))]

	#Add activities to the data table
	print("   Adding activities...")
	activityData <- read.table(file.path(dir,paste0(type,"/y_",type,".txt")))
	activityData$V1 <- factor(activityData$V1, levels = labels$V1, labels = labels$V2)
	data <- cbind(Activity = activityData$V1, data)

	# Read the subjects list.
  	print("   Adding subjects...")
	subjectData <- read.table(file.path(dir,paste0(type,"/subject_",type,".txt")))
	data <- cbind(Subject = subjectData$V1, data)

	return(data)
}

RunAnalysis <- function(dir) {

	print("Getting and Cleaning Data Project")
	print("Author: Sergi Reñé")
	print("---")
	print("Starting up.")
	print("Preparing to run analysis.")

	labels <<- read.table(file.path(dir,"/activity_labels.txt"))
	features <<- read.table(file.path(dir,"/features.txt"))

	# Read the data.
	print("Getting train dataset...")
	trainData <- GetData(dir,"train")
	trainData$DataType <- "Train data"
	print("Getting test dataset...")
	testData <- GetData(dir,"test")
	testData$DataType <- "Test data"

	# Join the data.
  	print("Joining datasets.")
  	data <- rbind(testData, trainData)
	
	# Reshape the data.
	print("Melting.")
	meltedData <- melt(data, id = c("Subject", "Activity","DataType"))
	print("Dcasting.")
	tidyData <- dcast(meltedData, Subject + Activity ~ variable, mean)

	# Save the tidy data.
	print(paste("Saving clean data to:", tidyDataFile))
	write.table(tidyData, tidyDataFile, row.names = FALSE, quote = FALSE)

	return(tidyData)

}

