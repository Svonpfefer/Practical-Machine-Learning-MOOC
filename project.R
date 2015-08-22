# Course project for Practical Machine Learning 

library(caret)
library(kernlab)
library(randomForest)
library(rio)

data_training <- import("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
data_testing <- import("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

# Clean data sets, remove columns with NaN or empty cells
clean_training <- data_training[,colSums(is.na(data_training))==0]
clean_training <- clean_training[,colSums(clean_training == '')==0]

clean_testing <- data_testing[,colSums(is.na(data_testing))==0]
clean_testing <- clean_testing[,colSums(clean_testing == '')==0]

# remove identifier columns such as name, timestamps etc
clean_training <- clean_training[,8:length(clean_training)]
clean_testing <- clean_testing[,8:length(clean_testing)]

# convert classe variable to factor variable
clean_training$classe <- as.factor(clean_training$classe)


# split the cleaned testing data into training and cross validation
inTrain <- createDataPartition(y = clean_training$classe, p = 0.7, list = FALSE)
training <- clean_training[inTrain, ]
crossval <- clean_training[-inTrain, ]

# Fit random forest model
modelForest <- randomForest(classe ~ ., data = training)

# Crossvalidate model using the remaining 30% of data
predictCrossVal <- predict(modelForest, crossval)
confusionMatrix(crossval$classe, predictCrossVal)

# Predict class of test set
predictTesting <- predict(modelForest,clean_testing)

# Function to print individual files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictTesting)

