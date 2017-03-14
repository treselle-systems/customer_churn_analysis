library(stats)
library(caTools)
library(Amelia)
library(dplyr)

# set your working directory

# read the telecom dataset input file
telecomDataframe <- read.csv(file="WA_Fn-UseC_-Telco-Customer-Churn.csv")

# print the structure of the dataframe
print(str(telecomDataframe))

# check for the NA values 
any(is.na(telecomDataframe))

# visualize the missing values using the missing map from the Amelia package
missmap(telecomDataframe,col=c("yellow","red"))

# create new column "tenure_interval" from the tenure column
group_tenure <- function(tenure){
    if (tenure >= 0 && tenure <= 6){
        return('0-6 Month')
    }else if(tenure > 6 && tenure <= 12){
        return('6-12 Month')
    }else if (tenure > 12 && tenure <= 24){
        return('12-24 Month')
    }else if (tenure > 24 && tenure <=36){
        return('24-36 Month')
    }else if (tenure > 36 && tenure <=48){
        return('36-48 Month')
    }else if (tenure > 48 && tenure <= 62){
        return('48-62 Month')
    }else if (tenure > 62){
        return('> 62 Month')
    }
}

# apply group_tenure function on each row of dataframe
telecomDataframe$tenure_interval <- sapply(telecomDataframe$tenure,group_tenure)
telecomDataframe$tenure_interval <- as.factor(telecomDataframe$tenure_interval)

# Ignore the variables with more levels while predicting the model
# Columns "customerID" and "tenure" having more levels
telecomDataframe <- select(telecomDataframe,-customerID,-tenure)

# The value of the following columns affecting the model and introducing the NA value for "No phone service" and  and "No internet service" need to cleanup the data for these columns MultipleLine,OnlineSecurity,OnlineBackup,DeviceProtection,TechSupport,StreamingTV,StreamingMovies
telecomDataframe$MultipleLines <- as.character(telecomDataframe$MultipleLines)
telecomDataframe$OnlineSecurity <- as.character(telecomDataframe$OnlineSecurity)
telecomDataframe$OnlineBackup <- as.character(telecomDataframe$OnlineBackup)
telecomDataframe$DeviceProtection <- as.character(telecomDataframe$DeviceProtection)
telecomDataframe$TechSupport <- as.character(telecomDataframe$TechSupport)
telecomDataframe$StreamingTV <- as.character(telecomDataframe$StreamingTV)
telecomDataframe$StreamingMovies <- as.character(telecomDataframe$StreamingMovies)

# convert factor variables into character variables before changing the values
telecomDataframe$MultipleLines[telecomDataframe$MultipleLines=="No phone service"] <- "No"
telecomDataframe$OnlineSecurity[telecomDataframe$OnlineSecurity=="No internet service"] <- "No"
telecomDataframe$OnlineBackup[telecomDataframe$OnlineBackup=="No internet service"] <- "No"
telecomDataframe$DeviceProtection[telecomDataframe$DeviceProtection=="No internet service"] <- "No"
telecomDataframe$TechSupport[telecomDataframe$TechSupport=="No internet service"] <- "No"
telecomDataframe$StreamingTV[telecomDataframe$StreamingTV=="No internet service"] <- "No"
telecomDataframe$StreamingMovies[telecomDataframe$StreamingMovies=="No internet service"] <- "No"

# converting character variables into factor variables
telecomDataframe$MultipleLines <- as.factor(telecomDataframe$MultipleLines)
telecomDataframe$OnlineSecurity <- as.factor(telecomDataframe$OnlineSecurity)
telecomDataframe$OnlineBackup <- as.factor(telecomDataframe$OnlineBackup)
telecomDataframe$DeviceProtection <- as.factor(telecomDataframe$DeviceProtection)
telecomDataframe$TechSupport <- as.factor(telecomDataframe$TechSupport)
telecomDataframe$StreamingTV <- as.factor(telecomDataframe$StreamingTV)
telecomDataframe$StreamingMovies <- as.factor(telecomDataframe$StreamingMovies)

# check the number of NA rows if it is relatively small in number then ignore those rows from the analysis
telecomDataframe <- na.omit(telecomDataframe)

# set the seed it will output same output when ever the model is executed
set.seed(123)

# sample the input data with 70% for training and 30% for testing
sample <- sample.split(telecomDataframe$Churn,SplitRatio=0.70)
trainData <- subset(telecomDataframe,sample==TRUE)
testData <- subset(telecomDataframe,sample==FALSE)

# logistic regression model on top training the data
telecomModel <- glm(Churn ~ .,family=binomial(link="logit"),data=trainData)
print(summary(telecomModel))

# test the model with test dataset
test.predictions <- predict(telecomModel,newdata=testData,type="response")

# if the prediction probability is greater than 0.5 then those 
# customers are classified as churned customer less than 0.5 are classified as not churning customer
fitted.results <- ifelse(test.predictions > 0.5,1,0)
testData$Churn <- as.character(testData$Churn)
testData$Churn[testData$Churn=="No"] <- "0"
testData$Churn[testData$Churn=="Yes"] <- "1"

# calculating the misclassfication rate
misClasificationError <- mean(fitted.results!=testData$Churn)
print(misClasificationError)

# calculating the accuracy rate
accuracyRate <- 1-misClasificationError
print(accuracyRate)

# confusion matrix
table(testData$Churn,test.predictions > 0.5)

# cbinding actual results with the predicted results
results <- cbind(fitted.results,testData$Churn)
colnames(results) <- c("predicted","actual")
results <- as.data.frame(results)
print(results)