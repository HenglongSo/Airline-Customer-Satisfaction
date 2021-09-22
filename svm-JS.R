#############################################################
# SVM modeling

##########
# Importing the required packages for SVM modeling
#install.packages("RJSONIO") 
library(RJSONIO) 
#install.packages("grid")
library(grid)
#install.packages("Matrix")
library(Matrix)
#install.packages("kernlab")
library(kernlab)
#install.packages("ggplot2")
library(ggplot2)

# Reading the data
ProjectData <-read.csv( "spring19survey.csv")
#View(ProjectData)
summary(ProjectData)
str(ProjectData)
dim(ProjectData)
length(ProjectData)
table(factor(ProjectData$Satisfaction)) #undestand the Satisfaction data

# Cleaning the data
colSums(is.na(ProjectData))
clean <- na.omit(ProjectData)
# Treat NA values
CleanData <- ProjectData
CleanData$Satisfaction [is.na(ProjectData$Satisfaction)] <- round(mean(clean$Satisfaction))
CleanData$Departure.Delay.in.Minutes[is.na(ProjectData$Departure.Delay.in.Minutes)] <-as.integer(round(mean(clean$Departure.Delay.in.Minutes)))
CleanData$Arrival.Delay.in.Minutes[is.na(ProjectData$Arrival.Delay.in.Minutes)] <-round(mean(clean$Arrival.Delay.in.Minutes))
CleanData$Flight.time.in.minutes[is.na(ProjectData$Flight.time.in.minutes)] <-round(mean(clean$Flight.time.in.minutes))
CleanData <- na.omit(CleanData)
colSums(is.na(CleanData))
table(CleanData$Satisfaction)

# Formating the data
ind1 <- which(CleanData$Satisfaction == '4.00.2.00')
CleanData$Satisfaction[ind1[1]] <- 4
CleanData$Satisfaction[ind1[2]] <- 4
ind2 <- which(CleanData$Satisfaction == '4.00.5')
CleanData$Satisfaction[ind2[1]] <- 5
ind3 <- which(CleanData$Satisfaction == '2.5')
CleanData$Satisfaction[ind3] <- 3
ind4 <- which(CleanData$Satisfaction == '3.5')
CleanData$Satisfaction[ind4] <- 4
ind5 <- which(CleanData$Satisfaction == '4.5')
CleanData$Satisfaction[ind5] <- 5
CleanData$Satisfaction <- factor(CleanData$Satisfaction)
table(factor(CleanData$Satisfaction))
CleanData$Satisfaction <- as.numeric(CleanData$Satisfaction)
CleanData$Year.of.First.Flight <-factor(CleanData$Year.of.First.Flight)
CleanData$Price.Sensitivity <- factor(CleanData$Price.Sensitivity)
CleanData$Flight.Distance <- factor(CleanData$Flight.Distance)
CleanData$Total.Freq.Flyer.Accts <-factor(CleanData$Total.Freq.Flyer.Accts)
CleanData$Flight.cancelled <- factor(CleanData$Flight.cancelled)

table(CleanData$Satisfaction)
#############

# Copying the data of three companies to three dataframes
dataCheapseats <- subset(CleanData, Partner.Name == 'Cheapseats Airlines Inc.')
dataSigma <- subset(CleanData, Partner.Name == 'Sigma Airlines Inc.')
dataFlyFast <- subset(CleanData, Partner.Name == 'FlyFast Airways Inc.')

summary(dataCheapseats)
str(dataCheapseats)
dim(dataCheapseats)

summary(dataSigma)
str(dataSigma)
dim(dataSigma)

summary(dataFlyFast)
str(dataFlyFast)
dim(dataFlyFast)

# creating a function to divide  the satisfaction into three levels
createbucketsurvey <- function(vec){
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 3] <- "High"
  vBuckets[vec < 3] <- "Low"
  return(vBuckets)
}

# Copying the studied variables
CustSat1 <- createbucketsurvey(dataCheapseats$Satisfaction)
Arrival.Delay.in.Minutes1 <- dataCheapseats$Arrival.Delay.in.Minutes
Airline.Status1 <- dataCheapseats$Airline.Status
Price.Sensitivity1 <- dataCheapseats$Price.Sensitivity
Type.of.Travel1 <- dataCheapseats$Type.of.Travel
Loyalty1 <- dataCheapseats$Loyalty
Departure.Delay.in.Minutes1 <- dataCheapseats$Departure.Delay.in.Minutes
Long.Duration.Trip1 <- dataCheapseats$Long.Duration.Trip

CustSat2 <- createbucketsurvey(dataSigma$Satisfaction)
Arrival.Delay.in.Minutes2 <- dataSigma$Arrival.Delay.in.Minutes
Airline.Status2 <- dataSigma$Airline.Status
Price.Sensitivity2 <- dataSigma$Price.Sensitivity
Type.of.Travel2 <- dataSigma$Type.of.Travel
Loyalty2 <- dataSigma$Loyalty
Departure.Delay.in.Minutes2 <- dataSigma$Departure.Delay.in.Minutes
Long.Duration.Trip2 <- dataSigma$Long.Duration.Trip

CustSat3 <- createbucketsurvey(dataFlyFast$Satisfaction)
Arrival.Delay.in.Minutes3 <- dataFlyFast$Arrival.Delay.in.Minutes
Airline.Status3 <- dataFlyFast$Airline.Status
Price.Sensitivity3 <- dataFlyFast$Price.Sensitivity
Type.of.Travel3 <- dataFlyFast$Type.of.Travel
Loyalty3 <- dataFlyFast$Loyalty
Departure.Delay.in.Minutes3 <- dataFlyFast$Departure.Delay.in.Minutes
Long.Duration.Trip3 <- dataFlyFast$Long.Duration.Trip


# Combining the studied variables into a new dataframe
# In this svm model, I use 7 independent variables to train the model to estimate the dependent variable.
ruleDF1 <- data.frame(CustSat1,
                      Arrival.Delay.in.Minutes1,
                      Airline.Status1,
                      Price.Sensitivity1,
                      Type.of.Travel1,
                      Loyalty1,
                      Departure.Delay.in.Minutes1,
                      Long.Duration.Trip1
                      )

ruleDF2 <- data.frame(CustSat2,
                      Arrival.Delay.in.Minutes2,
                      Airline.Status2,
                      Price.Sensitivity2,
                      Type.of.Travel2,
                      Loyalty2,
                      Departure.Delay.in.Minutes2,
                      Long.Duration.Trip2
                      )# Creates a dataframe

ruleDF3 <- data.frame(CustSat3,
                      Arrival.Delay.in.Minutes3,
                      Airline.Status3,
                      Price.Sensitivity3,
                      Type.of.Travel3,
                      Loyalty3,
                      Departure.Delay.in.Minutes3,
                      Long.Duration.Trip3
                      )# Creates a dataframe

# Seperating the dataset to trainset and testset randomly
randIndex1 <- sample(1:dim(ruleDF1)[1])
cutPoint1 <- floor(0.7*dim(ruleDF1)[1])
trainData1 <- ruleDF1[randIndex1[1:cutPoint1],]
testData1 <- ruleDF1[randIndex1[(cutPoint1+1):dim(ruleDF1)[1]],]
dim(trainData1)
dim(testData1)

randIndex2 <- sample(1:dim(ruleDF2)[1])
cutPoint2 <- floor(0.7*dim(ruleDF2)[1])
trainData2 <- ruleDF2[randIndex2[1:cutPoint2],]
testData2 <- ruleDF2[randIndex2[(cutPoint2+1):dim(ruleDF2)[1]],]
dim(trainData2)
dim(testData2)

randIndex3 <- sample(1:dim(ruleDF3)[1])
cutPoint3 <- floor(0.7*dim(ruleDF3)[1])
trainData3 <- ruleDF3[randIndex3[1:cutPoint3],]
testData3 <- ruleDF3[randIndex3[(cutPoint3+1):dim(ruleDF3)[1]],]
dim(trainData3)
dim(testData3)

# Run the SVM model
svmOutput1 <- ksvm(CustSat1 ~ ., 
                   data=trainData1, 
                   kernel= "rbfdot", 
                   kpar = "automatic", 
                   C = 5, 
                   cross = 3, 
                   prob.model = TRUE)

svmOutput2 <- ksvm(CustSat2 ~ ., 
                   data=trainData2, 
                   kernel= "rbfdot", 
                   kpar = "automatic", 
                   C = 5, 
                   cross = 3, 
                   prob.model = TRUE)

svmOutput3 <- ksvm(CustSat3 ~ ., 
                   data=trainData3, 
                   kernel= "rbfdot", 
                   kpar = "automatic", 
                   C = 5, 
                   cross = 3, 
                   prob.model = TRUE)

# Showing the trained SVM model
svmOutput1
# Using the trained SVM model to predict the testset
svmPred1 <- predict(svmOutput1, testData1)
# Comparing the testset and predicted result
compTable1 <- data.frame(testData1[,1],svmPred1)
compTable1 <- table(compTable1)
compTable1
# Calculating the prediction correction percentage
percentCorrect1 <- (compTable1[1,3]+compTable1[2,2]+compTable1[3,1])/sum(compTable1)
percentCorrect1
# Drawing the heatmap for prediction results
compTable1 <- data.frame(compTable1)
svm1.heatmap <- ggplot(data = compTable1, mapping = aes(x = svmPred1,
                                                        y = testData1...1.,
                                                        fill = Freq)) +
  geom_tile() +
  xlab(label = "SVM Predicted") + 
  ylab(label = "Test Data") +
  scale_fill_gradient(name = "Freq",
                      low = "#FFFFFF",
                      high = "#5E0000") + 
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(label = "Cheapseats Airlines Inc.")
svm1.heatmap

# Showing the trained SVM model
svmOutput2
# Using the trained SVM model to predict the testset
svmPred2 <- predict(svmOutput2, testData2)
# Comparing the testset and predicted result
compTable2 <- data.frame(testData2[,1],svmPred2)
compTable2 <- table(compTable2)
compTable2
# Calculating the prediction correction percentage
percentCorrect2 <- (compTable2[1,3]+compTable2[2,2]+compTable2[3,1])/sum(compTable2)
percentCorrect2
# Drawing the heatmap for prediction results
compTable2 <- data.frame(compTable2)
svm2.heatmap <- ggplot(data = compTable2, mapping = aes(x = svmPred2,
                                                        y = testData2...1.,
                                                        fill = Freq)) +
  geom_tile() +
  xlab(label = "SVM Predicted") + 
  ylab(label = "Test Data") +
  scale_fill_gradient(name = "Freq",
                      low = "#FFFFFF",
                      high = "#5E0000") + 
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(label = "Sigma Airlines Inc.")
svm2.heatmap

# Showing the trained SVM model
svmOutput3
# Using the trained SVM model to predict the testset
svmPred3 <- predict(svmOutput3, testData3)
# Comparing the testset and predicted result
compTable3 <- data.frame(testData3[,1],svmPred3)
compTable3 <- table(compTable3)
compTable3
# Calculating the prediction correction percentage
percentCorrect3 <- (compTable3[1,3]+compTable3[2,2]+compTable3[3,1])/sum(compTable3)
percentCorrect3
# Drawing the heatmap for prediction results
compTable3 <- data.frame(compTable3)
svm3.heatmap <- ggplot(data = compTable3, mapping = aes(x = svmPred3,
                                                        y = testData3...1.,
                                                        fill = Freq)) +
  geom_tile() +
  xlab(label = "SVM Predicted") + 
  ylab(label = "Test Data") +
  scale_fill_gradient(name = "Freq",
                      low = "#FFFFFF",
                      high = "#5E0000") + 
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(label = "FlyFast Airways Inc.")
svm3.heatmap