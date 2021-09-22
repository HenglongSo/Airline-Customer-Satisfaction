####################################################################################################################
# Read Data
ProjectData <-read.csv( "spring19survey.csv")
View(ProjectData)
summary(ProjectData)
str(ProjectData)
dim(ProjectData)
length(ProjectData)
#undestand the Satisfaction data
#we need to format data for saticfaction data to whole number
################################################### Data Cleansing #################################################

# Show NAs in each colum
colSums(is.na(ProjectData))

# Treat NA values
CleanData <- ProjectData
CleanData$Satisfaction [is.na(ProjectData$Satisfaction)] <- 0
CleanData$Departure.Delay.in.Minutes[is.na(ProjectData$Departure.Delay.in.Minutes)] <-0
CleanData$Arrival.Delay.in.Minutes[is.na(ProjectData$Arrival.Delay.in.Minutes)] <-0
CleanData$Flight.time.in.minutes[is.na(ProjectData$Flight.time.in.minutes)] <-0
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
#################################################### Client Data ##############################################
# select client data
View(CleanData)
ClientData <- CleanData[CleanData$Partner.Name== 'Cheapseats Airlines Inc.', ]
data_Fly_Fast <- CleanData[CleanData$Partner.Name== 'FlyFast Airlines Inc.', ]
data_Sigma_Airlines <- CleanData[CleanData$Partner.Name== 'Sigma Airlines Inc.', ]
ClientData <-subset(ClientData, select = -c(Partner.Name,Partner.Code))
View(ClientData)

install.packages("grid")
library(grid)
install.packages("Matrix")
library(Matrix)
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
#nstall.packages("RJSONIO") 
library(RJSONIO)
library(ggplot2)

#Create buckets for transforming into categorical value for the satisfaction varia
createbucketsurvey <- function(vec){
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 3] <- "High"
  vBuckets[vec < 3] <- "Low"
  return(vBuckets)
}#This function categorizes into high low and average

CustSat <- createbucketsurvey(data$Satisfaction)

#Create buckets for transforming into categorical value for all the other varibles
newfunction <- function(vec){
  q <- quantile(vec, probs =c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
} #This function categorizes into high low and average

#convert all the numerical data into categorical data
Age1 <- newfunction(data$Age)
Price.Sensitivity1 <- newfunction(data$Price.Sensitivity)
Flights.Per.Year1 <- newfunction(data$Flights.Per.Year)
Shopping.Amount.at.Airport1 <-newfunction(data$Shopping.Amount.at.Airport)
Eating.and.Drinking.at.Airport1 <- newfunction(data$Eating.and.Drinking.at.Airport)
Scheduled.Departure.Hour1 <- newfunction(data$Scheduled.Departure.Hour)
Departure.Delay.in.Minutes1 <- newfunction(data$Departure.Delay.in.Minutes)
Arrival.Delay.in.Minutes1 <- newfunction(data$Arrival.Delay.in.Minutes)
Flight.time.in.minutes1 <- newfunction(data$Flight.time.in.minutes)
Flight.Distance1 <- newfunction(data$Flight.Distance)
Loyalty1 <- newfunction(data$Loyalty)


#creating a dataframe 
ruleDF <- data.frame(CustSat,
                     Age1,
                     Price.Sensitivity1,
                     Flights.Per.Year1,
                     Shopping.Amount.at.Airport1,
                     Eating.and.Drinking.at.Airport1,
                     Scheduled.Departure.Hour1,
                     Departure.Delay.in.Minutes1,
                     Arrival.Delay.in.Minutes1,
                     Flight.time.in.minutes1,
                     Flight.Distance1,
                     Loyalty1,
                     data$Arrival.Delay.greater.5.Mins,
                     data$Long.Duration.Trip,
                     data$Airline.Status,
                     data$Gender,
                     data$Type.of.Travel,
                     data$Class)# Creates a dataframe
Airline <- as(ruleDF,"transactions")  #creates a transactional matrix
View(Airline)

itemFrequency(Airline) 
itemFrequencyPlot(Airline, support=0.3,cex.names=0.8,topN = 15, col = rainbow(15))


#Mine frequent itemsets, association rules or association hyperedges using the Apriori algorithm for High Customer Satisfaction
rulesetHigh <- apriori(Airline,
                   parameter = list(support=0.05,confidence=0.5),
                   appearance = list(default="lhs",rhs=("CustSat=High"))) #Apriori function find these relations based on the frequency of items bought together.
inspect(head(sort(rulesetHigh, by = "lift"), 20)) # It summarizes all relevant options, plots and statistics that should be usually considered.


#Remove the reductdant rules for High Customer Satisfaction
subsetRules <- which(colSums(is.subset(rulesetHigh, rulesetHigh)) > 1) # get subset rules in vector
length(subsetRules) 
rulesetHigh <- rulesetHigh[-subsetRules] # remove subset rules. 
summary(rulesetHigh) #Displays the summary of the data frame

#Plots the graph of the top 10 rules for high Customer satisfaction
plot(head(sort(rulesetHigh, by = "lift"), 10), method = "graph", engine = "htmlwidget") 


#Mine frequent itemsets, association rules or association hyperedges using the Apriori algorithm for Low Customer Satisfaction
rulesetLow <- apriori(Airline,
                       parameter = list(support=0.05,confidence=0.5),
                       appearance = list(default="lhs",rhs=("CustSat=Low"))) #Apriori function find these relations based on the frequency of items bought together.
inspect(head(sort(rulesetLow, by = "lift"), 20)) # It summarizes all relevant options, plots and statistics that should be usually considered.

#Remove the reductdant rules for High Customer Satisfaction
subsetRules <- which(colSums(is.subset(rulesetLow, rulesetLow)) > 1) # get subset rules in vector
length(subsetRules)  #> 3913
rulesLow <- rulesetLow[-subsetRules]
summary(rulesetLow) #Displays the summary of the data frame

#Plots the graph of the top 10 rules for high Customer satisfaction
plot(head(sort(rulesetLow, by = "lift"), 10), method = "graph", engine = "htmlwidget")
