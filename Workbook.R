#Workbook
setwd("~/datasciencecoursera/RepData_PeerAssessment1")
#Part1 - Load the data
data<-read.csv("activity.csv", header=T, stringsAsFactors = F)
str(data)
#Transform the date variable from character string to Date type
data$date<-as.Date(data$date, format="%Y-%m-%d")
#Filter out the missing data
table(is.na(data$steps))
data_NoNa<-data[complete.cases(data),]

#Part2
library(dplyr)
TotalStepsByDate<- data_NoNa %>% group_by(date) %>% summarize(totalSteps = sum(steps))
hist(TotalStepsByDate$totalSteps, breaks=30, xlab="Bins", main="Histogram: Total Steps by Date")

#Part3
mean(TotalStepsByDate$totalSteps)
median(TotalStepsByDate$totalSteps)

##Not needed
stats<-data_NoNa %>% group_by(date) %>%summarize(totalSteps = sum(steps), StepsMean = mean(steps), StepsMedian=median(steps))
View(stats)
plot(stats$date,stats$totalSteps, type="l", xlab="Date", ylab="Total Steps", main="Total Steps by Date")
plot(stats$date,stats$StepsMean, type="l", xlab="Date", ylab="Average Steps", main="Average Steps by Date")
plot(stats$date,stats$StepsMedian, type="l", xlab="Date", ylab="Median Steps", main="Median Steps by Date")

#Part4
AvgStepsByInterval<- data_NoNa %>% group_by(interval) %>% summarize(avgSteps=mean(steps))
plot(AvgStepsByInterval$interval, AvgStepsByInterval$avgSteps, type="l", xlab="5-Min Interval", ylab="Avg Steps")
#Part5
max(AvgStepsByInterval$avgSteps)
AvgStepsByInterval[which.max(AvgStepsByInterval$avgSteps),]

#Part6

missing<-table(is.na(data$steps))[2]

AvgStepsByInterval<- data_NoNa %>% group_by(interval) %>% summarize(avgSteps=mean(steps))
imputedDataset <- data 
for (i in 1:nrow(imputedDataset)) {
  if (is.na(imputedDataset$steps[i])) {
    imputedDataset$steps[i] <- AvgStepsByInterval[which(imputedDataset$interval[i] == AvgStepsByInterval$interval), ]$avgSteps
  }
}
View(imputedDataset)
TotalStepsByDate<- imputedDataset %>% group_by(date) %>% summarize(totalSteps = sum(steps))
hist(TotalStepsByDate$totalSteps, breaks=30, xlab="Bins", main="Histogram: Total Steps by Date (Imputed Data Set)")
mean(TotalStepsByDate$totalSteps)
median(TotalStepsByDate$totalSteps)
#Comparing means and medians reveals that both new means are the same while the new median is greater than the old median.




