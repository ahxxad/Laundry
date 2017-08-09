# Laundary Anlysis By Ahmed ABDULSALAM ALI

install.packages("ggplot2")
install.packages("reshape2")


#Task1
#(a) Read the file test.csv into a variable named dt.laundry.

dt.laundry <-  function() { 
x <- read.csv("test.csv")
}
dt.laundry <- dt.laundry() 

#(b) Count the number of records in dt.laundry.
nrow(dt.laundry)
#(c) Count the number of columns in dt.laundry.
ncol(dt.laundry)
# (b)+(c) other way
dim(dt.laundry)


#Task2
#(a) List all records with missing data (NA).
is.na(dt.laundry)

#(b) Count the total number of records with missing data (NA).
sum(is.na(dt.laundry))


#Task3
#(a)Calculate the average for "VM Coins"
Avarage <- dt.laundry$VM.Coins
mean(Avarage, na.rm = TRUE)
mean(Avarage)

#(b)Calculate the minimum for "VM Coins"
dt.laundry[which.min(dt.laundry$VM.Coins),]

#(c)
dt.laundry[which.max(dt.laundry$VM.Coins),]

#Task4 Replace the missing data in each column with its mean, respectively.
library(ggplot2)
library(Hmisc)
dt.laundry$VM.Coins <- impute(dt.laundry$VM.Coins, fun=mean)
dt.laundry$VM.Coins <- as.integer(dt.laundry$VM.Coins)
dt.laundry$W1 <- impute(dt.laundry$W1, fun=mean)
dt.laundry$W1 <- as.integer(dt.laundry$W1)
dt.laundry$W2 <- impute(dt.laundry$W2, fun=mean)
dt.laundry$W2 <- as.integer(dt.laundry$W2)
dt.laundry$W3 <- impute(dt.laundry$W3, fun=mean)
dt.laundry$W3 <- as.integer(dt.laundry$W3)
dt.laundry$W4 <- impute(dt.laundry$W4, fun=mean)
dt.laundry$W4 <- as.integer(dt.laundry$W4)
dt.laundry$W5 <- impute(dt.laundry$W5, fun=mean)
dt.laundry$W5 <- as.integer(dt.laundry$W5)
dt.laundry$W6 <- impute(dt.laundry$W6, fun=mean)
dt.laundry$W6 <- as.integer(dt.laundry$W6)
dt.laundry$D7 <- impute(dt.laundry$D7, fun=mean)
dt.laundry$D7 <- as.integer(dt.laundry$D7)
dt.laundry$D8 <- impute(dt.laundry$D8, fun=mean)
dt.laundry$D8 <- as.integer(dt.laundry$D8)
dt.laundry$D9 <- impute(dt.laundry$D9, fun=mean)
dt.laundry$D9 <- as.integer(dt.laundry$D9)
dt.laundry$D10 <- impute(dt.laundry$D10, fun=mean)
dt.laundry$D10 <- as.integer(dt.laundry$D10)


#Task5 Create a ggplot chart to display the total sales per day. Total sales per day is defined
#as the sum of sales for VM coins, dryers, and washers. 
#[note: you can use the rowSums() function]
dt.laundry$Total_Sales <- rowSums(dt.laundry[2:12])
dt.laundry$Total_Sales <- as.integer(dt.laundry$Total_Sales)

ggplot(data = dt.laundry, aes(x = Date, y = Total_Sales)) + geom_point()


#Task6 In this question, you need to use the reshape2 library to transform the original dataset
#dt.laundry into dt.melt. The new dataset should consist of a new column name Weekday.
#[Note: use the functions weekdays() in the lubridate library]
##Write an R script to calculate the total sales for each day and list the record that has the highest
#total sales.
library(lubridate) 
class(dt.laundry$Date)
dt.laundry$Date <- as.Date(dt.laundry$Date, format= "%d/%m/%Y")
class(dt.laundry$Date)
dt.laundry$Weekday <- weekdays(dt.laundry$Date)
library(reshape2)
dt.melt <- melt(dt.laundry, id.vars= c("Date", "Weekday", "Total_Sales"))
View(dt.melt)
dt.melt[which.max(dt.melt$Total_Sales),]
dt.melt$Total_Sales <- NULL


#Q7 Task W
dt.washer <- dt.melt[grep("W",dt.melt$variable),]
dt.washer.sum <- aggregate(dt.washer$value, by=list(Variable=dt.washer$variable), FUN=sum)
colnames(dt.washer.sum)[colnames(dt.washer.sum)=="x"]<-"Total"
colnames(dt.washer.sum)[colnames(dt.washer.sum)=="Variable"]<-"Washer"
dt.washer.sum[1:nrow(dt.washer.sum),1:ncol(dt.washer.sum)]

ggplot(dt.washer.sum, aes(x = Washer, y = Total,fill=Washer)) + geom_bar(stat = "identity")


#Task8 D
dt.dryer <- dt.melt[grep("D",dt.melt$variable),]
dt.dryer.sum <- aggregate(dt.dryer$value, by=list(Variable=dt.dryer$variable), FUN=sum)
colnames(dt.dryer.sum)[colnames(dt.dryer.sum)=="x"] <- "Total"
colnames(dt.dryer.sum)[colnames(dt.dryer.sum)=="Variable"] <- "Dryer"
dt.dryer.sum[1:nrow(dt.dryer.sum),1:ncol(dt.dryer.sum)]

ggplot(dt.dryer.sum, aes(x = Dryer, y = Total,fill=Dryer)) + geom_bar(stat = "identity")