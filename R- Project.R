install.packages("zoo")
library(zoo)
install.packages("dplyr")
library(dplyr) 
install.packages("ggplot2")
library(ggplot2)

## Data Pre-Processing

#set the work directory
setwd("E:\\R (Data Preprocessing)")

#read the data file
dataset <- read.csv('G4_howell.csv')

#get structure of the data
str(dataset)

#the data dimension 
dim(dataset)

#Display some statics about the data
summary(dataset)

#number of rows and columns 
print(nrow(dataset))
print(ncol(dataset))

#Get all rows contain missing data
dataset[ ! complete.cases(dataset), ]

#count missing values in dataset
sum(is.na(dataset))

#number of missing value in each column in dataset
colSums(is.na(dataset))

#fill missing data by last observation
dataset$weight <- na.locf(dataset$weight)

#drop column with missing values 
dataset$Overweight <- NULL
dataset

#delete the dicimal number of age column to get clear age by year
dataset$age <- round(dataset$age)

#remove text from numeric data
dataset$weight<-gsub(" kg", " ",dataset$weight)

#convert dataset$weight data type from chr to num 
dataset$weight <- as.numeric(dataset$weight)

#Filter the dataset according to female gender.
print(filter1 <- dataset[dataset$sex == "F", ])

#sort data according to sex using dplyr Package
dataset2 <- dataset %>% arrange(sex)
dataset2

#Re-coding sex column.
dataset$code[dataset$sex == "M"] = "1"
dataset$code[dataset$sex == "F"] = "2"
dataset

## Data Visualization

#visualize age - Bar Chart
ggplot(data = dataset, aes(x = age)) + 
  geom_bar(fill = "darkblue", color = "white", alpha = 0.5) +
  ggtitle("Population Age") +
  labs(y = "Number of populations", x = ("Age of populations"))

#visualize age according to gender - Histogram
ggplot(data = dataset, aes(x = age, fill = sex)) + 
  geom_histogram(binwidth = 5., alpha = 0.5) +
  ggtitle("Population Age") +
  labs(y = "Number of populations", x = ("Age of populations"))

#Scatter plot between age and weight
ggplot(dataset, aes(age, weight)) + 
  geom_point(aes(color=weight)) + 
  labs(x="Age of population", y="Weight")
draw_sc + geom_point(aes(color=weight)) + stat_smooth(se=FALSE)

#visualize weight of population
ggplot(dataset, aes(x = age)) +
  geom_bar( fill = "darkblue", color = "white") +
  ggtitle("Population Ages") +
  xlab("age") +
  ylab("Frequency")

#visualize gender of population
ggplot(dataset, aes(x = sex)) +
  geom_bar( fill = "darkblue", color = "white") +
  ggtitle("Population Genders") +
  xlab("sex") +
  ylab("Frequency")

#Advanced Visualization 

#know the gender that has the more height
heightMean <- aggregate(height ~ sex, data = dataset, mean)
ggplot(heightMean, aes(x = sex, y = height)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  ggtitle("Mean of height according to gender") +
  xlab("gender") +
  ylab("Mean of height")

#know the age that has the more weight
weightMean <- aggregate(weight ~ age, data = dataset, mean)
ggplot(weightMean, aes(x = age, y = weight)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  ggtitle("Mean of weight according to age") +
  xlab("age") +
  ylab("Mean of Weight")