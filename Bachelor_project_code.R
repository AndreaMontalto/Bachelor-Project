#Bachelor Project --
library(readxl)
data1 <- read_xlsx('capstone_airline_reviews3.xlsx')
data2 <- read.csv('Dataset2.csv')


## DATA cleaning ## 

# Working on dataset 2# 

#removing uncessary variables 

data2 <- subset(data2, select = -c(X, Airline.Name, Route, Wifi...Connectivity)) 
str(data2)
#Checking for missing values 
NAs_data2 <- sum(is.na(data2))

apply()
## Data formatting



