#Bachelor Project --
library(readxl)
library(dplyr)
data1 <- read_xlsx('capstone_airline_reviews3.xlsx')
data2 <- read.csv('Dataset2.csv')
## DATA cleaning ## 

# Working on dataset 2# 
str(data2) #20 different variables, however not all of them are necessary 

#removing uncessary variables 
data2_clean <- subset(data2, select = -c(Airline.Name, Aircraft, Verified, Date.Flown, Route, Wifi...Connectivity)) 
str(data2_clean)
#Checking for missing values 
NAs_data2 <- sum(is.na(data2)) #there are 52k missing values 


## DATA FORMATTING ##
library(lubridate)
#making numeric values actually numeric # 
data2_clean$Overall_Rating <- as.numeric(data2_clean$Overall_Rating)
#making date numeric
data2_clean$Review.Date <- dmy(data2_clean$Review.Date)
#I would consider getting rid of reviews with NA values in overall rating 

#creating binary columns for the type of traveller 
names(data2_clean)[names(data2_clean) == "X"] <- "ID"
traveller_type <- subset(data2_clean, select = c("ID", "Type.Of.Traveller"))

colnames(traveller_type) <-c('ID','traveller_type')

for (c in unique(traveller_type[,2])){ 
  data2_clean[,c]<- 0
}
for (x in 1:nrow(traveller_type)) {
  data2_clean[data2_clean$ID==traveller_type$ID[x],traveller_type$traveller_type[x]] <- 1
}


#Creating binary variables for seat type 
seat_type <- subset(data2_clean, select = c("ID", "Seat.Type"))
seat_type<- seat_type[order(seat_type$Seat.Type, decreasing = TRUE), ]
colnames(seat_type) <- c('ID', 'seat type')
unique_seat <- unique(seat_type[2])
unique_seat <- subset(unique_seat, select = c("Economy Class", "Business Class"))
for (v in unique(seat_type[,2])){ 
  data2_clean[,v]<- 0
}

for (y in 1:nrow(seat_type)) {
  data2_clean[data2_clean$ID==seat_type$ID[y],seat_type$`seat type`[y]] <- 1
}

NA_val_dataset2 <- as.data.frame(colSums(is.na(data2_clean)))





