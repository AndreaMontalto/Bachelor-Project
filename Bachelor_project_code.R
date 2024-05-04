#Bachelor Project --
library(readxl)
library(dplyr)
data1 <- read_xlsx('capstone_airline_reviews3.xlsx')
data2 <- read.csv('Dataset2.csv')
## DATA cleaning ## 

# Working on dataset 2# 
str(data2) #20 different variables, however not all of them are necessary 

#removing uncessary variables 
data2_clean <- subset(data2, select = -c(Airline.Name, Aircraft, Review_Title, Verified, Date.Flown, Route, Wifi...Connectivity)) 
str(data2_clean)
#Checking for missing values 
NAs_data2 <- sum(is.na(data2)) #there are 52k missing values 


## DATA FORMATTING ##
library(lubridate)
#making numeric values actually numeric # 
data2_clean$Overall_Rating <- as.numeric(data2_clean$Overall_Rating)
#making date numeric
data2_clean$Review.Date <- dmy(data2_clean$Review.Date)

#counting NAs and missing values per significant variable
sum(is.na(data2_clean$Seat.Comfort))
sum(is.na(data2_clean$Cabin.Staff.Service))
sum(is.na(data2_clean$Food...Beverages))
sum(is.na(data2_clean$Ground.Service))
sum(is.na(data2_clean$Inflight.Entertainment))
sum(is.na(data2_clean$Value.For.Money))
sum(is.na(data2_clean$Recommended))
sum(is.na(data2_clean$Overall_Rating))
sum(is.na(data2_clean$Review.Date))
sum(data2_clean$Type.Of.Traveller=="")
sum(data2_clean$Seat.Type=="")
#transforming missing values in NAs for type of traveller and seat type
data2_clean$Type.Of.Traveller[data2_clean$Type.Of.Traveller==""]<-NA
data2_clean$Seat.Type[data2_clean$Seat.Type==""]<-NA


#means of significant variables
mean_overall<-round(mean(data2_clean$Overall_Rating, na.rm=TRUE),2)
mean_seat<-round(mean(data2_clean$Seat.Comfort, na.rm=TRUE),2)
mean_staff<-round(mean(data2_clean$Cabin.Staff.Service, na.rm=TRUE),2)
mean_food<-round(mean(data2_clean$Food...Beverages, na.rm=TRUE),2)
mean_service<-round(mean(data2_clean$Ground.Service, na.rm=TRUE),2)
mean_entrat<-round(mean(data2_clean$Inflight.Entertainment, na.rm=TRUE),2)
mean_value<-round(mean(data2_clean$Value.For.Money, na.rm=TRUE),2)

#changing NAs with means
data2_clean<- data2_clean%>%
  mutate(
    Overall_Rating = ifelse(is.na(Overall_Rating), mean_overall, Overall_Rating),
    Seat.Comfort = ifelse(is.na(Seat.Comfort), mean_seat, Seat.Comfort),
    Cabin.Staff.Service = ifelse(is.na(Cabin.Staff.Service), mean_staff, Cabin.Staff.Service),
    Food...Beverages = ifelse(is.na(Food...Beverages), mean_food, Food...Beverages),
    Ground.Service = ifelse(is.na(Ground.Service), mean_service, Ground.Service),
    Inflight.Entertainment = ifelse(is.na(Inflight.Entertainment), mean_entrat, Inflight.Entertainment),
    Value.For.Money = ifelse(is.na(Value.For.Money), mean_value, Value.For.Money)
  )

#eliminating remaining NAs
data2_clean<-na.omit(data2_clean)

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

#Removing unessary columns 
data2_clean <- subset(data2_clean, select = -c(Type.Of.Traveller, Seat.Type))
#changing recommended to binary 
data2_clean$Recommended <- ifelse(data2_clean$Recommended == "yes", 1, 0)
NA_val_dataset2 <- as.data.frame(colSums(is.na(data2_clean)))


#adjusting column names
colnames(data2_clean)<-c("ID", "Overall_Rating", "Review_Date", "Review", "Seat_Comfort", "Cabin_Service", "Food_Bev", "Ground_Service", "Entertainment", "Value_Money", "Recommended", "Solo Leisure", "Couple Leisure", "Business", "Family Leisure", "Premium Economy", "First Class", "Economy Class", "Business Class")
