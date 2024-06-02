#Bachelor Project --
library(readxl)
library(dplyr)
library(lubridate)
data1 <- read_xlsx('capstone_airline_reviews3.xlsx')
data2 <- read.csv('Dataset2.csv')
## DATA cleaning ## 


# Working on dataset 2# 
str(data2) #20 different variables, however not all of them are necessary 

#removing uncessary variables 
data2_clean <- subset(data2, select = -c(Airline.Name, Aircraft, Review_Title, Verified, Route, Wifi...Connectivity)) 
str(data2_clean)
#Checking for missing values 
NAs_data2 <- sum(is.na(data2)) #there are 52k missing values 
NAs_data1 <- sum(is.na(data1))

## DATA FORMATTING ##
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
unique_seat <- unique(seat_type["seat type"])

for (v in unique(seat_type[,2])){ 
  data2_clean[,v]<- 0
}

for (y in 1:nrow(seat_type)) {
  data2_clean[data2_clean$ID==seat_type$ID[y],seat_type$`seat type`[y]] <- 1
}

#Removing unessary columns 
data2_clean <- subset(data2_clean, select = -c(Type.Of.Traveller, Seat.Type, ID))
#changing recommended to binary 
data2_clean$Recommended <- ifelse(data2_clean$Recommended == "yes", 1, 0)
NA_val_dataset2 <- as.data.frame(colSums(is.na(data2_clean)))

# Changing the format of Date.Flown column to date 
data2_clean <- data2_clean %>%
  mutate(Date.Flown = as.Date(paste0(Date.Flown, "-01"), format="%B %Y-%d"))

# Calculate the difference between review date and flight date and filter out the rows where it's > 3 months
data2_clean <- data2_clean %>%
  filter(as.numeric(Review.Date - Date.Flown, units = "days") <= 91)

#adjusting column names
colnames(data2_clean)<-c("Overall_Rating", "Review_Date", "Review", "Date_Flown", "Seat_Comfort", "Cabin_Service", "Food_Bev", "Ground_Service", "Entertainment", "Value_Money", "Recommended", "Solo Leisure", "Couple Leisure", "Business", "Family Leisure", "Premium Economy", "First Class", "Economy Class", "Business Class")




### WORKING ON DATASET 1 ###

# Remove all rows where there are NA values in all columns
data1_clean <- data1[complete.cases(data1), ]
NAs_data1 <- sum(is.na(data1_clean))
# Put the review_date column in date format so we can use it for analyses
data1_clean$review_date <- as.Date(data1_clean$review_date, format = "%dth %B %Y")
data1_clean$review_date <- format(data1_clean$review_date, "%d-%m-%Y")
data1_clean$review_date <- as.Date(data1_clean$review_date, format = "%d-%m-%Y")

# Sort the dataset based on review_date, from newest to oldest
data1_clean <- data1_clean[order(data1_clean$review_date, decreasing = TRUE), ]

# Custom function to convert mixed date representations to consistent format in column "date_flown"
convert_to_date_format <- function(date_str) {
  # Check if the date string is numeric (assuming it's in the format "43586")
  if (grepl("^\\d+$", date_str)) {
    # Convert numeric string to Date object
    date <- as.Date(as.numeric(date_str), origin = "1970-01-01")
  } else {
    # Convert month-year string to Date object
    date <- as.Date(paste("01", date_str, sep = "-"), format = "%d-%B %Y")
  }
  # Format the date object to include only month and year ("%m-%Y")
  formatted_date <- format(date, "%m-%Y")
  return(formatted_date)
}

# Apply the function to the date_flown column
data1_clean$date_flown <- sapply(data1_clean$date_flown, convert_to_date_format)

# NOTE: Some rows have dates for column date_flown that are wrong, for example as year "2089". Do we remove these rows?

# Remove unnecessary columns from the dataset
data1_clean <- subset(data1_clean, select = -author)
data1_clean <- subset(data1_clean, select= -airline)
data1_clean <- subset(data1_clean, select= -aircraft)
data1_clean <- subset(data1_clean, select= -route)

# Make "recommended" column binary instead of containing yes or no
# First test whether there are more values than ys or no in this column
recommended_counts <- table(data1_clean$recommended)
print(recommended_counts) # There are not any other values in this column, so I can make it binary
data1_clean$recommended <- ifelse(data1_clean$recommended == "yes", 1, 0)

# Create ID column
data1_clean$ID <- seq_along(data1_clean$recommended)

#creating binary columns for the type of traveller 
names(data1_clean)[names(data1_clean) == "X"] <- "ID"
traveller_type <- subset(data1_clean, select = c("ID", "traveller_type"))

colnames(traveller_type) <-c('ID','traveller_type')

for (c in unique(traveller_type[,2])){ 
  data1_clean[,c]<- 0
}
for (x in 1:nrow(traveller_type)) {
  data1_clean[data1_clean$ID==traveller_type$ID[x],traveller_type$traveller_type[x]] <- 1
}

# Subset the data1_clean to select the ID and cabin columns
seat_type <- subset(data1_clean, select = c("ID", "cabin"))

# Sort the seat_type dataframe by the "cabin" column in descending order
seat_type <- seat_type[order(seat_type$cabin, decreasing = TRUE), ]

# Rename the column "cabin" to "seat type"
colnames(seat_type) <- c('ID', 'seat type')

# Extract unique values from the "seat type" column
unique_seat <- unique(seat_type[2])

# Initialize all columns corresponding to seat types to 0 in the data1_clean
for (v in unique(seat_type[,2])){ 
  data1_clean[, v] <- 0
}

# Assign 1 to corresponding seat type columns for each row in the data1_clean
for (y in 1:nrow(seat_type)) {
  data1_clean[data1_clean$ID == seat_type$ID[y], seat_type$`seat type`[y]] <- 1
}

#Move column ID to the first position in the data set
id_index <- which(names(data1_clean) == "ID")

# Move the "ID" column to the first position
data1_clean <- data1_clean[, c(id_index, setdiff(1:ncol(data1_clean), id_index))]


# Converting date flown to normal date format to calculate the difference later
data1_clean$date_flown <- as.Date(paste0("01-", data1_clean$date_flown), format="%d-%m-%Y")

# Calculate the difference between the review and flight dates
# And leave the reviews with <3 months difference
data1_clean <- data1_clean %>%
  filter(as.numeric(review_date - date_flown, units = "days") <= 91)

### PART 2 ###

# Check for NAs in the entire dataset
# Count NA values in each column
na_count_per_column <- sapply(data1_clean, function(x) sum(is.na(x)))

# Print the counts
print(na_count_per_column)

# We calculate the sum of columns for each row
# Check if the sum of these columns is greater than 0
# And subset the dataset to include only rows where at least one of the traveller type columns is 1
# Spoiler: nothing changes
data1_clean <- data1_clean[rowSums(data1_clean[c("Solo Leisure", "Family Leisure", "Couple Leisure", "Business")] == 1) > 0, ]

# Subset the dataset to include only rows where at least one of the cabin type columns is 1
# Spoiler: nothing changes
data1_clean <- data1_clean[rowSums(data1_clean[c("Premium Economy", "First Class", "Business Class", "Economy Class")] == 1) > 0, ]


# Remove columns ID, traveller_type and cabin 
data1_clean <- select(data1_clean, -c(ID, traveller_type, cabin))

#changing column names for merging
colnames(data1_clean)<-c("Overall_Rating", "Review_Date", "Review", "Date_Flown", "Seat_Comfort", "Cabin_Service", "Food_Bev", "Ground_Service", "Entertainment", "Value_Money", "Recommended", "Solo Leisure", "Couple Leisure", "Business", "Family Leisure", "Premium Economy", "First Class", "Economy Class", "Business Class")

#combining data1_clean and data2_clean in a single dataframe
df<-rbind(data1_clean,data2_clean)

#checking for duplicates and eliminating them
duplicate_rows<-duplicated(df)
unique_df<-df[!duplicate_rows, ]


# Add an ID column to the dataframe
unique_df$ID <- 1:nrow(unique_df)

# Reorder the columns to make ID the first column
unique_df <- unique_df[c("ID", setdiff(names(unique_df), "ID"))]


# Remove the flight date column as we don't need it anymore
unique_df$Date_Flown <- NULL

# Split the dataset into pre-covid and covid assuming that convid start is March 1st 
pre_covid <- unique_df %>%
  filter(Review_Date <= as.Date("2020-03-01"))
covid <- unique_df %>%
  filter(Review_Date > as.Date("2020-03-01"))


# Create training and testing sets for both pre
library(caret)
set.seed(123)

# Split pre_covid into training and testing sets
partition_pre_covid <- createDataPartition(y = pre_covid$ID, p = 0.80, list = FALSE)
training_pre_covid <- pre_covid[partition_pre_covid, ]
testing_pre_covid <- pre_covid[-partition_pre_covid, ]

# Split covid into training and testing sets
partition_covid <- createDataPartition(y = covid$ID, p = 0.80, list = FALSE)
training_covid <- covid[partition_covid, ]
testing_covid <- covid[-partition_covid, ]



### MODEL CHECK ###

# Fit the linear regression model
model_check <- lm(Overall_Rating ~ Seat_Comfort + Cabin_Service + Food_Bev + Ground_Service + Entertainment + Value_Money, 
            data = unique_df)

# Display the summary of the model to see the coefficients and other statistics
summary(model_check)


## LINEARITY
# Predicted values
predicted_values <- predict(model_check)

# Residuals
residuals <- residuals(model_check)

# Plot of residuals vs. predicted values
plot(predicted_values, residuals, main = "Residuals vs. Predicted Values", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# separating into individuals plots
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Plot for Seat_Comfort
ggplot(unique_df, aes(x = Seat_Comfort, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Residuals vs Seat Comfort", x = "Seat Comfort", y = "Residuals")

# Plot for Cabin_Service
ggplot(unique_df, aes(x = Cabin_Service, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Residuals vs Cabin Service", x = "Cabin Service", y = "Residuals")

# Plot for Food_Bev
ggplot(unique_df, aes(x = Food_Bev, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Residuals vs Food and Beverages", x = "Food and Beverages", y = "Residuals")

# Plot for Ground_Service
ggplot(unique_df, aes(x = Ground_Service, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Residuals vs Ground Service", x = "Ground Service", y = "Residuals")

# Plot for Entertainment
ggplot(unique_df, aes(x = Entertainment, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Residuals vs Entertainment", x = "Entertainment", y = "Residuals")

# Plot for Value_Money
ggplot(unique_df, aes(x = Value_Money, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Residuals vs Value for Money", x = "Value for Money", y = "Residuals")



## MULTICOLINEARITY

if (!require(car)) install.packages("car")
library(car)

# Calculate VIF
vif_values <- vif(model_check)
print(vif_values)


## HOMOSKEDASITY

# Scatter plot for homoskedasticity
plot(predicted_values, residuals, main = "Homoskedasticity Check: Residuals vs. Predicted Values", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red")


## NORMALITY
# QQ plot for normality of residuals
qqnorm(residuals)
qqline(residuals, col = "red")


#Multilateral regression model
precovid_model<-lm(Overall_Rating~Seat_Comfort+Cabin_Service+Food_Bev+Ground_Service+Entertainment+Value_Money, data = training_pre_covid)
summary(precovid_model)
testing_pre_covid$Pred<-predict(precovid_model, newdata=testing_pre_covid)

#testing moderating variables
moderating_model <- lm(Overall_Rating ~ Seat_Comfort + Cabin_Service + Food_Bev +
                         Ground_Service + Entertainment + Value_Money +
                         `Solo Leisure` * Seat_Comfort + `Solo Leisure` * Cabin_Service + `Solo Leisure` * Food_Bev + `Solo Leisure` * Ground_Service + `Solo Leisure` * Entertainment + `Solo Leisure` * Value_Money +
                         `Couple Leisure` * Seat_Comfort + `Couple Leisure` * Cabin_Service + `Couple Leisure` * Food_Bev + `Couple Leisure` * Ground_Service + `Couple Leisure` * Entertainment + `Couple Leisure` * Value_Money +
                         Business * Seat_Comfort + Business * Cabin_Service + Business * Food_Bev + Business * Ground_Service + Business * Entertainment + Business * Value_Money +
                         `Family Leisure` * Seat_Comfort + `Family Leisure` * Cabin_Service + `Family Leisure` * Food_Bev + `Family Leisure` * Ground_Service + `Family Leisure` * Entertainment + `Family Leisure` * Value_Money + 
                         `Premium Economy` * Seat_Comfort + `Premium Economy` * Cabin_Service + `Premium Economy` * Food_Bev + `Premium Economy` * Ground_Service + `Premium Economy` * Entertainment + `Premium Economy` * Value_Money +
                         `First Class` * Seat_Comfort +`First Class` * Cabin_Service + `First Class` * Food_Bev + `First Class` * Ground_Service + `First Class` * Entertainment + `First Class` * Value_Money + 
                         `Economy Class` * Seat_Comfort + `Economy Class` * Cabin_Service + `Economy Class` * Food_Bev + `Economy Class` * Ground_Service + `Economy Class` * Entertainment +`Economy Class` * Value_Money +
                         `Business Class`* Seat_Comfort + `Business Class`* Cabin_Service + `Business Class`* Food_Bev + `Business Class`* Ground_Service + `Business Class`* Entertainment + `Business Class`* Value_Money, data = training_pre_covid)
summary(moderating_model)
testing_pre_covid$Pred_m<-predict(moderating_model, newdata=testing_pre_covid)

#evaluating predictions normal model
MAE_pre <- mean(abs(testing_pre_covid$Pred - testing_pre_covid$Overall_Rating))
print(MAE_pre)
MSE_pre <- mean((testing_pre_covid$Pred - testing_pre_covid$Overall_Rating)^2)
print(MSE_pre)
RMSE_pre <- sqrt(MSE_pre)
print(RMSE_pre)
R_squared_pre <- summary(precovid_model)$r.squared
print(R_squared_pre)
#despite the low R_squared and high MSE the other two measures have a good low value, we can say that predictions fit acceptably

#evaluating predictions moderating model
MAE_pre_m <- mean(abs(testing_pre_covid$Pred_m - testing_pre_covid$Overall_Rating))
print(MAE_pre_m)
MSE_pre_m <- mean((testing_pre_covid$Pred_m - testing_pre_covid$Overall_Rating)^2)
print(MSE_pre_m)
RMSE_pre_m <- sqrt(MSE_pre_m)
print(RMSE_pre_m)
R_squared_pre_m <- summary(moderating_model)$r.squared
print(R_squared_pre_m)
#the moderating model predicts better than the normal model as seen by the improvement of all measures


#predicting on covid with the model for pre-covid
covid$Pred<-predict(moderating_model, newdata = covid)

#evaluating the predictions
MAE_post<-mean(abs(covid$Pred - covid$Overall_Rating))
print(MAE_post)
MSE_post <- mean((covid$Pred - covid$Overall_Rating)^2)
print(MSE_post)
RMSE_post <- sqrt(MSE_post)
print(RMSE_post)
#as we can see all measures have increased in value, demonstrating that the model does not predict well for the covid dataframe
#H2 is accepted


#H3 comparing covid to overall
#creating a dummy variable for covid
covid_start_date<- as.Date("2020-03-01")
unique_df$Covid <- ifelse(unique_df$Review_Date >= covid_start_date, 1, 0)
overall_model<-lm(Overall_Rating ~ Seat_Comfort + Cabin_Service + Food_Bev +
                    Ground_Service + Entertainment + Value_Money + Covid +
                    `Solo Leisure` * Seat_Comfort + `Solo Leisure` * Cabin_Service + `Solo Leisure` * Food_Bev + `Solo Leisure` * Ground_Service + `Solo Leisure` * Entertainment + `Solo Leisure` * Value_Money +
                    `Couple Leisure` * Seat_Comfort + `Couple Leisure` * Cabin_Service + `Couple Leisure` * Food_Bev + `Couple Leisure` * Ground_Service + `Couple Leisure` * Entertainment + `Couple Leisure` * Value_Money +
                    Business * Seat_Comfort + Business * Cabin_Service + Business * Food_Bev + Business * Ground_Service + Business * Entertainment + Business * Value_Money +
                    `Family Leisure` * Seat_Comfort + `Family Leisure` * Cabin_Service + `Family Leisure` * Food_Bev + `Family Leisure` * Ground_Service + `Family Leisure` * Entertainment + `Family Leisure` * Value_Money + 
                    `Premium Economy` * Seat_Comfort + `Premium Economy` * Cabin_Service + `Premium Economy` * Food_Bev + `Premium Economy` * Ground_Service + `Premium Economy` * Entertainment + `Premium Economy` * Value_Money +
                    `First Class` * Seat_Comfort +`First Class` * Cabin_Service + `First Class` * Food_Bev + `First Class` * Ground_Service + `First Class` * Entertainment + `First Class` * Value_Money + 
                    `Economy Class` * Seat_Comfort + `Economy Class` * Cabin_Service + `Economy Class` * Food_Bev + `Economy Class` * Ground_Service + `Economy Class` * Entertainment +`Economy Class` * Value_Money +
                    `Business Class`* Seat_Comfort + `Business Class`* Cabin_Service + `Business Class`* Food_Bev + `Business Class`* Ground_Service + `Business Class`* Entertainment + `Business Class`* Value_Money,data = unique_df)
summary(overall_model)
<<<<<<< HEAD

install.packages("strucchange")
library(strucchange)
chow_test_result <- sctest(overall_model, type = "Chow", point = break_point)
print(chow_test_result)

covid_model<-lm(Overall_Rating ~ Seat_Comfort + Cabin_Service + Food_Bev +
                    Ground_Service + Entertainment + Value_Money +
                    `Solo Leisure` * Seat_Comfort + `Solo Leisure` * Cabin_Service + `Solo Leisure` * Food_Bev + `Solo Leisure` * Ground_Service + `Solo Leisure` * Entertainment + `Solo Leisure` * Value_Money +
                    `Couple Leisure` * Seat_Comfort + `Couple Leisure` * Cabin_Service + `Couple Leisure` * Food_Bev + `Couple Leisure` * Ground_Service + `Couple Leisure` * Entertainment + `Couple Leisure` * Value_Money +
                    Business * Seat_Comfort + Business * Cabin_Service + Business * Food_Bev + Business * Ground_Service + Business * Entertainment + Business * Value_Money +
                    `Family Leisure` * Seat_Comfort + `Family Leisure` * Cabin_Service + `Family Leisure` * Food_Bev + `Family Leisure` * Ground_Service + `Family Leisure` * Entertainment + `Family Leisure` * Value_Money + 
                    `Premium Economy` * Seat_Comfort + `Premium Economy` * Cabin_Service + `Premium Economy` * Food_Bev + `Premium Economy` * Ground_Service + `Premium Economy` * Entertainment + `Premium Economy` * Value_Money +
                    `First Class` * Seat_Comfort +`First Class` * Cabin_Service + `First Class` * Food_Bev + `First Class` * Ground_Service + `First Class` * Entertainment + `First Class` * Value_Money + 
                    `Economy Class` * Seat_Comfort + `Economy Class` * Cabin_Service + `Economy Class` * Food_Bev + `Economy Class` * Ground_Service + `Economy Class` * Entertainment +`Economy Class` * Value_Money +
                    `Business Class`* Seat_Comfort + `Business Class`* Cabin_Service + `Business Class`* Food_Bev + `Business Class`* Ground_Service + `Business Class`* Entertainment + `Business Class`* Value_Money,data = covid)
summary(covid_model)


#the p-value of the covid dummy variable is significant in the overall model, showing the significant impact of covid in influencing the customers preferences in the airline industry.
#the chow test determines whether the relationship between the DV and IVs has changed significantly due to COVID-19. since the p-value is <0.05 it demonstrates a significant structural break occured and the changes will probably last in the long-term


#passenger by type
sum(unique_df$`Solo Leisure`==1)
sum(unique_df$`Solo Leisure`==1&unique_df$Covid==1)
sum(unique_df$`Couple Leisure`==1)
sum(unique_df$`Couple Leisure`==1&unique_df$Covid==1)
sum(unique_df$Business==1)
sum(unique_df$Business==1&unique_df$Covid==1)
sum(unique_df$`Family Leisure`==1)
sum(unique_df$`Family Leisure`==1&unique_df$Covid==1)
sum(unique_df$`Premium Economy`==1)
sum(unique_df$`Premium Economy`==1&unique_df$Covid==1)
sum(unique_df$`First Class`==1)
sum(unique_df$`First Class`==1&unique_df$Covid==1)
sum(unique_df$`Economy Class`==1)
sum(unique_df$`Economy Class`==1&unique_df$Covid==1)
sum(unique_df$`Business Class`==1)
sum(unique_df$`Business Class`==1&unique_df$Covid==1)
=======
#we lose significance for Food_Bev from the pre_covid model

## FEATURE SELECTION ##
install.packages("FSelectorRcpp")
library(FSelectorRcpp)
install.packages("infotheo")
library(infotheo)

#PRE-covid#
#discretizise data 

pre_covid$Seat_Comfort_disc <- discretize(pre_covid$Seat_Comfort, disc="equalfreq", nbins=3)
pre_covid$Cabin_Service_disc <- discretize(pre_covid$Cabin_Service, disc="equalfreq", nbins=3)
pre_covid$Food_Bev_disc <- discretize(pre_covid$Food_Bev, disc="equalfreq", nbins=3)
pre_covid$Ground_Service_disc <- discretize(pre_covid$Ground_Service, disc="equalfreq", nbins=3)
pre_covid$Entertainment_disc <- discretize(pre_covid$Entertainment, disc="equalfreq", nbins=3)
pre_covid$Value_Money_disc <- discretize(pre_covid$Value_Money, disc="equalfreq", nbins=3)

#pre_covid 
mutual_info_seat_pre <- mutinformation(pre_covid$Overall_Rating, pre_covid$Seat_Comfort_disc)
mutual_info_cabin_pre <- mutinformation(pre_covid$Overall_Rating, pre_covid$Seat_Comfort_disc)
mutual_info_food_pre <- mutinformation(pre_covid$Overall_Rating, pre_covid$Food_Bev_disc)
mutual_info_gservice_pre <- mutinformation(pre_covid$Overall_Rating, pre_covid$Ground_Service_disc)
mutual_info_entertainment_pre <- mutinformation(pre_covid$Overall_Rating, pre_covid$Entertainment_disc)
mutual_info_money_pre <- mutinformation(pre_covid$Overall_Rating, pre_covid$Value_Money_disc)

mutual_info_pre_covid <- data.frame(Variables = c('cabin', 'seat','entertainment','food', 'gservice','value money'), 
                                      IG = c(mutual_info_cabin_pre, mutual_info_seat_pre, mutual_info_entertainment_pre, mutual_info_food_pre, mutual_info_gservice_pre, mutual_info_money_pre))

#COVID#

#discretizize data
covid$Seat_Comfort_disc <- discretize(covid$Seat_Comfort, disc="equalfreq", nbins=3)
covid$Cabin_Service_disc <- discretize(covid$Cabin_Service, disc="equalfreq", nbins=3)
covid$Food_Bev_disc <- discretize(covid$Food_Bev, disc="equalfreq", nbins=3)
covid$Ground_Service_disc <- discretize(covid$Ground_Service, disc="equalfreq", nbins=3)
covid$Entertainment_disc <- discretize(covid$Entertainment, disc="equalfreq", nbins=3)
covid$Value_Money_disc <- discretize(covid$Value_Money, disc="equalfreq", nbins=3)

#computing information gain
mutual_info_seat_covid <- mutinformation(covid$Overall_Rating, covid$Seat_Comfort_disc)
mutual_info_cabin_covid <- mutinformation(covid$Overall_Rating, covid$Seat_Comfort_disc)
mutual_info_food_covid <- mutinformation(covid$Overall_Rating, covid$Food_Bev_disc)
mutual_info_gservice_covid <- mutinformation(covid$Overall_Rating, covid$Ground_Service_disc)
mutual_info_entertainment_covid <- mutinformation(covid$Overall_Rating, covid$Entertainment_disc)
mutual_info_money_covid <- mutinformation(covid$Overall_Rating, covid$Value_Money_disc)

mutual_info_covid <- data.frame(Variables = c('cabin', 'seat','entertainment','food', 'gservice','value money'), 
                                    IG = c(mutual_info_cabin_covid, mutual_info_seat_covid, mutual_info_entertainment_covid, mutual_info_food_covid, mutual_info_gservice_covid, mutual_info_money_covid))

>>>>>>> cfe7cfd2e11e930537c13bfebb7ec4eed2fdf6f8
