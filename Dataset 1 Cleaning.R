library(readxl)
capstone_airline_reviews3 <- read_excel("capstone_airline_reviews3.xlsx")

# Remove all rows where there are NA values in all columns
cleaned_dataset <- capstone_airline_reviews3[complete.cases(capstone_airline_reviews3), ]

# Put the review_date column in date format so we can use it for analyses
cleaned_dataset$review_date <- as.Date(cleaned_dataset$review_date, format = "%dth %B %Y")
cleaned_dataset$review_date <- format(cleaned_dataset$review_date, "%d-%m-%Y")
cleaned_dataset$review_date <- as.Date(cleaned_dataset$review_date, format = "%d-%m-%Y")

# Sort the dataset based on review_date, from newest to oldest
cleaned_dataset <- cleaned_dataset[order(cleaned_dataset$review_date, decreasing = TRUE), ]

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
cleaned_dataset$date_flown <- sapply(cleaned_dataset$date_flown, convert_to_date_format)

# NOTE: Some rows have dates for column date_flown that are wrong, for example as year "2089". Do we remove these rows?

# Remove unnecessary columns from the dataset
cleaned_dataset <- subset(cleaned_dataset, select = -author)
cleaned_dataset <- subset(cleaned_dataset, select= -airline)
cleaned_dataset <- subset(cleaned_dataset, select= -aircraft)
cleaned_dataset <- subset(cleaned_dataset, select= -route)
cleaned_dataset <- subset(cleaned_dataset, select= -date_flown)

# Make "recommended" column binary instead of containing yes or no
# First test whether there are more values than ys or no in this column
recommended_counts <- table(cleaned_dataset$recommended)
print(recommended_counts) # There are not any other values in this column, so I can make it binary
cleaned_dataset$recommended <- ifelse(cleaned_dataset$recommended == "yes", 1, 0)

# Create ID column
cleaned_dataset$ID <- seq_along(cleaned_dataset$recommended)

#creating binary columns for the type of traveller 
names(cleaned_dataset)[names(cleaned_dataset) == "X"] <- "ID"
traveller_type <- subset(cleaned_dataset, select = c("ID", "traveller_type"))

colnames(traveller_type) <-c('ID','traveller_type')

for (c in unique(traveller_type[,2])){ 
  cleaned_dataset[,c]<- 0
}
for (x in 1:nrow(traveller_type)) {
  cleaned_dataset[cleaned_dataset$ID==traveller_type$ID[x],traveller_type$traveller_type[x]] <- 1
}

# Subset the cleaned_dataset to select the ID and cabin columns
seat_type <- subset(cleaned_dataset, select = c("ID", "cabin"))

# Sort the seat_type dataframe by the "cabin" column in descending order
seat_type <- seat_type[order(seat_type$cabin, decreasing = TRUE), ]

# Rename the column "cabin" to "seat type"
colnames(seat_type) <- c('ID', 'seat type')

# Extract unique values from the "seat type" column
unique_seat <- unique(seat_type[2])

# Initialize all columns corresponding to seat types to 0 in the cleaned_dataset
for (v in unique(seat_type[,2])){ 
  cleaned_dataset[, v] <- 0
}

# Assign 1 to corresponding seat type columns for each row in the cleaned_dataset
for (y in 1:nrow(seat_type)) {
  cleaned_dataset[cleaned_dataset$ID == seat_type$ID[y], seat_type$`seat type`[y]] <- 1
}

#Move column ID to the first position in the data set
id_index <- which(names(cleaned_dataset) == "ID")

# Move the "ID" column to the first position
cleaned_dataset <- cleaned_dataset[, c(id_index, setdiff(1:ncol(cleaned_dataset), id_index))]


