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

# Remove column "author" from the dataset
cleaned_dataset <- subset(cleaned_dataset, select = -author)

# Make "recommended" column binary instead of containing yes or no
# First test whether there are more values than ys or no in this column
recommended_counts <- table(cleaned_dataset$recommended)
print(recommended_counts) # There are not any other values in this column, so I can make it binary
cleaned_dataset$recommended <- ifelse(cleaned_dataset$recommended == "yes", 1, 0)










