# Load necessary libraries
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(readxl)) install.packages("readxl")
if (!require(writexl)) install.packages("writexl")

library(tidyverse)
library(readxl)
library(writexl)

# Function to expand 'Sequence' into separate columns
expand_sequence <- function(data, sequence_column = "Sequence") {
  data %>%
    # Add a new column to check the length of each sequence
    mutate(Sequence_Length = nchar(as.character(.[[sequence_column]]))) %>%
    # Filter out rows where the sequence length is not equal to 6
    filter(Sequence_Length == 6) %>%
    # Remove the helper column as it's no longer needed
    select(-Sequence_Length) %>%
    # Separate the sequence into individual characters
    separate(col = sequence_column, into = paste0("Character_", 1:6), sep = 1:6, remove = TRUE)
}

# Mode calculation function
Mode <- function(x) {
  ux <- unique(x[!is.na(x)])  # Only consider non-NA values for mode calculation
  if (length(ux) == 0) return(NA)  # Return NA if all values are NA
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to remove outliers using IQR and delete entire rows containing outliers
remove_outliers <- function(data) {
  # For each numeric column, calculate the IQR and define bounds
  for (col_name in names(data)[sapply(data, is.numeric)]) {
    Q1 <- quantile(data[[col_name]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col_name]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Filter out rows where the column value is an outlier
    data <- data[data[[col_name]] >= lower_bound & data[[col_name]] <= upper_bound, ]
  }
  return(data)
}

# Updated function to handle entire data cleaning process including removing the 'Model' column
remove_and_fill_columns <- function(data) {
  # Remove the 'Model' column first
  data <- data %>% select(-Model)
  
  # Calculate missing values percentage
  total_rows <- nrow(data)
  missing_percentage <- sapply(data, function(x) sum(is.na(x)) / total_rows * 100)
  
  # Identify columns to remove
  columns_to_remove <- names(missing_percentage[missing_percentage > 5])
  
  # Remove these columns
  data <- data[, !(names(data) %in% columns_to_remove)]
  
  # Fill remaining missing values with the mode
  for (col_name in names(data)) {
    if (any(is.na(data[[col_name]]))) {
      mode_value <- Mode(data[[col_name]])
      if (!is.na(mode_value)) {
        data[[col_name]][is.na(data[[col_name]])] <- mode_value
      }
    }
  }
  
  # Remove rows with outliers
  #data <- remove_outliers(data)
  
  # Remove columns with only one unique value
  data <- data[, sapply(data, function(x) length(unique(na.omit(x))) > 1)]
  
  data$Class <- gsub("class7", "7", data$Class)  # Replace 'class7' with '7'
  data$Class <- gsub("class1", "1", data$Class)  # Replace 'class7' with '7'
  data$Class <- as.numeric(data$Class)  # Convert the column to numeric
  
  return(data)
}

# Combined function to read, expand, clean, and save data
process_and_save_data <- function(file_path, output_path) {
  # Read data, accounting for 'N.A.' as a missing value
  data <- read_excel(file_path, na = "N.A.")  # Treat 'N.A.' as NA in R
  
  # Expand the 'Sequence' column
  expanded_data <- expand_sequence(data)
  
  # Clean the dataset
  cleaned_data <- remove_and_fill_columns(expanded_data)
  
  # Print the cleaned data
  print(cleaned_data)
  
  # Save the cleaned data
  write_xlsx(cleaned_data, output_path)
}

# Specify paths
input_file_path <- "waltzdb_export.xlsx"
output_file_path <- "cleaned_waltzdb_export.xlsx"

# Execute the process
process_and_save_data(input_file_path, output_file_path)