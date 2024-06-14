library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(writexl)

# Step 1: Read the cleaned_waltzdb_export.xlsx file
waltz_data <- read_excel("cleaned_waltzdb_export.xlsx")

# Step 2: Read and parse the aaindex1.txt file
aaindex_path <- "./aaindex/aaindex1.txt"
aaindex_data <- readLines(aaindex_path)

# Function to parse AAindex file
parse_aaindex <- function(lines) {
  aaindex_list <- list()
  current_index <- NULL
  read_values <- FALSE
  number_of_read_lines <- 0
  
  for (line in lines) {
    if (startsWith(line, "H ")) {
      current_index <- sub("H ", "", line)
      aaindex_list[[current_index]] <- list()
      read_values <- FALSE
      number_of_read_lines <- 0
    } else if (startsWith(line, "I ")) {
      read_values <- TRUE
    } else if (read_values && number_of_read_lines < 2) {
      number_of_read_lines <- number_of_read_lines + 1
      values <- as.numeric(unlist(strsplit(trimws(line), "\\s+")))
      aaindex_list[[current_index]]$values <- c(aaindex_list[[current_index]]$values, values)
      if (current_index == "YANJ020101") {
        print(current_index)
        print(aaindex_list[[current_index]]$values)
      }
    }
  }
  return(aaindex_list)
}

# Parse the aaindex data
aaindex_list <- parse_aaindex(aaindex_data)

# Step 3: Map each amino acid to the 560-dimensional feature space
amino_acids <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
aa_index <- setNames(1:length(amino_acids), amino_acids)

map_aa_to_features <- function(aa, aaindex_list) {
  if (!aa %in% names(aa_index)) {
    print("Problem mapping feature...")
    return(rep(NA, length(aaindex_list)))
  }
  aa_values <- sapply(aaindex_list, function(index) {
    index$values[aa_index[[aa]]]
  })
  return(aa_values)
}

# Apply the mapping function to each character column
mapped_features <- waltz_data %>%
  rowwise() %>%
  mutate(f = list(
    across(starts_with("Character_"),
           ~ map_aa_to_features(.x, aaindex_list)
    ) %>% unlist()
  )) %>%
  unnest_wider(f, names_sep = "_") %>%
  ungroup() %>%
  select(starts_with("f_"), Classification, starts_with("Character_"))

Mode <- function(x) {
  ux <- unique(x[!is.na(x)])  # Only consider non-NA values for mode calculation
  if (length(ux) == 0) return(NA)  # Return NA if all values are NA
  ux[which.max(tabulate(match(x, ux)))]
}
  # Fill remaining missing values with the mode
  for (col_name in names(mapped_features)) {
    if (any(is.na(mapped_features[[col_name]]))) {
      mode_value <- Mode(mapped_features[[col_name]])
      if (!is.na(mode_value)) {
        mapped_features[[col_name]][is.na(mapped_features[[col_name]])] <- mode_value
      }
    }
  }

# Save the final dataset
na_indices <- which(is.na(mapped_features), arr.ind = TRUE)

# Print the row and column indices of NA values
print(na_indices)
write_xlsx(mapped_features, "indexed_waltzdb_export.xlsx")
print('Finished')
