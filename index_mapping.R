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
  
  for (line in lines) {
    if (startsWith(line, "H ")) {
      current_index <- sub("H ", "", line)
      aaindex_list[[current_index]] <- list()
      read_values <- FALSE
    } else if (startsWith(line, "I ")) {
      read_values <- TRUE
    } else if (read_values && grepl("^[ \\t]*[0-9.-]", line)) {
      values <- as.numeric(unlist(strsplit(trimws(line), "\\s+")))
      aaindex_list[[current_index]]$values <- c(aaindex_list[[current_index]]$values, values)
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
  select(starts_with("f_"), Classification)

# Save the final dataset
write_xlsx(mapped_features, "indexed_waltzdb_export.xlsx")
print('Finished')
