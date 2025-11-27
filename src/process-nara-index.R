# Install required packages if not already installed
if (!require("rvest")) install.packages("rvest")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("readr")) install.packages("readr")

library(rvest)
library(dplyr)
library(stringr)
library(readr)

# Define the base directory where the HTML files are located
base_dir <- "data/index-nara" 

# List of HTML files (only file names)
file_names_1979 <- c(
  "NARA - AAD - Display Partial Records - Electronic Telegrams, 1979 p1.html",
  "NARA - AAD - Display Partial Records - Electronic Telegrams, 1979 p2.html",
  "NARA - AAD - Display Partial Records - Electronic Telegrams, 1979 p3.html",
  "NARA - AAD - Display Partial Records - Electronic Telegrams, 1979notBoeker.html",
  "NARA - AAD - Electronic Telegrams, 1978 - Natusch.html"
)
file_names_1974 <- c(
  "NARA - AAD - Display Partial Records - Electronic Telegrams, 1974 p3.html",
  "NARA - AAD - Display Partial Records - Electronic Telegrams, 1974 p4.html",
  "NARA - AAD - Display Partial Records - Electronic Telegrams, 1974 p5.html",
  "NARA - AAD - Electronic Telegrams, 1974 p1.html",
  "NARA - AAD - Electronic Telegrams, 1974 p2.html"
)
file_names_map <- c(
  "NARA - AAD - Military Assistance Program 1000 System Master File, FY 1986, ca. 10_1_1950 - 9_30_1986 p2.html",
  "NARA - AAD - Military Assistance Program 1000 System Master File, FY 1986, ca. 10_1_1950 - 9_30_1986.html"
)

file_names <- file_names_1979
file_names <- c(file_names_1974, file_names_1979, file_names_map)

# Construct the full file paths by combining the base directory and file names
file_list <- file.path(base_dir, file_names)


process_aad_html <- function(file_path) {
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  # Read HTML content
  page <- read_html(file_path)
  
  # Extract the table
  table_node <- page %>% html_node("table#queryResults")
  
  if (length(table_node) == 0) {
    warning(paste("No table found in", file_path))
    return(NULL)
  }
  
  # Convert table to dataframe
  # fill=TRUE handles rowspans automatically by filling down
  df <- table_node %>% html_table(fill = TRUE)
  
  # Extract the URLs from the first column ("View Record")
  # We need to look at the 'a' tags inside the first 'td' of each row in tbody
  url_nodes <- table_node %>% 
    html_nodes("tbody tr td:first-child a")
  
  urls <- url_nodes %>% html_attr("href")
  
  # Fix relative URLs
  urls <- sapply(urls, function(u) {
    if (!is.na(u) && !startsWith(u, "http")) {
      return(paste0("https://aad.archives.gov/aad/", u))
    }
    return(u)
  })
  
  # Identify detail rows vs main rows
  # html_table with fill=TRUE repeats data for rowspanned cells.
  # However, detail rows in AAD usually appear as a separate row in HTML source 
  # often with a colspan. `html_table` might attempt to parse them into the first column 
  # or spread them out.
  # A more robust way with rvest for this specific structure:
  
  # Get all rows
  rows <- table_node %>% html_nodes("tbody tr")
  
  cleaned_data <- list()
  current_record <- NULL
  url_index <- 1
  
  for (i in seq_along(rows)) {
    row <- rows[[i]]
    cells <- html_nodes(row, "td")
    
    # Check if this is a detail row (e.g., colspan attribute exists)
    colspan <- html_attr(cells, "colspan")
    is_detail <- any(!is.na(colspan))
    
    if (is_detail) {
      # Extract text
      detail_text <- html_text(row, trim = TRUE)
      # Append to the last record's TAGS or create a note field
      if (!is.null(current_record)) {
        # Assuming TAGS is the 7th column (index 7) based on standard AAD layout
        # Headers: View Record, Draft Date, Doc Num, Film Num, From, Subject, TAGS, To, Msg Text
        # Index:   1            2           3        4         5     6        7     8   9
        current_record[7] <- paste(current_record[7], "|", detail_text)
      }
    } else {
      # This is a main row
      # Save previous record if exists
      if (!is.null(current_record)) {
        cleaned_data[[length(cleaned_data) + 1]] <- current_record
      }
      
      # Extract text from cells
      row_text <- html_text(cells, trim = TRUE)
      
      # Inject URL into the first column
      if (url_index <= length(urls)) {
        row_text[1] <- urls[url_index]
        url_index <- url_index + 1
      } else {
        row_text[1] <- "" 
      }
      
      current_record <- row_text
    }
  }
  # Add the last record
  if (!is.null(current_record)) {
    cleaned_data[[length(cleaned_data) + 1]] <- current_record
  }
  
  # Convert list to dataframe
  # Get headers from thead
  headers <- table_node %>% 
    html_node("thead tr") %>% 
    html_nodes("th") %>% 
    html_text(trim = TRUE)
  
  # Rename first header
  if (length(headers) > 0 && headers[1] == "View Record") {
    headers[1] <- "Record URL"
  }
  
  final_df <- do.call(rbind, cleaned_data) %>% as.data.frame()
  
  # Ensure column count matches headers
  if (ncol(final_df) == length(headers)) {
    colnames(final_df) <- headers
  }
  o
  return(final_df)
}

# Process all files and combine
all_records <- lapply(file_list, process_aad_html) %>% bind_rows()

# Export to CSV
write_csv(all_records, "data/aad_records_compiled_combined.csv")
write_rds(all_records, "data/aad_records_compiled_combined.rds")



print(paste("Successfully processed", nrow(all_records), "records."))
print(head(all_records))