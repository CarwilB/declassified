library(fs)
library(readr)
library(stringr)
library(dplyr)
library(purrr)

# --- Configuration ---
target_dir <- "cables" # Adjust to your folder name

# --- Helper Function ---
check_file_structure <- function(file_path) {
  lines <- read_lines(file_path)
  
  # 1. Find the metadata block
  # Matches ::: cable-meta OR ::: {.cable-meta}
  meta_start <- which(str_detect(lines, "^:::\\s*\\{?\\.?cable-meta\\}?"))[1]
  
  if (is.na(meta_start)) {
    return(tibble(file = basename(file_path), status = "No cable-meta block found", line_1 = NA, line_2 = NA))
  }
  
  # Find the closing ::: after the start
  # We look for the first ::: that occurs *after* meta_start
  all_closers <- which(str_detect(lines, "^:::$"))
  meta_end <- all_closers[all_closers > meta_start][1]
  
  if (is.na(meta_end)) {
    return(tibble(file = basename(file_path), status = "Unclosed cable-meta block", line_1 = NA, line_2 = NA))
  }
  
  # 2. Extract text immediately following the block
  # We look at all lines after the block
  rest_of_file <- lines[(meta_end + 1):length(lines)]
  
  # Filter out empty lines to find the "text" lines
  text_lines <- rest_of_file[str_trim(rest_of_file) != ""]
  
  # Grab the first two
  line_1 <- if(length(text_lines) >= 1) text_lines[1] else NA
  line_2 <- if(length(text_lines) >= 2) text_lines[2] else NA
  
  return(tibble(
    file = basename(file_path),
    status = "OK",
    line_1 = line_1, 
    line_2 = line_2
  ))
}

# --- Main Execution ---
files <- dir_ls(target_dir, glob = "*.qmd")
# Exclude index.qmd
files <- files[tolower(basename(files)) != "index.qmd"]

if (length(files) == 0) stop("No files found.")

results <- map_dfr(files, check_file_structure)

# --- Report Summary ---
message("--- Pattern Summary ---")
summary_table <- results %>%
  count(line_1, line_2, sort = TRUE)

print(summary_table)

# Optional: List files that DON'T match the standard pattern
# (Adjust the string below to match the majority count from the summary above)
standard_l1 <- "# DEPARTMENT OF STATE TELEGRAM"
# standard_l2 varies (UNCLASSIFIED, CONFIDENTIAL), so we might check if it STARTS with **
oddballs <- results %>%
  filter(line_1 != standard_l1 | !str_detect(line_2, "^\\*\\*"))

if (nrow(oddballs) > 0) {
  message("\n--- Files Deviating from Standard Structure ---")
  print(oddballs)
} else {
  message("\nAll files appear to have consistent header text lines.")
}