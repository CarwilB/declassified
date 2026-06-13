library(yaml)
library(dplyr)
library(purrr)
library(fs)
library(tibble)
library(stringr)

# --- Configuration ---
# Set the directory containing the .qmd files
# Change this to "." if the script is running inside the folder, 
# or "output_qmd" if running from the project root.
target_dir <- "cables" 
output_file <- file.path(target_dir, "metadata_index.rds")

# --- Helper Function ---
extract_yaml_from_file <- function(file_path) {
  
  # Read the file line by line
  lines <- readLines(file_path, warn = FALSE)
  
  # Find the indices of the YAML delimiters (---)
  # We look for lines that are exactly "---"
  delimiter_indices <- which(lines == "---")
  
  # Validation: Must have at least two "---" lines (start and end of header)
  if (length(delimiter_indices) < 2) {
    warning(paste("No valid YAML header found in:", basename(file_path)))
    return(NULL)
  }
  
  # Extract the text between the first and second delimiter
  # We assume the header starts on line 1, but this logic finds the first block regardless
  start_idx <- delimiter_indices[1] + 1
  end_idx   <- delimiter_indices[2] - 1
  
  # Guard against empty headers
  if (start_idx > end_idx) {
    return(NULL)
  }
  
  yaml_content <- lines[start_idx:end_idx]
  
  # Parse YAML into a named list
  # yaml.load handles type conversion (booleans, numbers) automatically
  meta_list <- tryCatch({
    yaml::yaml.load(paste(yaml_content, collapse = "\n"))
  }, error = function(e) {
    warning(paste("YAML parsing failed for:", basename(file_path), "-", e$message))
    return(NULL)
  })
  
  # Convert list to a one-row tibble
  # We wrap in a list and use map to handle potential NULLs or nested lists gracefully
  meta_tibble <- as_tibble(lapply(meta_list, function(x) {
    if (is.null(x)) return(NA)
    if (length(x) > 1) return(list(x)) # Keep lists (like tags) as list-columns
    return(x)
  }))
  
  # Add the source filename as the first column
  meta_tibble <- meta_tibble %>%
    mutate(source_file = basename(file_path)) %>%
    select(source_file, everything())
  
  return(meta_tibble)
}

process_directory_metadata <- function(target_dir = ".", 
                                       pattern = "*.qmd",
                                       .excludeindex = TRUE) {
  
  output_file <- file.path(target_dir, "metadata_index.rds")
  
  # 1. List all .qmd files
  all_files <- dir_ls(target_dir, glob = pattern)
  
  if (length(all_files) == 0) {
    stop(paste("No .qmd files found in directory:", target_dir))
  }
  
  # 2. Filter out index.qmd if requested
  if (.excludeindex) {
    files_to_process <- all_files[tolower(basename(all_files)) != "index.qmd"]
  } else {
    files_to_process <- all_files
  }
  
  if (length(files_to_process) == 0) {
    warning("No files to process after excluding index.qmd.")
    return(NULL)
  }
  
  message(paste("Extracting metadata from", length(files_to_process), "files in", target_dir, "..."))
  
  # 3. Map over files and bind into a single dataframe
  metadata_index <- map_dfr(files_to_process, extract_yaml_from_file)
  
  # 4. Save to RDS
  saveRDS(metadata_index, output_file)
  
  message(paste("Success! Metadata saved to:", output_file))
  return(metadata_index)
}

# metadata_index <- process_directory_metadata(target_dir = "cables")
