library(fs)
library(readr)
library(stringr)
library(yaml)
library(dplyr)

# --- Configuration ---
# Define the directory where your .qmd files are located
target_dir <- "cables" 
# List of fields that are likely to contain user-facing text and quotes
yaml_fields_to_clean <- c(
  "title", 
  "cable-subject", 
  "cable-ref",
  "cable-info",
  "cable-concepts",
  "cable-to",
  "author"
)

# --- Processing Function ---

clean_meta_block <- function(file_path) {
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(invisible(NULL))
  }
  
  lines <- read_lines(file_path)
  
  # 1. Identify YAML boundaries (---)
  delimiter_indices <- which(lines == "---")
  
  if (length(delimiter_indices) < 2) {
    warning(paste("Skipping", basename(file_path), "- No valid YAML header found."))
    return(invisible(NULL))
  }
  
  yaml_start <- delimiter_indices[1]
  yaml_end   <- delimiter_indices[2]
  
  # 2. Extract and modify YAML lines
  yaml_lines <- lines[(yaml_start + 1):(yaml_end - 1)]
  
  modified_yaml_lines <- yaml_lines
  
  # Regex to find a field (start of line, word boundary, colon)
  # Then escape double quotes in the value part
  
  for (field in yaml_fields_to_clean) {
    # Pattern looks for: (start of line) (field name) : (space) (optional quote) (value with quotes)
    # We target the specific field line by name
    field_pattern <- paste0("^", field, ":\\s*\"(.*)\"$")
    
    # Use str_replace to search for the line containing the field
    # The replacement escapes internal double quotes: \" -> \\"
    modified_yaml_lines <- str_replace(
      modified_yaml_lines,
      # Look for the line containing the field, capturing the quoted value
      field_pattern,
      function(line) {
        # Extract the content inside the outer quotes
        content_inside_quotes <- str_match(line, field_pattern)[1, 2]
        
        if (is.na(content_inside_quotes)) {
          # If the field exists but is not quoted or doesn't match the pattern, leave as is
          return(line)
        }
        
        # --- DEBUG MESSAGE INSERTED HERE ---
        if (str_detect(content_inside_quotes, "\"")) {
          # Find the line number of the current field for accurate debugging output
          line_number <- which(yaml_lines == line) + yaml_start 
          message(paste("DEBUG: Detected unescaped quote in field '", field, 
                        "' (line ", line_number, ") in file ", basename(file_path), sep=""))
        }
        # --- END DEBUG MESSAGE ---
        
        escaped_content <- str_replace_all(content_inside_quotes, '"', '\\\\"')
        
        if (escaped_content != content_inside_quotes) {
          # No change needed
          message(paste("Escaped quotes in field '", field, "': ",
                        content_inside_quotes, "\nReplaced by: ",
                        escaped_content, sep=""))
        }
        
        # Reconstruct the line: field: "escaped content"
        return(paste0(field, ": \"", escaped_content, "\""))
      }
    )
  }
  
  # 3. Reconstruct file content
  
  # Check if modifications occurred (simple check based on length, usually fine)
  if (length(modified_yaml_lines) != length(yaml_lines)) {
    warning(paste("Length mismatch during YAML processing for:", basename(file_path)))
  }
  
  # Reassemble: YAML start + modified content + YAML end + rest of file
  new_lines <- c(
    lines[1], # First ---
    modified_yaml_lines,
    lines[yaml_end], # Second ---
    lines[(yaml_end + 1):length(lines)] # Rest of the QMD file content
  )
  
  # 4. Write back to file
  write_lines(new_lines, file_path)
  message(paste("Cleaned:", basename(file_path), "for YAML quotes."))
}


clean_meta_block <- function(file_path) {
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(invisible(NULL))
  }
  
  lines <- read_lines(file_path)
  
  # 1. Identify YAML boundaries (---)
  delimiter_indices <- which(lines == "---")
  
  if (length(delimiter_indices) < 2) {
    warning(paste("Skipping", basename(file_path), "- No valid YAML header found."))
    return(invisible(NULL))
  }
  
  yaml_start <- delimiter_indices[1]
  yaml_end   <- delimiter_indices[2]
  
  # 2. Extract YAML lines
  yaml_lines <- lines[(yaml_start + 1):(yaml_end - 1)]
  
  # Initialize the final list of YAML lines
  cleaned_yaml_lines <- yaml_lines
  
  # We iterate over the *lines* and for each line, check if it contains a field we care about.
  for (i in seq_along(yaml_lines)) {
    line <- yaml_lines[i]
    
    # 2a. Identify the field name on the current line
    field_match <- str_match(line, "^([a-z-]+):\\s*\"(.*)\"$")
    
    # Check if the line is a quoted key-value pair and we care about the field
    if (!is.na(field_match[1, 1])) {
      
      field_name <- field_match[1, 2]
      content_inside_quotes <- field_match[1, 3]
      
      # Check if this field needs cleaning AND it contains unescaped quotes
      if (field_name %in% yaml_fields_to_clean && str_detect(content_inside_quotes, "\"")) {
        
        # --- DEBUG MESSAGE ---
        line_number <- i + yaml_start
        message(paste("DEBUG: FIXING unescaped quote in field '", field_name, 
                      "' (line ", line_number, ") in file ", basename(file_path), sep=""))
        # --- END DEBUG MESSAGE ---
        
        # --- FIX: Use stringr's safer replacement style ---
        # We need to replace all internal double quotes (") with an escaped double quote (\")
        escaped_content <- str_replace_all(content_inside_quotes, '"', '\\\\"')
        
        # Reconstruct the line: field: "escaped content"
        cleaned_yaml_lines[i] <- paste0(field_name, ": \"", escaped_content, "\"")
      }
    } else {
      # Line is not a quoted key-value pair (e.g., date: "May 23, 1974" is caught, 
      # but 'editor: visual' is ignored, which is fine, or it's a list item).
      # We only focus on the double-quoted string values as requested by the error type.
    }
  }
  
  # 3. Reconstruct file content
  
  # Reassemble: YAML start + modified content + YAML end + rest of file
  new_lines <- c(
    lines[1], # First ---
    cleaned_yaml_lines,
    lines[yaml_end], # Second ---
    lines[(yaml_end + 1):length(lines)] # Rest of the QMD file content
  )
  
  # 4. Write back to file
  write_lines(new_lines, file_path)
  message(paste("Cleaned:", basename(file_path), "for YAML quotes."))
}

# --- Example of Batch Processing (No output files generated here) ---

files_to_clean <- dir_ls(target_dir, glob = "*.qmd")
walk(files_to_clean, clean_meta_block)
