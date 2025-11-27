library(pdftools)
library(stringr)
library(dplyr)
library(readr)
library(tibble)
library(fs)

# Function to extract metadata and body from a single PDF
process_cable_pdf <- function(pdf_path) {
  
  # Read text from PDF
  raw_pages <- pdf_text(pdf_path)
  full_text <- paste(raw_pages, collapse = "\n")
  
  # --- Helper to safely extract regex matches ---
  extract_field <- function(text, pattern) {
    match <- str_match(text, pattern)
    if (nrow(match) > 0 && ncol(match) > 1) {
      clean_val <- str_replace_all(match[1, 2], "\\s+", " ")
      return(str_trim(clean_val))
    }
    return(NA_character_)
  }
  
  # --- 1. Extract Header Metadata (for YAML) ---
  cable_date <- extract_field(full_text, "Draft Date:\\s*([^\n]+)")
  if (is.na(cable_date)) {
    cable_date <- extract_field(full_text, "Date:\\s*([^\n]+)")
  }
  
  cable_from <- extract_field(full_text, "(?s)FM\\s+(.*?)(?=\nTO)")
  cable_to   <- extract_field(full_text, "(?s)\nTO\\s+(.*?)(?=\nINFO|\n[A-Z]+)")
  cable_info <- extract_field(full_text, "(?s)\nINFO\\s+(.*?)(?=\n[A-Z]+ [A-Z]+|\nE\\.O\\.)")
  cable_subject <- extract_field(full_text, "(?s)SUBJECT:\\s+(.*?)(?=\n\\s*1\\.|REF:)")
  cable_tags <- extract_field(full_text, "TAGS:\\s*([^\n]+)")
  cable_ref  <- extract_field(full_text, "(?s)REF:\\s+(.*?)(?=\n\\s*1\\.)")
  
  # --- 2. Extract Message Attributes (Footer Metadata for Tibble) ---
  doc_number     <- extract_field(full_text, "Document Number:\\s*([^\n]+)")
  classification <- extract_field(full_text, "Current Classification:\\s*([^\n]+)")
  concepts       <- extract_field(full_text, "Concepts:\\s*([^\n]+)")
  
  declass_date   <- extract_field(full_text, "Declassified/Released US Department of State.*?(\\d{1,2}\\s+[A-z]{3}\\s+\\d{4})")
  if(is.na(declass_date)) {
    declass_date <- extract_field(full_text, "Decaption Date:\\s*([^\n]+)")
  }
  
  # --- 3. Extract Body Text ---
  # Strategy: Locate start (Subject/Ref) and strictly cut off before Message Attributes
  
  body_start_pattern <- "(?s)SUBJECT:.*?(?=\n\\s*1\\.)"
  body_start_pos <- str_locate(full_text, body_start_pattern)[2]
  
  # Look for the start of the Metadata block to define the end of the body
  # Use regex to find "Message Attributes" or the specific "NNN" end-of-message marker
  body_end_pos <- str_locate(full_text, regex("Message Attributes|NNN", ignore_case = TRUE))[1]
  
  if (!is.na(body_start_pos)) {
    # If we found an end position, cut there. If not, go to end of text (fallback)
    end_index <- ifelse(!is.na(body_end_pos), body_end_pos - 1, nchar(full_text))
    cable_body <- str_sub(full_text, body_start_pos + 1, end_index)
  } else {
    # Fallback if subject line parsing fails
    cable_body <- full_text 
  }
  
  # Post-processing cleanup on the body
  # 1. Remove standard declassification headers repeated on pages
  cable_body <- str_remove_all(cable_body, "Sheryl P\\. Walter Declassified/Released.*")
  # 2. Remove classification markings embedded in text
  cable_body <- str_remove_all(cable_body, "CONFIDENTIAL|LIMITED OFFICIAL USE|UNCLASSIFIED")
  # 3. Remove page number artifacts
  cable_body <- str_remove_all(cable_body, "PAGE \\d+ [A-Z ]+ \\d+")
  # 4. Remove "Message Attributes" if it snuck in due to regex overlap
  cable_body <- str_remove(cable_body, "Message Attributes.*")
  
  cable_body <- str_trim(cable_body)
  
  # --- 4. Construct Markdown Content ---
  
  # Helper for cleaner NULL/NA handling in glue strings
  replace_na <- function(x) {
    ifelse(is.na(x), "", x)
  }
  
  # Generates the clean markdown file WITH the custom footer attributes block
  yaml_header <- glue::glue("---
title: \"{ifelse(!is.na(cable_subject), cable_subject, 'Unknown Subject')}\"
editor: visual
cable-date: \"{replace_na(cable_date)}\"
cable-from: \"{replace_na(cable_from)}\"
cable-to: \"{replace_na(cable_to)}\"
cable-info: \"{replace_na(cable_info)}\"
cable-subject: \"{replace_na(cable_subject)}\"
cable-tags: \"{replace_na(cable_tags)}\"
cable-ref: \"{replace_na(cable_ref)}\"
---

::: {{.cable-meta}}
:::

# DEPARTMENT OF STATE TELEGRAM
**{replace_na(classification)}**

{cable_body}

***

#### Message Attributes

- **Document Number:** {replace_na(doc_number)}

- **Date:** {replace_na(cable_date)}

- **Classification:** {replace_na(classification)}

- **Declassified:** {replace_na(declass_date)}

- **Concepts:** {replace_na(concepts)}
")
  
  # Return a list with the data row and the file content
  list(
    filename = basename(pdf_path),
    markdown = yaml_header,
    data_row = tibble(
      source_file = basename(pdf_path),
      cable_date = cable_date,
      cable_from = cable_from,
      cable_to = cable_to,
      cable_subject = cable_subject,
      cable_tags = cable_tags,
      cable_ref = cable_ref,
      doc_number = doc_number,
      classification = classification,
      declass_date = declass_date,
      concepts = concepts
    )
  )
}

# --- Main Execution Function ---

process_directory <- function(dir_path, output_dir) {
  
  # Get all PDF files
  pdf_files <- list.files(dir_path, pattern = "\\.pdf$", full.names = TRUE)
  
  if (length(pdf_files) == 0) {
    stop("No PDF files found in the specified directory.")
  }
  
  all_metadata <- list()
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  for (file in pdf_files) {
    message(paste("Processing:", basename(file)))
    
    tryCatch({
      result <- process_cable_pdf(file)
      
      # Write the Markdown file
      # Change extension from .pdf to .qmd
      out_name <- str_replace(result$filename, "\\.pdf$", ".qmd")
      write_file(result$markdown, file.path(output_dir, out_name))
      
      # Append metadata to list
      all_metadata[[length(all_metadata) + 1]] <- result$data_row
      
    }, error = function(e) {
      warning(paste("Failed to process", basename(file), ":", e$message))
    })
  }
  
  # Combine all metadata into one tibble
  final_tibble <- bind_rows(all_metadata)
  
  return(final_tibble)
}

# --- Usage Example ---
# To run:
# meta_data <- process_directory("pdf_input_folder", "qmd_output_folder")
# Run the script
# Change "pdf_input_folder" to your actual folder path
# Change "qmd_output_folder" to where you want the qmd files
meta_data <- process_directory("data/cables-usa", "data/cables-usa-md")
write_rds(meta_data, "data/cable_metadata.rds")
write_csv(meta_data, "data/cable_metadata.csv")
# View the resulting tibble
# print(meta_data)