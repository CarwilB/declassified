library(pdftools)
library(stringr)
library(dplyr)
library(readr)
library(tibble)
library(fs)
library(httr2)
library(jsonlite)
library(lubridate)

# --- 1. Configuration ---
api_key <- Sys.getenv("GEMINI_API_KEY") 

# --- 2. Formatting Helpers ---

format_cable_date <- function(date_str) {
  if (is.na(date_str)) return(NA_character_)
  clean_date <- str_trim(date_str)
  # Parse various date formats
  parsed_date <- parse_date_time(clean_date, orders = c("d b y", "d B Y", "Ymd", "dmy", "d m y"), quiet = TRUE)
  
  if (!is.na(parsed_date)) {
    if (year(parsed_date) < 100) year(parsed_date) <- year(parsed_date) + 1900
    return(format(parsed_date, "%B %d, %Y"))
  }
  return(date_str)
}

format_diplomatic_text <- function(text) {
  if (is.na(text)) return(NA_character_)
  
  # Convert to Title Case first
  text <- str_to_title(text)
  
  # Apply specific replacements
  text <- text %>%
    str_replace_all("\\bAmembassy\\b", "AmEmbassy") %>%
    str_replace_all("\\bSecstate\\b", "SecState") %>%
    str_replace_all("\\bWashdc\\b", "WashDC") %>%
    str_replace_all("\\bUs\\b", "US") %>%
    str_replace_all("\\bUsa\\b", "USA") %>%
    str_replace_all("\\bSec([a-z]+)\\b", function(match) {
      paste0("Sec", str_to_title(substr(match, 4, nchar(match))))
    })
  
  return(text)
}

# --- 3. LLM Processing Function ---
clean_text_with_llm <- function(raw_text, model = "gemini-2.5-flash") {
  if (nchar(raw_text) < 50 || api_key == "") return(raw_text)
  
  prompt <- paste(
    "You are an expert archivist cleaning OCR text of diplomatic cables.",
    "I will provide raw text. Your task is to output CLEAN MARKDOWN based on it.",
    "Strict Rules:",
    "1. Fix ALL CAPS text to Sentence case for readability.",
    "2. Keep proper nouns (names, countries, acronyms like NATO, US) capitalized.",
    "3. Format numbered paragraphs with Markdown (e.g. '1. **Summary:** Text...').",
    "4. Merge paragraphs broken across lines/pages.",
    "5. DO NOT include the header metadata (Date, From, To) or footer metadata tables.",
    "6. Return ONLY the cleaned body text.",
    "\n\nRaw Text:\n",
    raw_text
  )
  
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent?key=", api_key)
  body <- list(contents = list(list(parts = list(list(text = prompt)))))
  
  tryCatch({
    resp <- request(url) %>%
      req_headers("Content-Type" = "application/json") %>%
      req_body_json(body) %>%
      req_perform()
    result <- resp %>% resp_body_json()
    return(result$candidates[[1]]$content$parts[[1]]$text)
  }, error = function(e) {
    warning("LLM request failed: ", e$message)
    return(raw_text)
  })
}

# --- 4. PDF Extraction Function ---
process_cable_pdf <- function(pdf_path, use_llm = FALSE) {
  
  # Read text
  raw_pages <- pdf_text(pdf_path)
  full_text <- paste(raw_pages, collapse = "\n")
  
  # Generic cleaner for declassification stamps
  full_text <- str_remove_all(full_text, "(?i)(Declassified/Released|EO Systematic Review).*")
  
  # --- Helper Regex ---
  extract <- function(pattern) {
    match <- str_match(full_text, pattern)
    if (nrow(match) > 0 && ncol(match) > 1) {
      clean <- str_replace_all(match[1, 2], "\\s+", " ")
      return(str_trim(clean))
    }
    return(NA_character_)
  }
  
  # --- Extract Raw Metadata ---
  # Header Fields
  raw_date <- extract("Draft Date:\\s*([^\n]+)")
  if (is.na(raw_date)) raw_date <- extract("Date:\\s*([^\n]+)")
  
  raw_from <- extract("(?s)FM\\s+(.*?)(?=\nTO)")
  raw_to   <- extract("(?s)\nTO\\s+(.*?)(?=\nINFO|\n[A-Z]+)")
  raw_info <- extract("(?s)\nINFO\\s+(.*?)(?=\n[A-Z]+ [A-Z]+|\nE\\.O\\.)")
  
  # UPDATED REGEX: Handles SUBJ/SUBJECT and stops at REFS/SUMMARY/1.
  raw_subj <- extract("(?s)(?:SUBJECT|SUBJ):\\s+(.*?)(?=\\n\\s*(?:REFS?|SUMMARY|[0-9]+\\.):?)")
  
  # UPDATED REGEX: Handles REFS/REF and stops at SUMMARY/1.
  raw_tags <- extract("TAGS:\\s*([^\n]+)")
  raw_ref  <- extract("(?s)(?:REFS?):\\s+(.*?)(?=\\n\\s*(?:SUMMARY|[0-9]+\\.):?)")
  
  # Footer Fields (Message Attributes)
  doc_num     <- extract("Document Number:\\s*([^\n]+)")
  classif     <- extract("Current Classification:\\s*([^\n]+)")
  concepts    <- extract("Concepts:\\s*([^\n]+)")
  declass_dt  <- extract("Declassified/Released US Department of State.*?(\\d{1,2}\\s+[A-z]{3}\\s+\\d{4})")
  if(is.na(declass_dt)) declass_dt <- extract("Decaption Date:\\s*([^\n]+)")
  
  # Attempt to find Author (Signer)
  author_match <- str_match(full_text, "([A-Z]{3,})\\s+\n+(NNN|Message Attributes)")
  raw_author <- if (!is.na(author_match[1,2])) author_match[1,2] else NA
  
  # --- Apply Formatting ---
  formatted_date <- format_cable_date(raw_date)
  formatted_from <- format_diplomatic_text(raw_from)
  formatted_to   <- format_diplomatic_text(raw_to)
  formatted_info <- format_diplomatic_text(raw_info)
  
  title_case_subject <- str_to_title(raw_subj)
  
  author_str <- if (!is.na(raw_author)) {
    if (raw_author == "BOEKER") "Paul Boeker, US Ambassador to Bolivia"
    else if (raw_author == "VANCE") "Cyrus Vance, Secretary of State"
    else str_to_title(raw_author)
  } else {
    "Unknown"
  }
  
  # --- Extract Body for LLM ---
  # Smart body extractor: Starts AFTER the metadata block ends.
  # This preserves SUMMARY blocks which often appear before paragraph 1.
  
  # 1. Find end of Subject block
  subj_end <- str_locate(full_text, regex(paste0(fixed(raw_subj), ".*?"), multiline=TRUE))[2]
  if (is.na(subj_end)) subj_end <- 0
  
  # 2. Find end of Ref block (if exists)
  ref_end <- if (!is.na(raw_ref)) {
    str_locate(full_text, regex(paste0(fixed(raw_ref), ".*?"), multiline=TRUE))[2]
  } else 0
  
  # Start body after the last metadata element found
  start_pos <- max(subj_end, ref_end)
  
  # End body at Message Attributes or NNN
  end_pos   <- str_locate(full_text, regex("Message Attributes|NNN", ignore_case=T))[1]
  
  if (start_pos > 0) {
    end_idx <- ifelse(!is.na(end_pos), end_pos - 1, nchar(full_text))
    body_raw <- str_sub(full_text, start_pos + 1, end_idx)
  } else {
    # Fallback if metadata matching failed completely
    body_raw <- full_text
  }
  
  # Clean noise before sending to LLM
  body_raw <- str_remove_all(body_raw, "CONFIDENTIAL|LIMITED OFFICIAL USE|UNCLASSIFIED")
  body_raw <- str_remove_all(body_raw, "PAGE \\d+ [A-Z ]+ \\d+")
  body_raw <- str_remove_all(body_raw, "E\\.O\\..*") # Remove EO lines that might trail
  body_raw <- str_trim(body_raw)
  
  if (use_llm) {
    message("  - Cleaning body text with Gemini...")
    body_clean <- clean_text_with_llm(body_raw)
  } else {
    message("  - Using raw body text (LLM disabled)...")
    body_clean <- body_raw
  }
  
  # --- Construct Markdown ---
  replace_na <- function(x) ifelse(is.na(x), "", x)
  
  yaml_header <- glue::glue("---
title: \"{replace_na(title_case_subject)}\"
author: \"{replace_na(author_str)}\"
date: \"{replace_na(formatted_date)}\"
editor: visual
cable-date: \"{replace_na(formatted_date)}\"
cable-from: \"{replace_na(formatted_from)}\"
cable-to: \"{replace_na(formatted_to)}\"
cable-info: \"{replace_na(formatted_info)}\"
cable-subject: \"{replace_na(title_case_subject)}\"
cable-tags: \"{replace_na(raw_tags)}\"
cable-ref: \"{replace_na(raw_ref)}\"
cable-doc-number: \"{replace_na(doc_num)}\"
cable-classification: \"{replace_na(classif)}\"
cable-declass-date: \"{replace_na(declass_dt)}\"
cable-concepts: \"{replace_na(concepts)}\"
---

::: {{.cable-meta}}
:::

# DEPARTMENT OF STATE TELEGRAM
**{replace_na(classif)}**

{body_clean}

***

#### Message Attributes

- **Document Number:** {replace_na(doc_num)}

- **Date:** {replace_na(formatted_date)}

- **Classification:** {replace_na(classif)}

- **Declassified:** {replace_na(declass_dt)}

- **Concepts:** {replace_na(concepts)}
")
  
  list(
    filename = basename(pdf_path),
    markdown = yaml_header,
    data_row = tibble(
      source_file = basename(pdf_path),
      cable_date = formatted_date,
      cable_from = formatted_from,
      cable_subject = title_case_subject,
      doc_number = doc_num,
      concepts = concepts
    )
  )
}


# --- Main Execution ---
process_directory <- function(dir_path, output_dir, use_llm = FALSE, overwrite = FALSE) {
  pdf_files <- list.files(dir_path, pattern = "\\.pdf$", full.names = TRUE)
  
  if (length(pdf_files) == 0) stop("No PDF files found.")
  if (!dir.exists(output_dir)) dir.create(output_dir)
  
  all_metadata <- list()
  
  for (file in pdf_files) {
    base_name <- basename(file)
    out_name <- str_replace(base_name, "\\.pdf$", ".qmd")
    out_path <- file.path(output_dir, out_name)
    
    # Check for existence if overwrite is FALSE
    if (!overwrite && file.exists(out_path)) {
      message(paste("Skipping (exists):", base_name))
      next
    }
    
    message(paste("Processing:", base_name))
    tryCatch({
      result <- process_cable_pdf(file, use_llm = use_llm)
      write_file(result$markdown, out_path)
      all_metadata[[length(all_metadata) + 1]] <- result$data_row
      
      if (use_llm) {
        Sys.sleep(3) # Rate limit pause only if using LLM
      }
    }, error = function(e) {
      warning(paste("Failed:", base_name, "-", e$message))
    })
  }
  
  if (length(all_metadata) > 0) {
    bind_rows(all_metadata)
  } else {
    tibble()
  }
}
