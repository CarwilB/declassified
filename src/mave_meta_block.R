library(fs)
library(readr)
library(stringr)
library(dplyr)

# --- Configuration ---
target_dir <- "output_qmd" # Adjust to your folder name

# --- Processing Function ---
move_meta_block <- function(file_path) {
  lines <- read_lines(file_path)
  
  # 1. Locate the cable-meta block
  meta_start <- which(str_detect(lines, "^:::\\s*\\{?\\.?cable-meta\\}?"))[1]
  
  if (is.na(meta_start)) {
    warning(paste("Skipping", basename(file_path), "- No cable-meta block."))
    return()
  }
  
  all_closers <- which(str_detect(lines, "^:::$"))
  meta_end <- all_closers[all_closers > meta_start][1]
  
  if (is.na(meta_end)) return()
  
  # 2. Locate the Target Insertion Point
  # Look for "# DEPARTMENT OF STATE TELEGRAM"
  dept_header_idx <- which(str_detect(lines, "^# DEPARTMENT OF STATE TELEGRAM"))[1]
  
  if (is.na(dept_header_idx)) {
    warning(paste("Skipping", basename(file_path), "- 'DEPARTMENT OF STATE TELEGRAM' header not found."))
    return()
  }
  
  # Check if block is ALREADY below the header (skip if so)
  if (meta_start > dept_header_idx) {
    message(paste("Skipping", basename(file_path), "- Block already moved (or out of order)."))
    return()
  }
  
  # Look for the Classification line (e.g., **UNCLASSIFIED**) AFTER the Dept header
  # We search lines starting from dept_header_idx
  # Pattern: line starts and ends with **
  potential_class_indices <- which(str_detect(lines, "^\\*\\*.*\\*\\*$"))
  class_idx <- potential_class_indices[potential_class_indices > dept_header_idx][1]
  
  if (is.na(class_idx)) {
    warning(paste("Skipping", basename(file_path), "- Classification line (**...**) not found after header."))
    return()
  }
  
  # 3. Reconstruct the file
  #   Part A: Top of file (YAML) up to just before meta block
  #   Part B: Content between meta block and the Classification line (Header + Class)
  #   Part C: The Meta Block
  #   Part D: The rest of the file
  
  # Careful with indices.
  # Content BEFORE meta: lines 1 to (meta_start - 1)
  part_a <- lines[1:(meta_start - 1)]
  
  # Content BETWEEN meta and insertion point: lines (meta_end + 1) to class_idx
  part_b <- lines[(meta_end + 1):class_idx]
  
  # The Meta Block itself
  part_c <- lines[meta_start:meta_end]
  
  # Content AFTER insertion point
  part_d <- if ((class_idx + 1) <= length(lines)) lines[(class_idx + 1):length(lines)] else character(0)
  
  # Assemble with clean spacing
  # We ensure there's a blank line before the meta block if one doesn't exist in Part C logic
  new_content <- c(
    part_a,
    part_b,
    "",             # Spacer before meta block
    part_c,
    "",             # Spacer after meta block
    part_d
  )
  
  # Write back
  write_lines(new_content, file_path)
  message(paste("Processed:", basename(file_path)))
}

# --- Main Execution ---
files <- dir_ls(target_dir, glob = "*.qmd")
files <- files[tolower(basename(files)) != "index.qmd"]

if (length(files) == 0) stop("No files found.")

message(paste("Processing", length(files), "files..."))
for (f in files) {
  move_meta_block(f)
}
message("Done.")