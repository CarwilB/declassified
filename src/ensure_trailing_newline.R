# Ensure trailing newline for all files in a directory
#
# ensure_trailing_newline(dir = ".", pattern = ".*", recursive = FALSE,
#                        dry_run = TRUE, verbose = TRUE, skip_binary = TRUE)
#
# - dir: directory to scan
# - pattern: regex (or glob like "*.qmd") to match filenames
# - recursive: recurse into subdirectories
# - dry_run: when TRUE, don't modify files; only report what would be done
# - verbose: print progress/messages
# - skip_binary: skip files that look binary (contain NUL bytes in the first chunk)
#
# Returns a data.frame/tibble with columns: file, status, message.
#
# Example:
# ensure_trailing_newline("cables", pattern = "\\.qmd$", recursive = TRUE,
#                        dry_run = FALSE, verbose = TRUE)
#
ensure_trailing_newline <- function(dir = ".",
                                    pattern = ".*",
                                    recursive = FALSE,
                                    dry_run = TRUE,
                                    verbose = TRUE,
                                    skip_binary = TRUE,
                                    sample_bytes = 4096L) {
  stopifnot(is.character(dir), length(dir) == 1)
  stopifnot(is.character(pattern), length(pattern) == 1)
  if (grepl("[*?\\[]", pattern)) {
    # treat as glob if it contains glob characters
    pattern <- utils::glob2rx(pattern)
  }
  files <- list.files(path = dir, pattern = pattern,
                      recursive = recursive, full.names = TRUE, all.files = FALSE)
  if (length(files) == 0L) {
    if (verbose) message("No files matched.")
    return(if (requireNamespace("tibble", quietly = TRUE)) tibble::tibble(file = character(0), status = character(0), message = character(0)) else data.frame(file = character(0), status = character(0), message = character(0), stringsAsFactors = FALSE))
  }
  results <- vector("list", length(files))
  for (i in seq_along(files)) {
    f <- files[[i]]
    status <- "unknown"
    msg <- ""
    # skip directories
    info <- file.info(f)
    if (is.na(info$size)) {
      status <- "missing"
      msg <- "file does not exist"
      if (verbose) message(f, ": ", msg)
      results[[i]] <- list(file = f, status = status, message = msg)
      next
    }
    if (isTRUE(info$isdir)) {
      status <- "skipped"
      msg <- "is directory"
      if (verbose) message(f, ": ", msg)
      results[[i]] <- list(file = f, status = status, message = msg)
      next
    }
    # skip binary if requested: sample the first chunk for NUL bytes
    if (skip_binary) {
      con_s <- file(f, "rb")
      on.exit(close(con_s), add = TRUE)
      n <- min(sample_bytes, info$size)
      if (n > 0) {
        raw_sample <- readBin(con_s, "raw", n = n)
        if (any(raw_sample == as.raw(0))) {
          status <- "skipped"
          msg <- "binary-like (NUL byte) - skipped"
          if (verbose) message(f, ": ", msg)
          results[[i]] <- list(file = f, status = status, message = msg)
          close(con_s)
          next
        }
      }
      close(con_s)
    }
    # check writability
    if (file.access(f, 2) != 0) {
      status <- "error"
      msg <- "not writable"
      if (verbose) message(f, ": ", msg)
      results[[i]] <- list(file = f, status = status, message = msg)
      next
    }
    # determine last byte (if any)
    fs <- info$size
    last_byte <- NULL
    if (fs == 0) {
      last_byte <- NULL
    } else {
      con <- file(f, "rb")
      on.exit(close(con), add = TRUE)
      # seek to last byte
      seek(con, where = fs - 1, origin = "start")
      lb <- readBin(con, "raw", n = 1)
      last_byte <- if (length(lb) == 1) lb else NULL
      close(con)
    }
    # newline is LF (0x0A). If last_byte is 0x0A we are done.
    need_append <- FALSE
    if (is.null(last_byte)) {
      # empty file -> append newline
      need_append <- TRUE
      reason <- "empty file"
    } else if (identical(as.integer(last_byte), 10L)) {
      need_append <- FALSE
      reason <- "already ends with LF"
    } else if (identical(as.integer(last_byte), 13L)) {
      # ends with CR only; append LF to produce CRLF
      need_append <- TRUE
      reason <- "ends with CR only (will append LF)"
    } else {
      need_append <- TRUE
      reason <- "does not end with newline"
    }
    if (!need_append) {
      status <- "ok"
      msg <- reason
      if (verbose) message(f, ": ", msg)
      results[[i]] <- list(file = f, status = status, message = msg)
      next
    }
    # perform append (or dry_run)
    if (dry_run) {
      status <- "would_append"
      msg <- reason
      if (verbose) message(f, ": would append newline (dry_run=TRUE) -- ", reason)
    } else {
      # append LF byte
      # open append in binary mode
      con_a <- file(f, "ab")
      on.exit(close(con_a), add = TRUE)
      tryCatch({
        writeBin(charToRaw("\n"), con_a)
        close(con_a)
        status <- "appended"
        msg <- reason
        if (verbose) message(f, ": appended newline -- ", reason)
      }, error = function(e) {
        status <<- "error"
        msg <<- paste("write error:", conditionMessage(e))
        if (verbose) message(f, ": error appending newline - ", msg)
        try(close(con_a), silent = TRUE)
      })
    }
    results[[i]] <- list(file = f, status = status, message = msg)
  }
  df <- do.call(rbind, lapply(results, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  # prefer tibble if available
  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(df))
  } else {
    return(df)
  }
}
