meta_data <- process_directory("data/cables-usa", "cables", use_llm = TRUE)
meta_data_new <- process_directory("data/cables-unprocessed", "cables", use_llm = TRUE,
                                   overwrite = TRUE)
