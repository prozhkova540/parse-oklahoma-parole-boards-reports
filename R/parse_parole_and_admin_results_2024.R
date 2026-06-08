# Extract entries reliably by splitting on \n\n followed by name pattern
extract_entries <- function(text) {
  text |>
    str_split("\\n\\n(?=[A-Z][A-Z.,' -]+)") |>
    unlist() |>
    trimws()
  }

# DOC number should have at least 5 digits
extract_doc_number <- function(entry) {
  str_extract(entry, "#\\s*\\d{5,}")
}

# Extract decision phrases
extract_decision <- function(entry) {
  possible_board_decisions <- c("DENIED AT BOARD MEETING",
                                "COMMUTE SENTENCE",
                                "COMMUTE TO TIME SERVED",
                                "CONSECUTIVE SENTENCE",
                                "DET OR VOID",
                                "DET OR STREET",
                                "STREET",
                                "PASSED TO ANOTHER DOCKET",
                                "STRICKEN",
                                "DISCHARGED",
                                "PAROLE TO STREET")
  pattern <- paste0("(", paste(
    possible_board_decisions, collapse = "|"),")(?:\\s+\\([0-9]+\\))?")

  # Extract the decision
  str_extract(entry, pattern)
}


# Extract all-caps name
extract_name <- function(entry) {
  # Define decision words to exclude from name
  decision_words <- c(
    "DENIED", "COMMUTE", "CONSECUTIVE", "DET", "STREET",
    "PASSED", "STRICKEN", "DISCHARGED", "PAROLE", "VOID"
  )

  # Pattern to stop at decision words
  stop_pattern <- paste0("(?=\\s+(",
                         paste(decision_words, collapse = "|"), "))")

  # Extract name until a decision word is encountered
  name_pattern <- paste0("^[A-Z][A-Z.,' -]+?", stop_pattern)

  name <- str_extract(entry, name_pattern) |>
    str_trim()

}

# Main parser with safe stopping and validation
#TODO: finish parsing full text for PRE-PAROLE STIPULATIONS (1)
# and SPECIAL PAROLE CONDITIONS (2)
parse_parole_results <- function(text) {
  entries <- extract_entries(text)

  output <- data.frame(
    full_text = entries,
    name = map_chr(entries, extract_name),
    doc_num_raw = map_chr(entries, extract_doc_number),
    decision = map_chr(entries, extract_decision),
    # TODO: add parole stipulation or conditions
    # TODO: add board decision notes
    stringsAsFactors = FALSE
  )

  # only keep rows relevant info eliminates headers
  output <- output[!is.na(output$name), ]

  return(output)
}

process_all_result_pdfs <- function(base_dir) {
  # Get all PDF files
  pdf_files <- list.files(base_dir, pattern = "*.pdf", full.names = TRUE)

  # Initialize empty dataframe for results
  all_results <- NULL

  # Process each file
  for (file_path in pdf_files) {
    # Get month from filename
    month <- str_extract(basename(file_path), "[A-Za-z]+")

    # Read and process PDF
    pdf_content <- pdf_text(file_path)

    processed_data <- pdf_content |>
      parse_parole_results() |>
      mutate(month = month)

    # Add to results
    all_results <- bind_rows(all_results, processed_data) |>
      as_tibble()
  }

  all_results |>
    select(month, name, doc_num_raw, decision, everything())
}
