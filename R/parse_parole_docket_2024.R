# 1. Preprocessing function - cleans text and adds comma delimiters
preprocess_text <- function(page_text) {
  # Replace multiple spaces with a single space
  cleaned_text <- str_replace_all(page_text, "\\s+", " ")

  # Add commas before key fields to make parsing easier
  key_fields <- c("Docket Month:", "Personal Appearance:", "Type:",
                  "OFFENSE", "Authority:", "Last Board",
                  "Jail Time:", "Reception Date:", "Projected Release Date:",
                  "Next Board Consideration:", "Sentence:",
                  "Concurrent Cases:", "Consecutive Cases:", "Detainer:")

  for (field in key_fields) {
    cleaned_text <- str_replace_all(
      cleaned_text, paste0(" ", field),
      paste0(", ", field))
  }

  return(cleaned_text)
}

# info for odd numbered people closes out with \n\n(number)\n\n
# \n\n(number)\n for even numbered people
# Each new page is denoted with [number]
# 2. Extraction function - pulls out individual records
extract_records <- function(preprocessed_text) {
  # Split by record numbers
  records <- unlist(str_split(preprocessed_text, "\\s+\\([0-9]+\\)\\s+"))

  return(records)
}

# 3. parse each record into structured data
parse_records <- function(records) {
  # Apply to each record in the vector
  parse_name <- sapply(records, function(record) {
    # Extract name by splitting at "Docket Month"
    parts <- unlist(str_split(record, ", Docket Month"))
    if(length(parts) > 0) {
      # Remove any quotes at the beginning
      name <- str_replace(parts[1], "^\"", "")
      return(name)
    } else {
      return(NA)
    }
  })

  parse_county <- sapply(records, function(record) {
    # extract word before "County" and make sure that we are extracting from
    # the correct part of the record
    # sometimes county names also listed in the consecutive cases
    county_pattern <- "([A-Za-z]+)\\s+County, Authority"
    county_match <- str_match(record, county_pattern)
    # If we found a word followed by 'County' in the record
    # return just that word. Otherwise return NA.
    if (!is.na(county_match[1])) {
      return(county_match[2])
    } else {
      return(NA)
    }
  })

  parse_month <- sapply(records, function(record) {
    month_pattern <- "Docket Month:\\s*([0-9]{2})"
    month_match <- str_match(record, month_pattern)
    if (!is.na(month_match[1])) {
      return(month_match[2])
    } else {
      return(NA)
    }
  })

  parse_doc_num <- sapply(records, function(record) {
    # Look for 5+ digit sequence right before ", Type:"
    pattern <- "([0-9]{5,})\\s*,\\s*Type:"
    match <- str_match(record, pattern)
    if (!is.na(match[1])) {
      return(match[2])
    } else {
      return(NA)
    }
  })

  parse_type <- sapply(records, function(record) {
    pattern <- "Type:\\s*([^,]*?)(?=,\\s*OFFENSE|$)"
    match <- str_match(record, pattern)
    if (!is.na(match[1])) {
      return(str_trim(match[2]))
    } else {
      return(NA)
    }
  })

  parse_personal_appearance <- sapply(records, function(record) {
    pattern <- "Personal Appearance:\\s*([^,]*?)(?=,\\s*Type:|$)"
    match <- str_match(record, pattern)
    if (!is.na(match[1])) {
      return(str_trim(match[2]))
    } else {
      return(NA)
    }
  })

  parse_next_board <- sapply(records, function(record) {
    # Look for text between "Next Board Consideration:" and ", Sentence:"
    pattern <- "Next Board Consideration:\\s*(.*?)(?=,\\s*Sentence:|$)"
    match <- str_match(record, pattern)

    if (!is.na(match[1])) {
      text <- str_trim(match[2]) |>
        str_remove_all(",")

      # Replace spaces with semicolons, but not before "days"
      text <- str_replace_all(text, " (?!days)", "; ")

      return(text)
    } else {
      return(NA)
    }
  })


  parse_sentence <- sapply(records, function(record) {
    # Look for text between "Sentence:" and ", Concurrent Cases:"
    pattern <- "Sentence:\\s*(.*?)(?=,\\s*Concurrent Cases:|$)"
    match <- str_match(record, pattern)
    if (!is.na(match[1])) {
      return(str_trim(match[2]))
    } else {
      return(NA)
    }
  })

  parse_concurrent_cases <- sapply(records, function(record) {
    pattern <- "Concurrent Cases:\\s*(.*?)(?=,\\s*Consecutive Cases:|$)"
    match <- str_match(record, pattern)
    if (!is.na(match[1])) {
      return(str_trim(match[2]))
    } else {
      return(NA)
    }
  })

  parse_consecutive_cases <- sapply(records, function(record) {
    # Look for text between "Consecutive Cases:" and
    # and either ", Detainer:" or ",, Detainer:" or end of string
    pattern <- "Consecutive Cases:\\s*(.*?)(?=,\\s*Detainer:|,,\\s*Detainer:|$)"
    match <- str_match(record, pattern)
    if (!is.na(match[1])) {
      return(str_trim(match[2]))
    } else {
      return(NA)
    }
  })

  parse_detainer <- sapply(records, function(record) {
    pattern <- "Detainer:\\s*([^\"]*?)(?=\"|$)"
    match <- str_match(record, pattern)
    if (!is.na(match[1])) {
      return(str_trim(match[2]))
    } else {
      return(NA)
    }
  })

  # Create dataframe
  result <- data.frame(
    records = records,
    name = parse_name,
    doc_num_raw = parse_doc_num,
    county = parse_county,
    docket_month = parse_month,
    personal_appearance = parse_personal_appearance,
    next_board_WIP = parse_next_board,
    sentence = parse_sentence,
    type = parse_type,
    concurrent_cases = parse_concurrent_cases,
    consecutive_cases = parse_consecutive_cases,
    detainer = parse_detainer,
    stringsAsFactors = FALSE
  )

  result <- result |>
    mutate(
      # Split next_board by semicolon and extract the parts directly
      jail_time = str_extract(next_board_WIP, "^[^;]+"),
      reception_date = str_extract(str_replace(next_board_WIP, "^[^;]+;", ""), "^[^;]+"),
      projected_release_date = str_extract(str_replace(str_replace(next_board_WIP, "^[^;]+;", ""), "^[^;]+;", ""), "^[^;]+"),

      # Update next_board to remove the extracted parts
      next_board_WIP = str_replace(str_replace(str_replace(next_board_WIP, "^[^;]+;", ""), "^[^;]+;", ""), "^[^;]+;?", "")
    ) |>
    filter(!(is.na(records) | records == "") & !(is.na(name) | name == ""))

  return(result)
}

process_all_parole_docket_pdfs <- function(base_dir = "docs/oklahoma_ppb_dockets_pdfs/2024/Parole") {
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
      preprocess_text() |>
      extract_records() |>
      parse_records() |>
      filter(!(is.na(records) | records == "")) |>
      mutate(month = month,
             year = 2024)

    # Add to results
    all_results <- bind_rows(all_results, processed_data)
  }

  all_results |>
    select(docket_month, month, doc_num_raw, name, type, sentence, everything())
}



