process_administrative_parole_docket_report_2024 <- function(base_dir, ok_counties) {
  # List all PDF files in the directory
  pdf_files <- list.files(base_dir, pattern = "\\.pdf$", full.names = TRUE)

  # Exclude files with excluded months in their names
  # These are excluded because the offense description are lowercase
  excluded_months <- c("july", "august", "september",
                       "october", "november", "december")

  pdf_files <- pdf_files[!grepl(paste(excluded_months, collapse = "|"), tolower(basename(pdf_files)))]

  # Create empty list to store results
  all_results <- list()

  # Process each PDF file
  for (pdf_file in pdf_files) {
    file_name <- basename(pdf_file)
    message("Processing: ", file_name)

    # Extract month from filename if possible
    month_name <- str_extract(
      file_name,
      "(January|February|March|April|May|June)")

    if(is.na(month_name))
      month_name <- "Unknown"

    # Process this file
    result <- fix_wrapped_lines(pdf_file) |>
      data.frame() |>
      extract_admin_parole_offense_and_fields(known_offenses, ok_counties) |>
      separate(
        text_before_offense,
        into = c("DOCKET_NUM", "DOC_NUM", "LAST_NAME", "FIRST_NAME", "CRF_NUM"),
        sep = ", ",
        fill = "right",
        extra = "drop",
        remove = FALSE
      ) |>
      select(-c(header)) |>
      select(-c(text_before_offense)) |>
      mutate(month = month_name,
             year = 2024,
             filename = file_name)

    all_results[[file_name]] <- result
  }

  # Combine all results
  combined_data <- bind_rows(all_results)

  return(combined_data)
}

check_lowercase_offenses <- function(row_text, known_offenses) {
  row_text <- as.character(row_text)

  # First try the original extraction method
  offense <- extract_offense_desc(row_text)

  if (!is.na(offense)) {
    return(offense)
  }

  row_text_lower <- tolower(row_text)

  matched_offense <- known_offenses |>
    discard(is.na) |> # Remove NA known offenses
    str_replace_all('"', '') |> # Clean quotes
    str_trim() |>
    keep(~ nchar(.x) >= 5) |> # Keep only offenses longer than 5 chars
    detect(~ str_detect(row_text_lower, tolower(.x))) # Find the first match

  matched_offense %||% NA_character_  # If none found, return NA
}

extract_admin_parole_offense_and_fields <- function(df,
                                                    known_offenses,
                                                    ok_counties) {
  df |>
    rowwise() |>
    mutate(
      # First find the county
      county = detect_county(
        row_text = rows,
        ok_counties = ok_counties
      ),
      # Then extract numbers before and after county
      sent_count = extract_number_relative_to_county(rows, county, "before"),
      # Extract offense description
      offense_desc = check_lowercase_offenses(rows, known_offenses),
      # Extract everything before the offenses
      text_before_offense = extract_text_before_offense(rows, offense_desc)
    ) |>
    ungroup()
}

process_lowercase_administrative_parole_docket_report_2024 <- function(
  base_dir = "docs/oklahoma_ppb_dockets_pdfs/2024/Administrative Parole",
  known_offenses,
  ok_counties
) {
  # List all PDF files in the directory
  pdf_files <- list.files(base_dir, pattern = "\\.pdf$", full.names = TRUE)

  # Exclude files with excluded months in their names
  # These are excluded because the offense description are allcaps
  excluded_months <- c("january", "february", "march", "april", "may",
                       "june", "july", "august", "october")

  pdf_files <- pdf_files[!grepl(paste(excluded_months, collapse = "|"), tolower(basename(pdf_files)))]

  # Create empty list to store results
  all_results <- list()

  # Process each PDF file
  for (pdf_file in pdf_files) {
    file_name <- basename(pdf_file)
    message("Processing: ", file_name)

    # Extract month from filename if possible
    month_name <- str_extract(
      file_name,
      "(September|November|December)")

    if(is.na(month_name))
      month_name <- "Unknown"

    # Process this file
    result <- fix_wrapped_lines(pdf_file) |>
      data.frame() |>
      extract_admin_parole_offense_and_fields(known_offenses, ok_counties) |>
      separate(
        text_before_offense,
        into = c("DOCKET_NUM", "DOC_NUM", "LAST_NAME", "FIRST_NAME", "CRF_NUM"),
        sep = ", ",
        fill = "right",
        extra = "drop",
        remove = FALSE
      ) |>
      select(-c(header)) |>
      select(-c(text_before_offense)) |>
      mutate(month = month_name,
             year = 2024,
             filename = file_name) |>
      rename(doc_num_raw = DOC_NUM)

    all_results[[file_name]] <- result
  }

  # Combine all results
  combined_data <- bind_rows(all_results)

  return(combined_data)
}

