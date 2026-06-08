# This script handles Parole, Administrative Parole,
# Stage One Commutations, and SIR Docket starting February 2025 when PPB
# stopped using docket website in favor of monthly reports.

#TODO: still need to do offenses but don't need that for Phase 1 of PC

# each page contains multiple records
read_pdf_text_docket_report <- function(path) {
  pdf_text(path) |>
    # Combine all pages into one long string
    paste(collapse = " ") |>
    str_replace_all("\n", " ") |>
    # Remove repeated header lines
    str_remove_all(regex("REQUESTOR:.*?PRD", ignore_case = TRUE)) |>
    str_remove_all(regex(" OK Parole Bo HEARING DOCKET PAGE: ", ignore_case = TRUE)) |>
    # Remove varying footer line like: 7 of 109 REPORT NO
    str_remove_all(regex(
      "\\d+ of \\d+ REPORT NO\\..*?PROCESSED: \\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2} [AP]M",
      ignore_case = TRUE
    )) |>
    str_squish()
}

split_by_record_docket_report <- function(text) {
  # split on (1), (2), -->
  str_split(text, "\\(\\d+\\)\\s+")[[1]] |>
    # create tibble with column 'record'
    tibble(record = _)
}

# Parse a single row or record (person)
#TODO: extract offenses, multiple counts
parse_single_record_docket_report <- function(record_text) {
  hearing_types <- c("Parole Hearing (Stage I)",
                     "Administrative Parole",
                     "Parole Hearing (Stage II)",
                     "Executive Clemency (Board)",
                     "Re-Entry Hearing",
                     # issues with splitting up because sometimes text between
                     # Hearing and (Initial) or Hearing and (Stage Two)
                     # "Commutation Hearing (Initial)",
                     # "Commutation Hearing (Stage Two)",
                     "Commutation Hearing",
                     "Parole Hearing (Board)")

  # Extract name - stop before the doc number
  name_match <- str_match(
    record_text,
    "^([A-Za-z]+,\\s+[A-Za-z\\s]+)\\s+\\d{5,}"
  )

  name <- if (!is.null(name_match) && !is.na(name_match[1])) name_match[2] else NA_character_

  # Extract doc number - 5+ digits
  doc_match <- str_match(
    record_text, "\\s(\\d{5,})\\s")

  doc_num_raw <- if (!is.null(doc_match) && !is.na(doc_match[1])) doc_match[2] else NA_character_

  # Extract hearing type
  type_pattern <- hearing_types |>
    str_replace_all("([()])", "\\\\\\1") |>  # Escape parentheses for regex
    str_c(collapse = "|")

  hearing_type <- str_extract(record_text, type_pattern)

  # Extract everything after "TOTAL SENTENCE LENGTH:"
  sentence_match <- str_match(
    record_text,
    "TOTAL SENTENCE LENGTH:\\s+(.*?)(?=$|\\([0-9]+\\))"
  )

  sentence <- if (!is.null(sentence_match) && !is.na(sentence_match[1]))
    str_trim(sentence_match[2]) else NA_character_


  tibble(
    record = record_text,
    name = name,
    doc_num_raw = doc_num_raw,
    hearing_type = hearing_type,
    sentence_full = sentence
  )
}

# Process a single PDF file
parse_all_records_docket_report <- function(pdf_path) {
  # Read & clean full PDF text
  text <- read_pdf_text_docket_report(pdf_path)

  # Break into records
  records_df <- split_by_record_docket_report(text)

  # Process each record
  parsed_records <- map(records_df$record, function(rec) {
    tryCatch({
      parse_single_record_docket_report(rec)
    }, error = function(e) {
      NULL
    })
  })

  bind_rows(parsed_records)

}


process_all_reports <- function(
    base_dir,
    parser,
    months = NULL, # character vector of month names, or NULL for all
    year) {
  # 1. list all PDFs
  pdf_files <- list.files(
    base_dir,
    pattern = "\\.pdf$",
    full.names = TRUE
  )

  # 2. if months specified, filter by those names in the filename
  if (!is.null(months)) {
    user_pattern <- str_c(months, collapse = "|")
    pdf_files <- keep(
      pdf_files,
      ~ str_detect(basename(.x), regex(user_pattern, ignore_case = TRUE))
    )
  }

  # 3. month-extraction pattern if months not specified
  all_pattern <- str_c(month.name, collapse = "|")

  # 4. parse each file, and add year & filename
  results <- map(pdf_files, \(path) {
    file_name <- basename(path)
    month_name <- str_extract(file_name, regex(all_pattern, ignore_case = TRUE))

    df <- parser(path) |>
      mutate(
        month = month_name,
        year = year,
        file_name = file_name
      )

    message(sprintf("%s → %d records", file_name, nrow(df)))
    df
  })

  # 5. combine and return
  list_rbind(results)
}
