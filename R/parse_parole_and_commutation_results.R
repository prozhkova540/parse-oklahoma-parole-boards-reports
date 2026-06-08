read_pdf_text_result_report <- function(path) {
  prep_text <- pdf_text(path) |>
    # Strip out the big page‐headers
    str_remove_all(regex("(?s)PAGE:.*?BOARD ACTION.*?\\n", ignore_case = TRUE)) |>
    str_remove_all("^OK Parole Bo.") |>
    str_remove_all("OFFICIAL BOARD ACTION") |>
    str_squish()

  return(prep_text)
}

mark_records_helper <- function(text) {
  # Pattern to match names followed by DOC numbers anywhere in text

  name_pattern <- regex(
    paste0(
      "([A-Za-z][A-Za-z'\\-]*)", # Last name
      ",\\s+", # comma + space
      "(", # Begin first+middles group
      "[A-Za-z][A-Za-z'\\-]*", #   First name
      "(?:\\s+(?:[A-Za-z][A-Za-z'\\-]*|[A-Z]\\.))*", # Optional: space + (name‐segment OR initial.)
      ")",  # End first+middles group
      "\\s+(\\d{5,8})(?!\\d)"  # DOC# (5–8 digits, not followed by another digit)
    )
  )

  # Add markers before each name occurrence
  marked_text <- str_replace_all(
    text,
    name_pattern,
    "\n\n=== NEW RECORD ===\n\\0"
  )

  return(marked_text)
}

split_records_result_report <- function(text) {
  # Define the pattern to identify the start of each record -- name
  start_pattern <- "\n\n=== NEW RECORD ===\\n"

  end_patterns <- c(
    "TOTAL\\s+SENTENCE\\s+LENGTH:\\s+\\d+y\\s+\\d+m\\s+\\d+d$",
    "Life$",
    "Death$",
    "Life\\s+W/O$"
  )
  end_pattern <- paste(end_patterns, collapse = "|")

  # Split the raw text into potential records
  raw_records <- text |>
    mark_records_helper() |>
    paste(collapse = "\n") |>
    str_split(start_pattern) |>
    pluck(1) |>   # extract that vector (same as .[[1]])
    str_trim() |>
    tibble(record = _) |>
    filter(record != "")

  # Initialize last_name to maintain context across records
  last_name <- NULL

  # Process each record to extract the name and validate the ending
  parsed_records <- map(raw_records$record, function(record) {
    # Enhanced name parsing to handle middle initials with or without a period
    name_match <- str_match(
      record, "^([A-Z][A-Za-z\\s\\-']+,\\s+[A-Z](?:\\.\\s+|\\s+)?[A-Za-z\\s\\-']+(?:\\s+Jr|\\s+Sr)?)\\s+\\d{5,}"
    )

    # Update last_name if a valid match is found
    if (!is.na(name_match[1, 2])) {
      last_name <- name_match[1, 2]
    } else if (!is.null(last_name)) {
      # Append to the last known name for incomplete records
      record <- paste(last_name, record)
    }

    #Check if the record ends with a valid sentence length pattern
    if (!str_detect(record, end_pattern)) {
      warning("Record does not end with a valid sentence length pattern:\n", record)
    }

    tibble(
      record = record,
      name = last_name
    )
  })

  list_rbind(parsed_records)
}

extract_board_decisions <- function(record_text) {
  board_decision_patterns <- c(
    "Parole To Street",
    "Complete Substance Abuse",
    "Mental Health Evaluation",
    "Denied 3 Years",
    "Passed to Stage II",
    "Parole Denied by Board",
    "Commutation Denied",
    "Denied",
    "Denied 1 Year",
    "Parole to Detainer or",
    "Test for GED Prior",
    "Stricken",
    "Commutation of Sentence",
    "Granted by Governor",
    "Commutation to Time",
    "Served Granted By Governor",
    "Parole To Detainer or Void",
    "Mandatory UA's for12",
    "Treatment Eval/Follow Trtmt",
    "Recommendations2.",
    "Complete Anger",
    "Interlock Device for 12",
    "Parole to Consecutive",
    "Complete Cognitive",
    "Behavioral Treatment",
    "Complete Regimented",
    "Treatment Program Prior ",
    "Complete Batterer's",
    "Intervention Prior to Release",
    "Recommend by Board",
    "Pass to next month",
    "Pass To Another Docket",
    "Complete RDAP Prior to",
    "Parole to Consecutive",
    "Discharged",
    "Recommend to Stage II",
    "Waived Parole Hearing",
    "Support Svcs (SASS) Prior to",
    "Treatment Recommendations",
    "52 Week Tribal or Certified",
    "Batterer's Program",
    "Follow Treatment",
    "Clemency Recommended",
    # New as of Jun 2025
    "Passed per Attorney"
  )

  # Escape special characters and build non-capturing regex group
  pattern <-
    paste0(
      "(",
      paste(board_decision_patterns |>
              str_replace_all(
                "([\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}])", "\\\\\\1") |>
              # Sort longer strings first to avoid substring precedence
              sort(decreasing = TRUE),
            collapse = "|"),
      ")"
    )

  matches <- str_extract_all(record_text, regex(pattern, ignore_case = TRUE))[[1]]

  if (length(matches) == 0) {
    return(NA_character_)
  } else {
    return(paste(toupper(matches), collapse = "; "))
  }
}

parse_single_record_result_report <- function(
  record_text,
  name
) {
  hearing_patterns <- c(
    "Administrative Parole Hearing",
    "Commutation Hearing (Stage Two)",
    "Parole Hearing (Stage II)",
    "Parole Hearing (Stage I)",
    "Commutation Hearing",
    "Executive Clemency",
    "Re-Entry Hearing",
    "Review Hearing",
    "Pardon (Board)",
    "Parole Hearing"
  )

  # Extract DOC number - 5+ digits
  doc_match <- str_match(record_text, "\\b(\\d{5,})\\b")
  doc_num_raw <- if (!is.na(doc_match[1,2])) doc_match[1, 2] else NA_character_

  # Extract county - follows DOC number
  county_match <- str_match(record_text, "\\b\\d{5,}\\b\\s+(\\w+)")
  county <- if (!is.na(county_match[1, 2])) county_match[1, 2] else NA_character_

  # Extract hearing type - match known phrases
  # stringr::fixed()
  matched_hearing_patterns <-
    hearing_patterns[str_detect(record_text, fixed(hearing_patterns))]

  hearing_type <- if (length(matched_hearing_patterns) > 0) {
    matched_hearing_patterns[1] # returns first most specific match
    } else {
    NA_character_
  }

  # Extract sentence length - must follow "TOTAL SENTENCE LENGTH:"
  sentence_patterns <- c(
    "TOTAL\\s+SENTENCE\\s+LENGTH:\\s+(\\d+y\\s+\\d+m\\s+\\d+d)",
    "(Life)",
    "(Death)",
    "(Life\\s+W/O)"
  )

  sentence_pattern <- paste(sentence_patterns, collapse = "|")
  sentence_match <- str_match(record_text, sentence_pattern)
  # %||% returns the left-hand side if it is not NULL; otherwise, NA_character_
  sentence <- sentence_match[!is.na(sentence_match)][2] %||% NA_character_

  # Extract board decisions in order of appearance and then use this for conditions
  raw_board_decision <- extract_board_decisions(record_text)

  # 1) split off anything AFTER the first “;” (or NA if none)
  stipulations_temp <- case_when(
    str_detect(raw_board_decision, ";") ~ str_extract(raw_board_decision, "(?<=; ).*"),
    TRUE ~ NA_character_)

  # 2) Trim board_decision to only the part BEFORE the first semicolon
  board_decision <- case_when(
    !is.na(raw_board_decision) ~ str_extract(raw_board_decision, "^[^;]+"),
    TRUE ~ NA_character_)

  # 3) Clean up conditions_temp column
  board_stipulations <- case_when(
    is.na(stipulations_temp) & !str_detect(board_decision, "(?i)denied|discharged|stricken") & str_detect(hearing_type, "Parole Hearing") ~  "check for parole condition",
    is.na(stipulations_temp) & !str_detect(board_decision, "(?i)denied|discharged|stricken") & str_detect(hearing_type, "Commutation Hearing") ~ "check for commutation condition",
    TRUE ~ stipulations_temp
  )

  parsed_tibble <- tibble(
    record = record_text,
    name = name,
    doc_num_raw = doc_num_raw,
    county = county,
    board_decision = board_decision,
    board_stipulations = board_stipulations,
    hearing_type = hearing_type,
    sentence_length = sentence
  )

  return(parsed_tibble)
}


parse_all_records_result_report <- function(pdf_path) {

  text <- read_pdf_text_result_report(pdf_path)

  records_df <- split_records_result_report(text)

  parsed_list <- map2(
    records_df$record,
    records_df$name,
    parse_single_record_result_report
  )

  all_records <- list_rbind(parsed_list) |>
    mutate(
      name = case_when(
        # 2025
        str_detect(record, "Christian, Kenneth E.") ~ "Christian, Kenneth E.",
        str_detect(record, "Miller, Kiante L.") ~ "Miller, Kiante L.",
        str_detect(record, "Owens, Terrion E.") ~ "Owens, Terrion E.",
        str_detect(record, "Rogers, Chase W.") ~ "Rogers, Chase W.",
        str_detect(record, "Lindley, Willie B.") ~ "Lindley, Willie B.",
        str_detect(record, "Taylor, Claude M.") ~ "Taylor, Claude M.",
        str_detect(record, "Carson, Willie C. Jr") ~ "Carson, Willie C. Jr",
        str_detect(record, "Kelley, Timothy L.") ~ "Kelley, Timothy L.",
        str_detect(record, "Brignoni-Molina, jose") ~ "Brignoni-Molina, Jose",
        # 2026
        str_detect(record, "Allison, Zane T.") ~ "Allison, Zane T.",
        str_detect(record, "Johnston, Devon K.") ~ "Johnston, Devon K.",
        str_detect(record, "Mallett, Robert L.") ~ "Mallett, Robert L.",
        str_detect(record, "Hunter, Nathan A.") ~ "Hunter, Nathan A.",
        str_detect(record, "Gage, Benjamin J.") ~ "Gage, Benjamin J.",
        TRUE ~ name
      )
    )

  return(all_records)
}
