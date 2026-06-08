extract_commutation_docket_offense_and_fields <- function(df, ok_counties) {
  df |>
    rowwise() |>
    mutate(
      # First find the county
      county = detect_county(rows, ok_counties),
      # Then extract numbers before
      # TODO: figure out what to do with the "after" sometimes this is
      # the DA district and sometimes county number
      count_no = extract_number_relative_to_county(rows, county, "before"),
      # Extract offense description (ALL CAPS text)
      offense_desc = extract_offense_desc(rows),
      # Extract everything before the ALL CAPS offenses
      text_before_offense = extract_text_before_offense(rows, offense_desc)
    ) |>
    ungroup()
}

process_commutation_pdfs_2024 <- function(
    base_dir,
    ok_counties,
    include_results = FALSE,
    exclude_months = NULL,
    year = 2024
) {
  # 1. list and filter PDF files
  pdf_files <-
    list.files(base_dir, pattern = "\\.pdf$", full.names = TRUE) |>
    discard(~ str_detect(basename(.x) |>
                           str_to_lower(),
                         paste(exclude_months, collapse = "|")))

  # 2. choose the appropriate extract function
  extract_fields <-
    if (include_results) extract_commutation_results_offense_and_fields
  else extract_commutation_docket_offense_and_fields

  # 3. process each PDF into a tibble
  results_list <-map(pdf_files, function(pdf_file) {
    file_name <- basename(pdf_file)

    month_name <- str_extract(file_name,
      "(January|February|March|April|May|June|July|August|September|October|November)")

    if(is.na(month_name))
      month_name <- "Unknown"

    fix_wrapped_lines(pdf_file)[["rows"]] |>
      tibble(rows = _) |>
      extract_fields(ok_counties) |>
      separate_wider_delim(text_before_offense,
                           delim = ", ",
                           names = c(
                             "DOCKET_NUM", "DOC_NUM", "LAST_NAME",
                             "FIRST_NAME", "TEMP_FIELD", "CRF_NUM"
                             ),
                           too_few  = "align_start",
                           too_many = "drop") |>
      # Determine if TEMP_FIELD is a middle initial or CRF_NUM
      # Not everyone has a middle initial
      mutate(
        MIDDLE_INITIAL = if_else(
          (str_length(TEMP_FIELD) == 1 & str_detect(TEMP_FIELD, "^[A-Z]$")) |
            str_detect(TEMP_FIELD, "^[A-Z][a-z]+$"),
          TEMP_FIELD,
          NA_character_
          ),
        CRF_NUM = if_else(
          is.na(MIDDLE_INITIAL) & !is.na(TEMP_FIELD),
          TEMP_FIELD,
          CRF_NUM
          ),
        LAST_NAME = str_trim(LAST_NAME)) |>
      select(-TEMP_FIELD) |>
      mutate(
        month = month_name,
        year = year,
        filename = file_name
        ) |>
      rename(
        doc_num_raw = DOC_NUM
        )
    }
    )

  # 4. bind into one data frame
  combined_data <- list_rbind(results_list)

  return(combined_data)
}
