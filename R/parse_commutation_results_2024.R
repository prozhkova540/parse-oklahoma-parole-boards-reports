# For 2024 commutation results only

extract_commutation_result <- function(row_text, county) {
  # if the county is NA and returns NA_character_ (we can backfill manually)
  if(is.na(county)) return(NA_character_)

  # column 1: county name + whatever text follows
  # the specific result is in column 2
  str_match(as.character(row_text),
            paste0("\\b", county, "\\b.*?(DENIED|PASSED TO STAGE II|ASSED TO STAGE|STRICKEN)\\b"))[, 2]
}

extract_commutation_results_offense_and_fields <- function(df, ok_counties) {
  df |>
    rowwise() |>
    mutate(
      # First find the county
      county = detect_county(rows, ok_counties),
      # Then extract numbers before
      count_no = extract_number_relative_to_county(rows, county, "before"),
      # Extract offense description (ALL CAPS text)
      offense_desc = extract_offense_desc(rows),
      # Extract everything before the ALL CAPS offenses
      text_before_offense = extract_text_before_offense(rows, offense_desc),
      # Extract result columns
      board_decision = extract_commutation_result(rows, county)
    ) |>
    ungroup()
}

