parse_sir_docket_2024 <- function(
    may_SIR_docket,
    ok_counties
) {
    parse_all_records_sir_docket_2024(
        may_SIR_docket,
        ok_counties
    )
}

read_pdf_text_sir_docket_2024 <- function(path) {
  pdf_text(path) |>
    # drop any “Page X of Y” headers
    str_remove_all(regex("Page\\s*\\d+\\s*of\\s*\\d+", ignore_case = TRUE)) |>
    str_squish()
}

split_records_sir_docket_2024 <- function(raw_text) {
  start_pattern <- "(?=\\d{1,3}\\.\\s*[^\\d]+?,)"

  all_text <- paste(raw_text, collapse = "\n")

  records <- str_split(all_text, start_pattern)[[1]] |>
    str_trim()

  # drop the initial header‐chunk if it doesn't start with “NN.”
  if (length(records) > 0 && !str_detect(records[[1]], "^\\d{1,3}\\.")) {
    records <- records[-1]
  }

  tibble(record = records) |>
    filter(record != "")
}

parse_single_record_sir_docket_2024 <- function(record_text, ok_counties) {

  record_text <- as.character(record_text)

  name_pattern <- regex(
    "^\\d{1,3}\\.\\s*([^\\d]+?)\\s+(?=\\d{5,})",
    multiline = TRUE
  )
  name <- str_match(record_text, name_pattern)[,2] |>
    str_squish() %||% NA_character_

  doc_num_raw <- str_match(record_text, "\\b(\\d{5,})\\b") |>
    pluck(2) %||% NA_character_


  county_pattern <- regex(
    paste0("\\b(", paste(ok_counties$county, collapse = "|"), ")\\b"),
    ignore_case = TRUE
  )
  county <- str_extract(record_text, county_pattern)

  tibble(
    record_text = record_text,
    name = name,
    doc_num_raw = doc_num_raw,
    county = county
  )

}

parse_all_records_sir_docket_2024 <- function(pdf_path, ok_counties) {
  read_pdf_text_sir_docket_2024(pdf_path) |>
    split_records_sir_docket_2024() |>
    pull(record) |>
    map(safely(\(x) {
        parse_single_record_sir_docket_2024(x, ok_counties)
    })) |>
    transpose() |>
    pluck("result") |>
    compact() |>
    list_rbind() |>
    filter(!(is.na(name) & is.na(doc_num_raw) & is.na(county)))
}
