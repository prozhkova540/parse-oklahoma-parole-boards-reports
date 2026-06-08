process_docket_parole_202501 <- function() {
  # Format for January 2025 matches 2024 but the rest of 2025 is formatted differently
  january_2025_docket <- "docs/oklahoma_ppb_dockets_pdfs/2025/Administrative Parole, Parole, Commuation, SIR/January 2025 Parole Docket.pdf"

  january_parole_docket_2025 <- pdf_text(january_2025_docket) |>
    preprocess_text() |>
    extract_records() |>
    parse_records() |>
    filter(!(is.na(records) | records == "")) |>
    mutate(month = "January",
           year = 2025)
}
