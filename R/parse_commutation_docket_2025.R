process_commutation_docket_202501 <- function(
    base_dir,
    ok_counties,
    commutations_docket_except_december
) {
  known_offenses <- commutations_docket_except_december |>
    count(offense_desc) |>
    mutate(offense_desc = str_replace_all(offense_desc, '^"|"$', '') |>
             str_trim()
    )
  known_offenses <- known_offenses$offense_desc

  base_dir |>
    fix_wrapped_lines() |>
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
    mutate(month = "January",
           year = 2025) |>
    rename(doc_num_raw = DOC_NUM)
}
