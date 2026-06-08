export_csv <- function(data, path) {
  write_csv(
    data,
    path
  )

  return(path)
}

get_ok_counties <- function() {
  get_acs(geography = "county",
          variables = "B19013_001",
          state = "OK",
          year = 2020) |>
    mutate(county = str_replace(NAME, " County, Oklahoma", "")) |>
    select(county)
}
