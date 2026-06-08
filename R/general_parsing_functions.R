fix_wrapped_lines <- function(pdf_path) {
  # Read the PDF
  lines <- pdf_text(pdf_path) |>
    unlist() |>
    str_split("\n") |>
    unlist() |>
    str_squish()

  # Identify header line
  header_idx <- which(str_detect(lines, "DOCKET #|DOC_NUM|LAST_NAME"))

  if (length(header_idx) == 0) {
    return("No header found in the PDF")
  }

  header_line <- lines[header_idx[1]]

  # Extract lines after the header
  data_lines <- lines[(header_idx[1] + 1):length(lines)]

  # Identify row starts and reconstruct complete rows
  row_start_indices <- which(str_detect(data_lines, "^\\s*\\d+\\s"))

  # Reconstruct complete rows
  complete_rows <- vector("character", length(row_start_indices))

  for (i in 1:length(row_start_indices)) {
    start_idx <- row_start_indices[i]

    # If this is the last row start, go to the end of data_lines
    # Otherwise, go until the next row start
    end_idx <- if (i == length(row_start_indices)) length(data_lines) else row_start_indices[i+1] - 1

    # Combine all lines for this row
    row_lines <- data_lines[start_idx:end_idx]
    complete_row <- paste(row_lines, collapse = " ")

    # Normalize spaces
    complete_row <- str_squish(complete_row)

    complete_rows[i] <- complete_row
  }

  # Return the header and complete rows
  return(list(header = header_line,
              rows = complete_rows))
}

# Specifying that it should take the last match because
# the county value comes at the end of the string
# otherwise we run into problem if someone's first name
# is Bryan or Marshall (names come at the beginning of the string)
detect_county <- function(row_text, ok_counties) {
  # Convert to character in case it's not
  row_text <- as.character(row_text)

  # Find matching county
  matching_county <- ok_counties[sapply(ok_counties, function(county) {
    str_detect(row_text, fixed(county))
  })]

  # Return last match or NA if none found
  if(length(matching_county) > 0) {
    return(matching_county[length(matching_county)])
  } else {
    return(NA_character_)
  }
}

extract_number_relative_to_county <- function(
    row_text,
    county,
    position = c("before", "after")
) {
  if(is.na(county)) return(NA_integer_)
  position <- match.arg(position)

  row_text <- as.character(row_text)

  # Choose pattern based on position
  pattern <- if(position == "before") {
    paste0("\\b(\\d+)\\s+(?=\\b", county, "\\b)")
  } else {
    paste0("\\b", county, "\\b\\s+(\\d+)")
  }

  # Extract number
  number <- str_extract(row_text, pattern)
  number <- str_extract(number, "\\d+")

  if(is.na(number)) return(NA_integer_)
  return(as.integer(number))
}


# Find text in ALL CAPS (for offense descriptions)
#TODO: This is a little redundant but actually couldn't get it to work
# with a single line of regex. It either drops something out or takes in
# an extra unnecesary number which should be the count_num
extract_offense_desc <- function(row_text) {
  row_text <- as.character(row_text)

  # Try to capture ALL CAPS text that may include special characters, parentheses, and periods
  # Uses {5,} after [A-Z] to fix where the CRF_num
  # contains capital letters which then takes
  # the middle initial + start of CRF_num and using that
  # as the offense description
  with_paren <- str_extract(row_text, "\\b[A-Z][A-Z/&\\s.-]{5,}(?:\\([^)]*\\))?")

  if (!is.na(with_paren)) {
    return(with_paren)
  }

  # Capture ALL CAPS text while avoiding single-letter initials
  # and allowing periods
  all_matches <- str_extract_all(
    row_text, "(?<=^|\\s|/|\\(|\\d)\\b[A-Z][A-Z/&\\s.-]{5,}\\b")[[1]]

  # If we found any matches, return the longest one
  # as final assurance that we don't pick up middle initial
  # or other random stuff
  if(length(all_matches) > 0) {
    # Get the longest match by character count
    longest_match <- all_matches[which.max(nchar(all_matches))]

    return(longest_match)

  } else {
    return(NA_character_)
  }
}

# take everything that comes before the offense description and treat that
# as a single column that we will then split into docket_num,
# doc_num, last_name, etc.
extract_text_before_offense <- function(row_text, offense_desc) {
  if (is.na(offense_desc)) return(NA_character_)

  # Convert to character
  row_text <- as.character(row_text)

  # Split the text at the offense description
  parts <- str_split(row_text, fixed(offense_desc), n = 2)[[1]]

  # Return the part before the offense description with normalized spaces
  if (length(parts) > 0) {
    # First trim any whitespace at the beginning and end
    # Then replace any internal consecutive whitespace with comma
    # we then use this to split into separate columns
    # Filter out any empty strings or commas
    # Join words back with commas
    normalized_text <- parts[1] |>
      str_trim() |>
      str_split("\\s+") |>
      unlist() |>
      discard(~ .x == "" | .x == ",") |>
      str_c(collapse = ", ")

    return(normalized_text)
  } else {
    return(NA_character_)
  }
}
