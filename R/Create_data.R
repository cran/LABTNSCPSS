#' Create and clean episode data
#'
#' Reads a CSV file, renames columns using a mapping, parses dates, cleans ICD codes,
#' and writes a cleaned CSV to `cleaned_path`.
#'
#' @param input_file Path to the input CSV.
#' @param col_mapping Named character vector or list mapping standard names to user column names.
#'   Example: list(patient_id="Patient_id", ICD="diag_code", start_date="date_start", end_date="date_end", episode_id="episode_id")
#' @param cleaned_path Path to write the cleaned CSV. If NULL, a default path in `tempdir()` is used.
#'
#' @return The `cleaned_path` (character string). Also invisibly returns the cleaned data.frame.
#' @export
Create_data <- function(input_file, col_mapping, cleaned_path = NULL) {

  if (!file.exists(input_file)) stop("File not found: ", input_file)

  df <- utils::read.csv(input_file, stringsAsFactors = FALSE)

  # Validate mapping
  missing_cols <- setdiff(unname(col_mapping), names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in CSV: ", paste(missing_cols, collapse = ", "))
  }

  # Rename columns to standard names
  for (std_name in names(col_mapping)) {
    user_col <- col_mapping[[std_name]]
    names(df)[names(df) == user_col] <- std_name
  }

  # Parse dates (robust)
  if ("start_date" %in% names(df)) {
    df$start_date <- as.Date(lubridate::parse_date_time(
      df$start_date,
      orders = c("ymd HMS","ymd HM","ymd","dmy HMS","dmy HM","dmy"),
      quiet = TRUE
    ))
  }

  if ("end_date" %in% names(df)) {
    df$end_date <- as.Date(lubridate::parse_date_time(
      df$end_date,
      orders = c("ymd HMS","ymd HM","ymd","dmy HMS","dmy HM","dmy"),
      quiet = TRUE
    ))
  }

  # Clean ICD codes
  if ("ICD" %in% names(df)) {
    df$ICD <- gsub("\\.", "", df$ICD)
  }

  # Default output path if not provided
  if (is.null(cleaned_path)) {
    input_basename <- tools::file_path_sans_ext(basename(input_file))
    cleaned_path <- file.path(tempdir(), paste0("input_data_cleaned_", input_basename, ".csv"))
  }

  # Write cleaned file
  utils::write.csv(df, cleaned_path, row.names = FALSE)

  invisible(df)
  cleaned_path
}
