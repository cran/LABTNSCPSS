#' Run the LABTNSCPSS Comorbidity-Frailty Pipeline
#'
#' Computes Charlson, Elixhauser, Combined Comorbidity,
#' Frailty, and Morbidity-Frailty scores from episode-level data.
#'
#' @param input_file Path to input CSV file.
#' @param col_mapping Named list mapping required fields to column names in `input_file`.
#' Must include: `patient_id`, `ICD`, `start_date`, `end_date`, `episode_id`.
#' @param coding_system One of: "ICD-10-CA", "ICD-10-CM", "ICD-11".
#' @param out_dir Directory where output CSV files will be written (default: `tempdir()`).
#'
#' @return A named list containing:
#' \describe{
#'   \item{scores_final}{Final combined scores (comorbidity + frailty).}
#'   \item{final_data_charlson}{Charlson scores by episode.}
#'   \item{final_data_elixhauser}{Elixhauser scores by episode.}
#'   \item{final_data_combined}{Combined comorbidity scores by episode.}
#'   \item{score_pop_Elixh}{Weighted Elixhauser score (ICD-10-CA only; may be NULL).}
#' }
#'
#' @examples
#' library(LABTNSCPSS)
#' f <- system.file("extdata", "testpackage.csv", package = "LABTNSCPSS")
#'
#' col_mapping <- list(
#'   patient_id = "trajectoire_id",
#'   ICD        = "diagnostic_code",
#'   start_date = "date_debut",
#'   end_date   = "date_fin",
#'   episode_id = "episode_id"
#' )
#'
#' res <- run_pipeline(
#'   input_file = f,
#'   col_mapping = col_mapping,
#'   coding_system = "ICD-10-CA",
#'   out_dir = tempdir()
#' )
#' names(res)
#'
#' @export
run_pipeline <- function(input_file,
                         col_mapping,
                         coding_system = c("ICD-10-CA", "ICD-10-CM", "ICD-11"),
                         out_dir = tempdir()) {

  coding_system <- match.arg(coding_system)

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  input_basename <- tools::file_path_sans_ext(basename(input_file))
  cleaned_path <- file.path(out_dir, paste0("input_data_cleaned_", input_basename, ".csv"))
  updated_path <- file.path(out_dir, paste0("updated_episodes_carry_forward_", input_basename, ".csv"))

  # 1) Create cleaned data
  cleaned_path <- Create_data(
    input_file,
    col_mapping = col_mapping,
    cleaned_path = cleaned_path
  )

  # 2) Carry-forward / chronic propagation
  updated_path <- chronic_pathologies(
    cleaned_path,
    updated_path = updated_path,
    coding_system = coding_system
  )

  # 3) Frailty
  frailty_results <- Frailty_Calculation(
    updated_path,
    coding_system = coding_system,
    out_dir = out_dir
  )

  fr_grouped <- frailty_results$fr_grouped
  fr_grouped_como <- frailty_results$fr_grouped_como

  # 4) Comorbidity + combined + final merge
  out <- Comorbidity_Frailty_Calculation(
    updated_path,
    fr_grouped,
    fr_grouped_como,
    coding_system = coding_system,
    out_dir = out_dir
  )

  out <- lapply(out, function(x) if (!is.data.frame(x)) as.data.frame(x) else x)
  out
}
