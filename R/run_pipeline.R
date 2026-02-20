#' Run the LABTNSCPSS comorbidity + frailty pipeline
#'
#' Runs data cleaning, chronic pathology propagation, frailty calculation,
#' and comorbidity/frailty scoring.
#'
#' @param input_file Path to the input CSV file.
#' @param col_mapping Named list mapping standard names to the user's column names.
#' @param coding_system Coding system used (e.g., "ICD-10-CA"). Optional; used by internal functions when applicable.
#' @param out_dir Output directory for generated files. If NULL, writes next to `input_file`.
#'
#' @return A named list of `data.frame`s returned by `Comorbidity_Frailty_Calculation()`.
#'   Elements typically include comorbidity and frailty summary tables.
#'
#' @export
run_pipeline <- function(input_file,
                         col_mapping,
                         coding_system = NULL,
                         out_dir = NULL) {

  .LABTNSCPSS_source_internal()

  # Keep outputs consistent with Create_data() default behavior
  if (is.null(out_dir)) out_dir <- dirname(input_file)

  # If internal scripts expect a global 'coding_system'
  #if (!is.null(coding_system)) assign("coding_system", coding_system, envir = .GlobalEnv)
  if (!is.null(coding_system)) options(LABTNSCPSS.coding_system = coding_system)

  input_basename <- tools::file_path_sans_ext(basename(input_file))
  cleaned_path <- file.path(out_dir, paste0("input_data_cleaned_", input_basename, ".csv"))
  updated_path <- file.path(out_dir, paste0("updated_episodes_carry_forward_", input_basename, ".csv"))

  Create_data(input_file, col_mapping = col_mapping)
  chronic_pathologies(cleaned_path)

  frailty_results <- Frailty_Calculation(updated_path)
  fr_grouped <- frailty_results$fr_grouped
  fr_grouped_como <- frailty_results$fr_grouped_como

  res <- Comorbidity_Frailty_Calculation(updated_path, fr_grouped, fr_grouped_como)
  res <- lapply(res, function(x) if (!is.data.frame(x)) as.data.frame(x) else x)
  res
}
