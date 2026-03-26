#' @noRd
.resolve_weight_map <- function(coding_system, index = c("elixhauser", "combined", "charlson")) {
  index <- match.arg(index)

  coding_system <- gsub("\\s+", "", coding_system)

  if (index == "elixhauser") {
    return(switch(coding_system,
                  "ICD-10-CA" = "elixhauser_icd10ca_labtns",
                  "ICD-10-CM" = "elixhauser_icd10_cm",
                  "ICD-11"    = "elixhauser_icd11",
                  stop("Unsupported coding_system: ", coding_system)
    ))
  }

  if (index == "combined") {
    return(switch(coding_system,
                  "ICD-10-CA" = "combined_icd10ca_labtns",
                  "ICD-10-CM" = "combined_icd10_cm",
                  "ICD-11"    = "combined_icd11",
                  stop("Unsupported coding_system: ", coding_system)
    ))
  }

  return(switch(coding_system,
                "ICD-10-CA" = "charlson_icd10ca_labtns",
                "ICD-10-CM" = "charlson_icd10_cm",
                "ICD-11"    = "charlson_icd11",
                stop("Unsupported coding_system: ", coding_system)
  ))
}
