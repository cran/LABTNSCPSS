# zzz.R
# Global variable declarations to satisfy R CMD check (NSE, data.table, dplyr)

.onLoad <- function(libname, pkgname) {
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(

      # magrittr / dplyr placeholders
      ".", ".data",

      # Core identifiers
      "id", "code", "patient_id", "episode_id", "start_date",
      "updated_icd_codes", "row_id", "present",

      # ICD variables
      "ICD", "frailty_category", "MorbiFrailtyCategory",
      "category", "category_codes", "ord",

      # Chronic pathology columns
      "chronique_code_cat1", "chronique_code_cat2",
      "cleaned_chronique_code_cat1", "cleaned_chronique_code_cat2",
      "basal_codes",

      # Frailty / Morbi-frailty flags
      "DiabNC", "DiabC", "HBPNoComp", "HBPComp",
      "Frailty_labtns_cpss", "Morbi_frailty_labtns_cpss"

    ))
  }
}
