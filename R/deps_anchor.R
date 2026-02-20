# This function exists only to mark Imports as "used" for R CMD check,
# because core code is sourced from LABTNSCPSS_Code at runtime.

.LABTNSCPSS_deps_anchor <- function() {
  if (FALSE) {
    dplyr::mutate
    dplyr::summarise
    dplyr::left_join
    lubridate::ymd
    data.table::fread
    purrr::map
  }
  invisible(NULL)
}
