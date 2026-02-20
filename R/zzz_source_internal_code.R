#' @keywords internal
.LABTNSCPSS_source_internal <- function() {
  code_dir <- system.file("LABTNSCPSS_Code", package = "LABTNSCPSS")
  if (code_dir == "") {
    stop("Internal code directory not found. Please reinstall LABTNSCPSS.", call. = FALSE)
  }

  r_files <- list.files(code_dir, pattern = "\\.R$", full.names = TRUE)
  for (f in r_files) source(f, local = FALSE)
  invisible(TRUE)
}
