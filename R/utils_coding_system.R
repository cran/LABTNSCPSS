#' Choose a coding system
#'
#' @param coding_system Character string. If NULL and interactive(), the user is prompted.
#' @param valid_systems Character vector of allowed coding systems.
#' @param verbose Logical; if TRUE, prints guidance using message().
#'
#' @return A single character string indicating the selected coding system.
#' @export
get_coding_system <- function(coding_system = NULL,
                              valid_systems = c("ICD-10-CA", "ICD-10-CM", "ICD-11"),
                              verbose = interactive()) {

  # If provided, validate and return (no printing)
  if (!is.null(coding_system)) {
    coding_system <- trimws(coding_system)
    if (!coding_system %in% valid_systems) {
      stop("Invalid coding_system. Choose one of: ",
           paste(valid_systems, collapse = ", "),
           call. = FALSE)
    }
    return(coding_system)
  }

  # Only prompt in interactive use
  if (!interactive()) {
    stop("coding_system must be provided in non-interactive use. Choose one of: ",
         paste(valid_systems, collapse = ", "),
         call. = FALSE)
  }

  repeat {
    user_input <- trimws(readline(
      prompt = paste0("Enter coding system (", paste(valid_systems, collapse = ", "), "): ")
    ))

    if (user_input %in% valid_systems) return(user_input)

    if (verbose) {
      message("Invalid input. Please choose from: ", paste(valid_systems, collapse = ", "))
    }
  }
}
