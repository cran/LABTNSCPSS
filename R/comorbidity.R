#' Comorbidity mapping
#'
#' Maps ICD diagnostic codes to comorbidity categories using supported algorithms.
#'
#' @param x A data.frame (or data.table/tibble) containing at least an ID column and a code column.
#' @param id Name of the patient identifier column in `x`.
#' @param code Name of the diagnostic code column in `x`.
#' @param map Name of the mapping algorithm to use.
#' @param assign0 Logical; if TRUE, applies hierarchy rules to avoid double-counting.
#' @param labelled Logical; if TRUE, adds labels for easier viewing.
#' @param tidy.codes Logical; if TRUE, cleans codes before mapping.
#'
#' @return A data.frame with comorbidity indicator columns and class `"comorbidity"`.
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'   id = c(1, 1, 2, 2),
#'   code = c("I10", "E119", "F200", "E110"),
#'   stringsAsFactors = FALSE
#' )
#'
#' comorbidity(x = x, id = "id", code = "code",
#'             map = "charlson_icd10_quan", assign0 = FALSE)
#'
#' comorbidity(x = x, id = "id", code = "code",
#'             map = "elixhauser_icd10_quan", assign0 = FALSE)
comorbidity <- function(x, id, code, map, assign0, labelled = TRUE, tidy.codes = TRUE) {
  ...
}

comorbidity <- function(x, id, code, map, assign0, labelled = TRUE, tidy.codes = TRUE) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()
  # x must be a data.frame (or a data.table)
  checkmate::assert_multi_class(x, classes = c("data.frame", "data.table", "tbl", "tbl_df"), add = arg_checks)
  # id, code, map must be a single string value
  checkmate::assert_string(id, add = arg_checks)
  checkmate::assert_string(code, add = arg_checks)
  checkmate::assert_string(map, add = arg_checks)
  # map must be one of the supported; case insensitive
  map <- tolower(map)
  checkmate::assert_choice(map, choices = names(.maps), add = arg_checks)
  # assign0, labelled, tidy.codes must be a single boolean value
  checkmate::assert_logical(assign0, add = arg_checks)
  checkmate::assert_logical(labelled, len = 1, add = arg_checks)
  checkmate::assert_logical(tidy.codes, len = 1, add = arg_checks)
  # force names to be syntactically valid:
  if (any(names(x) != make.names(names(x)))) {
    names(x) <- make.names(names(x))
    warning("Names of the input dataset 'x' have been modified by make.names(). See ?make.names() for more details.", call. = FALSE)
  }
  if (id != make.names(id)) {
    id <- make.names(id)
    warning("The input 'id' string has been modified by make.names(). See ?make.names() for more details.", call. = FALSE)
  }
  if (code != make.names(code)) {
    code <- make.names(code)
    warning("The input 'id' string has been modified by make.names(). See ?make.names() for more details.", call. = FALSE)
  }
  # id, code must be in x
  checkmate::assert_subset(id, choices = names(x), add = arg_checks)
  checkmate::assert_subset(code, choices = names(x), add = arg_checks)
  # Report if there are any errors
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  ### Tidy codes if required
  if (tidy.codes) x <- .tidy(x = x, code = code)

  ### Create regex from a list of codes
  regex <- lapply(X = .maps[[map]], FUN = .codes_to_regex)

  ### Subset only 'id' and 'code' columns
  if (data.table::is.data.table(x)) {
    mv <- c(id, code)
    x <- x[, ..mv]
  } else {
    x <- x[, c(id, code)]
  }

  ### Turn x into a DT
  data.table::setDT(x)

  ### Match comorbidity mapping
  x <- .matchit(x = x, id = id, code = code, regex = regex)

  ### Assign zero-values to avoid double-counting comorbidities, if requested
  if (assign0) {
    x <- .assign0(x = x, map = map)
  }

  ### Turn internal DT into a DF
  data.table::setDF(x)

  ### Check output for possible unknown-state errors
  .check_output(x = x, id = id)

  ### Label variables for RStudio viewer if requested
  if (labelled) x <- .labelled(x = x, map = map)

  ### Return it, after adding class 'comorbidity' and some attributes
  class(x) <- c("comorbidity", class(x))
  attr(x = x, which = "map") <- map
  return(x)
}

