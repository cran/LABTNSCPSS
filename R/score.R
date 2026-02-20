#' @title Compute (weighted) comorbidity scores
#'
#' @param x An object of class `comorbidity` returned by a call to \code{\link{comorbidity}()}.
#' @param weights A string denoting the weighting system to be used (depends on the mapping algorithm).
#'
#' Possible values for the Charlson index are:
#' * `charlson`, for the original weights by Charlson et al. (1987);
#' * `quan`, for the revised weights by Quan et al. (2011).
#'
#' Possible values for the Elixhauser score are:
#' * `vw`, for the weights by van Walraven et al. (2009);
#' * `swiss`, for the Swiss Elixhauser weights by Sharma et al. (2021).
#'
#' Defaults to `NULL`, in which case an unweighted score will be used.
#'
#' @param assign0 A logical value indicating whether to apply a hierarchy of comorbidities (so milder forms are set to 0).
#'
#' @return A numeric vector with the (possibly weighted) comorbidity score for each subject.
#'
#' @references
#' Charlson ME, Pompei P, Ales KL, et al. _A new method of classifying prognostic comorbidity in longitudinal studies: development and validation_. Journal of Chronic Diseases 1987; 40:373-383.
#'
#' @references
#' Quan H, Li B, Couris CM, et al. _Updating and validating the Charlson Comorbidity Index and Score for risk adjustment in hospital discharge abstracts using data from 6 countries_. American Journal of Epidemiology 2011; 173(6):676-682.
#'
#' @references
#' van Walraven C, Austin PC, Jennings A, Quan H and Forster AJ. _A modification of the Elixhauser comorbidity measures into a point system for hospital death using administrative data_. Medical Care 2009; 47(6):626-633.
#'
#' @references
#' Sharma N, Schwendimann R, Endrich O, et al. _Comparing Charlson and Elixhauser comorbidity indices with different weightings to predict in-hospital mortality: an analysis of national inpatient data_. BMC Health Services Research 2021; 21(13).
#'
#' @export
#'
#' @examples
#' # IMPORTANT: these examples must be runnable during R CMD check.
#' # If `x` is not defined in examples, create a tiny reproducible dataset here.
#' x <- data.frame(id = c(1, 1, 2), code = c("I10", "E119", "J449"), stringsAsFactors = FALSE)
#'
#' # Checking the `assign0` argument:
#' x3 <- data.frame(id = 1, code = c("E100", "E102"), stringsAsFactors = FALSE)
#' ccF <- comorbidity(x = x3, id = "id", code = "code", map = "charlson_icd10_quan", assign0 = FALSE)
#' score(x = ccF, assign0 = FALSE)
#' score(x = ccF, assign0 = TRUE)
score <- function(x, weights = NULL, assign0) {
  if (!inherits(x = x, what = "comorbidity")) {
    stop(
      "This function can only be used on an object of class 'comorbidity', ",
      "which you can obtain by using the 'comorbidity()' function. See ?comorbidity for more details.",
      call. = FALSE
    )
  }

  map <- attr(x, "map")

  arg_checks <- checkmate::makeAssertCollection()
  checkmate::assert_class(x, classes = "comorbidity", add = arg_checks)
  checkmate::assert_string(weights, null.ok = TRUE, add = arg_checks)

  if (!is.null(weights)) {
    weights <- stringi::stri_trans_tolower(weights)
  }
  checkmate::assert_choice(weights, choices = names(.weights[[map]]), null.ok = TRUE, add = arg_checks)

  checkmate::assert_logical(assign0, add = arg_checks)
  if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

  if (is.null(weights)) {
    ww <- rep(1, length(.maps[[map]]))
    names(ww) <- names(.maps[[map]])
  } else {
    ww <- .weights[[map]][[weights]]
  }
  ww <- matrix(data = ww, ncol = 1)

  x <- x[, names(.maps[[map]])]
  if (assign0) {
    data.table::setDT(x)
    x <- .assign0(x = x, map = map)
    data.table::setDF(x)
  }

  out <- as.matrix(x) %*% ww
  out <- drop(out)
  attr(out, "map") <- map
  attr(out, "weights") <- weights
  out
}
