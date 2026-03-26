#' Carry-forward chronic ICD codes and build updated episode-level ICD history
#'
#' This function assigns chronic categories to ICD codes, propagates chronic
#' conditions forward in time (category 2 indefinitely; category 1 within 1 year),
#' adds basal codes, and writes an updated episode file with `updated_icd_codes`.
#'
#' @param file_path_main Path to cleaned episode-level CSV (must include patient_id, episode_id, start_date, ICD).
#' @param updated_path Output CSV path. If NULL, a temp file is created.
#' @param coding_system One of "ICD-10-CA", "ICD-10-CM", "ICD-11".
#'
#' @return The output path (`updated_path`).
#' @export
chronic_pathologies <- function(file_path_main,
                                updated_path = NULL,
                                coding_system = c("ICD-10-CA", "ICD-10-CM", "ICD-11")) {

  coding_system <- match.arg(coding_system)

  if (!file.exists(file_path_main)) stop("File not found: ", file_path_main)

  if (is.null(updated_path)) {
    base <- tools::file_path_sans_ext(basename(file_path_main))
    updated_path <- file.path(tempdir(), paste0("updated_episodes_carry_forward_", base, ".csv"))
  }

  # Load the episodes data
  df <- data.table::fread(file_path_main, sep = ",", stringsAsFactors = FALSE)

  # ---------- helpers ----------
  normalize_codes <- function(x) toupper(gsub("\\.", "", trimws(as.character(x))))

  # ---------- Load categorisation table without polluting global env ----------
  env <- environment()

  if (coding_system == "ICD-10-CA") {
    data("ICD10CA_categorisation", package = "LABTNSCPSS", envir = env)
    codes_df <- get("ICD10CA_categorisation", envir = env)
    names(codes_df)[names(codes_df) == "ICD10CAcodes"] <- "ICD"
  } else if (coding_system == "ICD-10-CM") {
    data("ICD10CM_categorisation", package = "LABTNSCPSS", envir = env)
    codes_df <- get("ICD10CM_categorisation", envir = env)
    names(codes_df)[names(codes_df) == "CIM10CMcodes"] <- "ICD"
  } else { # ICD-11
    data("ICD11_categorisation", package = "LABTNSCPSS", envir = env)
    codes_df <- get("ICD11_categorisation", envir = env)
    names(codes_df)[names(codes_df) == "ICD11codes"] <- "ICD"
  }

  # Ensure strings + normalize ICD column in mapping
  codes_df$ICD <- normalize_codes(codes_df$ICD)

  # Sort episodes by patient_id and start date
  df <- df[order(df$patient_id, df$start_date), ]

  # Normalize ICD codes on patient side
  df <- dplyr::mutate(df, ICD = normalize_codes(ICD))

  # group per episode
  df_summary <- dplyr::summarise(
    dplyr::group_by(df, patient_id, start_date, episode_id),
    ICD = list(ICD),
    .groups = "drop"
  )

  # Build dictionary for exact mapping ICD -> category
  codes_dict <- data.table::data.table(
    ICD      = codes_df$ICD,
    category = as.character(codes_df$category)
  )
  codes_dict <- codes_dict[!is.na(ICD) & nzchar(ICD)]
  codes_dict <- unique(codes_dict, by = c("ICD", "category"))

  assign_categories <- function(icd_codes) {
    # icd_codes may be a vector or a 1-element character
    if (length(icd_codes) == 0) return(character(0))

    codes <- icd_codes
    if (length(codes) == 1L) {
      codes <- unlist(strsplit(as.character(codes), ",\\s*"))
    }
    if (!length(codes)) return(character(0))

    tmp <- data.table::data.table(ICD = normalize_codes(codes), ord = seq_along(codes))
    data.table::setkey(tmp, ICD)
    data.table::setkey(codes_dict, ICD)

    res <- codes_dict[tmp, on = .(ICD), nomatch = 0L][, .(ICD, category, ord)]

    out <- rep("None", length(codes))
    if (nrow(res)) out[res$ord] <- res$category
    out
  }

  df_summary$category_codes <- lapply(df_summary$ICD, assign_categories)

  # list -> character
  df_summary$category_codes <- vapply(df_summary$category_codes, function(x) paste(x, collapse = ", "), character(1))
  df_summary$ICD            <- vapply(df_summary$ICD,            function(x) paste(x, collapse = ", "), character(1))

  # Convert to data.table
  df <- data.table::as.data.table(df_summary)

  # ---------- Step 2: Category 2 ----------
  get_category2_codes <- function(episode_codes, category_codes) {
    icd_list <- unlist(strsplit(episode_codes, ",\\s*"))
    cat_list <- unlist(strsplit(category_codes, ",\\s*"))

    cat_list[cat_list == "None"] <- NA
    cat_list <- as.numeric(cat_list)

    icd_list[cat_list == 2]
  }

  df[, chronique_code_cat2 := mapply(get_category2_codes, ICD, category_codes)]

  clean_unique_codes <- function(codes) unique(stats::na.omit(codes))
  df[, cleaned_chronique_code_cat2 := lapply(chronique_code_cat2, clean_unique_codes)]

  # propagate cat2 forward indefinitely within patient
  df$cleaned_chronique_code_cat2 <- stats::ave(
    df$cleaned_chronique_code_cat2,
    df$patient_id,
    FUN = function(x) {
      vapply(seq_along(x), function(i) {
        paste(unique(stats::na.omit(x[1:i])), collapse = ",")
      }, character(1))
    }
  )

  clean_and_extract_values <- function(x) {
    x <- gsub('^character\\(0\\)', '', x)
    x <- trimws(x)

    if (grepl('^c\\(', x)) {
      x <- gsub('^c\\(\\s*\"|\"\\s*\\)$', '', x)
      x <- gsub('","', ',', x)
    }

    x <- gsub('^"|"$', '', x)
    x <- trimws(x)

    values <- unlist(strsplit(x, ","))
    values <- trimws(values)
    values <- values[values != ""]

    paste(unique(values), collapse = ",")
  }

  df[, cleaned_chronique_code_cat2 := vapply(cleaned_chronique_code_cat2, clean_and_extract_values, character(1))]

  # ---------- Step 3: Category 1 ----------
  get_category_codes <- function(cim, category_codes, target_category) {
    icd_list <- unlist(strsplit(cim, ",\\s*"))
    cat_list <- unlist(strsplit(category_codes, ",\\s*"))

    cat_list[cat_list == "None"] <- NA
    cat_list <- as.numeric(cat_list)

    icd_list[cat_list == target_category]
  }

  df[, chronique_code_cat1 := mapply(get_category_codes, ICD, category_codes, target_category = 1)]
  df[, cleaned_chronique_code_cat1 := lapply(chronique_code_cat1, clean_unique_codes)]

  # robust date parsing
  df$start_date <- vapply(df$start_date, function(x) {
    x <- as.character(x)

    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x) || grepl(" ", x)) return(x)

    # try YYYYMMDD
    date_parsed <- lubridate::ymd(paste0(
      substring(x, 1, 4), "-", substring(x, 5, 6), "-", substring(x, 7, 8)
    ))

    if (is.na(date_parsed)) {
      warning(paste("Failed to parse date:", x))
      return(NA_character_)
    }
    as.character(date_parsed)
  }, character(1))

  df$start_date <- as.Date(df$start_date)

  # add ICD codes in category 1 to following 1 year
  df_combined <- df %>%
    dplyr::group_by(patient_id) %>%
    dplyr::arrange(start_date) %>%
    dplyr::mutate(
      cleaned_chronique_code_cat1 = sapply(seq_along(start_date), function(i) {
        codes_within_year <- cleaned_chronique_code_cat1[
          start_date <= start_date[i] & start_date >= start_date[i] - 365
        ]
        paste(stats::na.omit(unique(c(codes_within_year))), collapse = ", ")
      })
    ) %>%
    dplyr::ungroup()

  # Convert list columns to character strings WITHOUT using '.' placeholder
  df_combined <- df_combined %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.list),
        function(col) vapply(col, function(x) paste(unlist(x), collapse = ","), character(1))
      )
    )

  # ---------- Step 4: Basal codes ----------
  if (coding_system == "ICD-10-CA") {
    data("Basal_Codes", package = "LABTNSCPSS", envir = env)
    main_dt <- get("Basal_Codes", envir = env)
    names(main_dt)[names(main_dt) == "B_ICD10CAcodesBasale"] <- "B_ICD"
    names(main_dt)[names(main_dt) == "Comp_ICD10CAcodes"] <- "Comp_ICD"
  } else if (coding_system == "ICD-10-CM") {
    data("ICD10CM_Basal_Codes", package = "LABTNSCPSS", envir = env)
    main_dt <- get("ICD10CM_Basal_Codes", envir = env)
    names(main_dt)[names(main_dt) == "B_CIM10CMcodesbasale"] <- "B_ICD"
    names(main_dt)[names(main_dt) == "Comp_CIM10CMcodes"] <- "Comp_ICD"
  } else {
    data("ICD11_Basal_Codes", package = "LABTNSCPSS", envir = env)
    main_dt <- get("ICD11_Basal_Codes", envir = env)
    names(main_dt)[names(main_dt) == "B_CIM11Basale"] <- "B_ICD"
    names(main_dt)[names(main_dt) == "Comp_CIM11codes"] <- "Comp_ICD"
  }

  find_basal_codes <- function(code_list, main_dt) {
    tmp <- main_dt[main_dt$Comp_ICD %in% code_list, ]
    if (nrow(tmp) == 0) return(NA)
    unique(tmp$B_ICD)
  }

  if (!data.table::is.data.table(df_combined)) df_combined <- data.table::as.data.table(df_combined)

  clean_code_string <- function(x) {
    if (is.na(x) || x == "" || x == "None") return(character(0))
    x <- gsub("c\\(|\\)", "", x)
    x <- gsub("\\\\\"", "", x)
    x <- gsub("\"", "", x)
    x <- gsub(",\\s*,", ",", x)
    x <- gsub("^,|,$", "", x)

    codes <- unlist(strsplit(x, ","))
    codes <- trimws(codes)
    codes[codes != ""]
  }

  df_combined[, cleaned_chronique_code_cat1 := lapply(cleaned_chronique_code_cat1, clean_code_string)]

  df_combined[, basal_codes := vapply(cleaned_chronique_code_cat1, function(x) {
    paste(find_basal_codes(x, main_dt), collapse = ",")
  }, character(1))]

  clean_value <- function(x) {
    x <- gsub('^c\\(\\s*\"|\"\\s*\\)$', '', x)
    x <- gsub('^\\s*character\\(0\\)\\s*', '', x)
    x <- gsub('^\\s*\"\\s*|\\s*\"\\s*$', '', x)
    x <- gsub('[\\s*\"\\(\\)]', '', x)
    paste(trimws(unlist(strsplit(x, ","))), collapse = ",")
  }

  # propagate cat2 + basal across episodes per patient (keep your logic)
  df_combined$cleaned_chronique_code_cat2 <- stats::ave(
    df_combined$patient_id,
    df_combined$patient_id,
    FUN = function(pid_block) {

      idx <- which(df_combined$patient_id == pid_block[1])

      vapply(seq_along(idx), function(i) {
        rows <- idx[1:i]
        merged <- unique(c(
          unlist(df_combined$cleaned_chronique_code_cat2[rows]),
          unlist(df_combined$basal_codes[rows])
        ))
        paste(unique(stats::na.omit(merged)), collapse = ",")
      }, character(1))
    }
  )

  df_combined[, cleaned_chronique_code_cat2 := vapply(cleaned_chronique_code_cat2, clean_and_extract_values, character(1))]

  df_combined[, cleaned_chronique_code_cat1 := vapply(cleaned_chronique_code_cat1, clean_value, character(1))]
  df_combined[, basal_codes := vapply(basal_codes, clean_value, character(1))]
  df_combined[, ICD := vapply(ICD, clean_value, character(1))]

  # ---------- Step 5: combine updated codes ----------
  df_combined[, updated_icd_codes := paste(cleaned_chronique_code_cat1, cleaned_chronique_code_cat2, basal_codes, ICD, sep = ",")]

  clean_and_concatenate <- function(...) {
    combined <- unlist(list(...))
    cleaned <- stats::na.omit(combined)
    cleaned <- trimws(cleaned)
    cleaned <- cleaned[cleaned != ""]
    cleaned <- gsub('\\bc', '', cleaned)
    cleaned <- trimws(cleaned)
    paste(unique(cleaned), collapse = ", ")
  }

  df_combined[, updated_icd_codes := apply(
    df_combined[, .(cleaned_chronique_code_cat1, cleaned_chronique_code_cat2, basal_codes, ICD)],
    1,
    function(row) clean_and_concatenate(row)
  )]

  # final cleanup of updated_icd_codes
  df_combined[, updated_icd_codes := vapply(updated_icd_codes, function(x) {

    x <- as.character(x)
    x <- gsub("character\\(0\\)", "", x)
    x <- gsub("^c\\(|\\)$", "", x)
    x <- gsub("\\)|\\(", "", x)
    x <- gsub('\\"', "", x)
    x <- gsub('"', "", x)
    x <- gsub("\\s+", " ", x)

    v <- unlist(strsplit(x, ","))
    v <- trimws(v)
    v <- v[!is.na(v)]
    v <- v[v != "" & v != "NA" & v != "None"]
    v <- unique(v)

    paste(v, collapse = ",")
  }, character(1))]

  utils::write.csv(df_combined, file = updated_path, row.names = FALSE)
  updated_path
}
