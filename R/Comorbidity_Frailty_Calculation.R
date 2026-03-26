Comorbidity_Frailty_Calculation <- function(file_path_main,
                                            fr_grouped,
                                            fr_grouped_como,
                                            coding_system = c("ICD-10-CA","ICD-10-CM","ICD-11"),
                                            out_dir = tempdir()) {

  coding_system <- match.arg(coding_system)
  if (!file.exists(file_path_main)) stop("File not found: ", file_path_main)

  # Ensure out_dir exists
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  base <- tools::file_path_sans_ext(basename(file_path_main))

  # Read updated episodes
  X_CHUM <- data.table::fread(file_path_main, sep = ",")

  # Build long comorbidity input
  df_combined <- data.frame(
    id         = paste(X_CHUM$patient_id, X_CHUM$episode_id, X_CHUM$start_date, sep = "_"),
    code       = as.character(X_CHUM$updated_icd_codes),
    start_date = X_CHUM$start_date,
    episode_id = X_CHUM$episode_id,
    stringsAsFactors = FALSE
  )

  # Split codes into rows
  df_cleaned <- tidyr::separate_rows(df_combined, code, sep = ",")
  df_cleaned$code <- trimws(df_cleaned$code)
  df_cleaned <- df_cleaned[!is.na(df_cleaned$code) & df_cleaned$code != "" & df_cleaned$code != "NA", , drop = FALSE]

  df_comrbidity <- df_cleaned[, c("id","code","start_date","episode_id")]

  # Choose mappings
  if (coding_system == "ICD-10-CA") {
    mapping_Elix     <- "elixhauser_icd10ca_labtns"
    mapping_Ch       <- "charlson_icd10ca_labtns"
    mapping_combined <- "combined_icd10ca_labtns"
  } else if (coding_system == "ICD-10-CM") {
    mapping_Elix     <- "elixhauser_icd10_cm"
    mapping_Ch       <- "charlson_icd10_cm"
    mapping_combined <- "combined_icd10_cm"
  } else {
    mapping_Elix     <- "elixhauser_icd11"
    mapping_Ch       <- "charlson_icd11"
    mapping_combined <- "combined_icd11"
  }

  # --- Elixhauser
  elix <- LABTNSCPSS::comorbidity(
    x = df_comrbidity, id = "id", code = "code", map = mapping_Elix, assign0 = FALSE
  )

  # Ensure missing columns exist (some maps might not include these)
  if (!"Diab_NC" %in% names(elix)) elix$Diab_NC <- 0L
  if (!"Diab_C"  %in% names(elix)) elix$Diab_C  <- 0L

  # Coerce NA -> 0
  elix[] <- lapply(elix, function(x) { x[is.na(x)] <- 0L; x })

  # Exclusivity rule + score
  elix$Diab_NC <- ifelse(elix$Diab_NC == 1L & elix$Diab_C == 1L, 0L, elix$Diab_NC)
  elix$Elixhauser_labtns_cpss <- rowSums(elix[, setdiff(names(elix), "id"), drop = FALSE], na.rm = TRUE)

  # Split id back
  elix_split <- tidyr::separate(
    data.frame(id = elix$id),
    id,
    into = c("patient_id","episode_id","start_date"),
    sep = "_",
    remove = TRUE
  )

  final_data_elixhauser <- cbind(elix_split, elix[, setdiff(names(elix), "id"), drop = FALSE])

  # Write Elix CSV
  file_path <- file.path(out_dir, glue::glue("ECI_Labtns_cpss_{coding_system}_{base}.csv"))
  utils::write.csv(final_data_elixhauser, file = file_path, row.names = FALSE)

  # Optional weighted score (only for ICD-10-CA mapping)
  score_pop_Elixh <- NULL
  if (mapping_Elix == "elixhauser_icd10ca_labtns") {

    # Keep the comorbidity object (class + attr(map)) for score()
    elix_for_score <- elix

    # Remove extra computed column before score() (score() expects only comorbidity vars)
    if ("Elixhauser_labtns_cpss" %in% names(elix_for_score)) {
      elix_for_score$Elixhauser_labtns_cpss <- NULL
    }

    score_pop_Elixh <- LABTNSCPSS::score(
      x = elix_for_score,
      weights = "readmission_elix_hcup",
      assign0 = FALSE
    )

    # Recommended: save weighted score as id + score
    score_pop_Elixh_df <- data.frame(
      id = elix$id,
      readmission_elix_hcup = as.numeric(score_pop_Elixh),
      stringsAsFactors = FALSE
    )

    utils::write.csv(
      score_pop_Elixh_df,
      file = file.path(out_dir, glue::glue("ECI_weighted_readmission_{coding_system}_{base}.csv")),
      row.names = FALSE
    )
  }

  # --- Charlson
  ch <- LABTNSCPSS::comorbidity(
    x = df_comrbidity, id = "id", code = "code", map = mapping_Ch, assign0 = FALSE
  )

  if (!"Diab_NC" %in% names(ch)) ch$Diab_NC <- 0L
  if (!"Diab_C"  %in% names(ch)) ch$Diab_C  <- 0L
  ch[] <- lapply(ch, function(x) { x[is.na(x)] <- 0L; x })
  ch$Diab_NC <- ifelse(ch$Diab_NC == 1L & ch$Diab_C == 1L, 0L, ch$Diab_NC)

  ch$Charlson_labtns_cpss <- rowSums(ch[, setdiff(names(ch), "id"), drop = FALSE], na.rm = TRUE)

  ch_split <- tidyr::separate(
    data.frame(id = ch$id),
    id,
    into = c("patient_id","episode_id","start_date"),
    sep = "_",
    remove = TRUE
  )

  final_data_charlson <- cbind(ch_split, ch[, setdiff(names(ch), "id"), drop = FALSE])

  file_path <- file.path(out_dir, glue::glue("CCI_Labtns_cpss_{coding_system}_{base}.csv"))
  utils::write.csv(final_data_charlson, file = file_path, row.names = FALSE)

  # --- Combined
  cmb <- LABTNSCPSS::comorbidity(
    x = df_comrbidity, id = "id", code = "code", map = mapping_combined, assign0 = FALSE
  )

  if (!"Diab_NC" %in% names(cmb)) cmb$Diab_NC <- 0L
  if (!"Diab_C"  %in% names(cmb)) cmb$Diab_C  <- 0L
  cmb[] <- lapply(cmb, function(x) { x[is.na(x)] <- 0L; x })
  cmb$Diab_NC <- ifelse(cmb$Diab_NC == 1L & cmb$Diab_C == 1L, 0L, cmb$Diab_NC)

  cmb$Combined_comorb_labtns_cpss <- rowSums(cmb[, setdiff(names(cmb), "id"), drop = FALSE], na.rm = TRUE)

  cmb_split <- tidyr::separate(
    data.frame(id = cmb$id),
    id,
    into = c("patient_id","episode_id","start_date"),
    sep = "_",
    remove = TRUE
  )

  final_data_combined <- cbind(cmb_split, cmb[, setdiff(names(cmb), "id"), drop = FALSE])

  file_path <- file.path(out_dir, glue::glue("Combined_Comorb_Labtns_cpss_{coding_system}_{base}.csv"))
  utils::write.csv(final_data_combined, file = file_path, row.names = FALSE)

  # --- Combine final scores with frailty tables (use base merge for robustness)
  # Ensure character keys
  final_data_elixhauser$patient_id <- as.character(final_data_elixhauser$patient_id)
  final_data_elixhauser$episode_id <- as.character(final_data_elixhauser$episode_id)
  final_data_elixhauser$start_date <- as.character(final_data_elixhauser$start_date)

  final_data_charlson$patient_id <- as.character(final_data_charlson$patient_id)
  final_data_charlson$episode_id <- as.character(final_data_charlson$episode_id)
  final_data_charlson$start_date <- as.character(final_data_charlson$start_date)

  final_data_combined$episode_id <- as.character(final_data_combined$episode_id)

  fr_grouped$patient_id <- as.character(fr_grouped$patient_id)
  fr_grouped$episode_id <- as.character(fr_grouped$episode_id)
  fr_grouped$start_date <- as.character(fr_grouped$start_date)

  fr_grouped_como$patient_id <- as.character(fr_grouped_como$patient_id)
  fr_grouped_como$episode_id <- as.character(fr_grouped_como$episode_id)
  fr_grouped_como$start_date <- as.character(fr_grouped_como$start_date)

  scores_final <- final_data_charlson[, c("patient_id","episode_id","start_date","Charlson_labtns_cpss"), drop = FALSE]
  scores_final <- merge(
    scores_final,
    final_data_elixhauser[, c("patient_id","episode_id","start_date","Elixhauser_labtns_cpss"), drop = FALSE],
    by = c("patient_id","episode_id","start_date"),
    all.x = TRUE
  )

  scores_final <- merge(
    scores_final,
    final_data_combined[, c("episode_id","Combined_comorb_labtns_cpss"), drop = FALSE],
    by = "episode_id",
    all.x = TRUE
  )

  scores_final <- merge(
    scores_final,
    fr_grouped[, c("patient_id","episode_id","start_date","Frailty_labtns_cpss"), drop = FALSE],
    by = c("patient_id","episode_id","start_date"),
    all.x = TRUE
  )

  scores_final <- merge(
    scores_final,
    fr_grouped_como[, c("patient_id","episode_id","start_date","Morbi_frailty_labtns_cpss"), drop = FALSE],
    by = c("patient_id","episode_id","start_date"),
    all.x = TRUE
  )

  file_path <- file.path(out_dir, glue::glue("Final_scores_comorbidity_frailty_Labtns_cpss_{coding_system}_{base}.csv"))
  utils::write.csv(scores_final, file = file_path, row.names = FALSE)

  return(list(
    scores_final = scores_final,
    final_data_combined = final_data_combined,
    final_data_charlson = final_data_charlson,
    final_data_elixhauser = final_data_elixhauser,
    score_pop_Elixh = score_pop_Elixh
  ))
}
