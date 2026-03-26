Frailty_Calculation <- function(file_path_main,
                                coding_system = c("ICD-10-CA","ICD-10-CM","ICD-11"),
                                out_dir = tempdir()) {

  coding_system <- match.arg(coding_system)

  if (!file.exists(file_path_main)) stop("File not found: ", file_path_main)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  base <- tools::file_path_sans_ext(basename(file_path_main))

  file_path1 <- file.path(out_dir, paste0("Frailty_Labtns_cpss_", coding_system, "_", base, ".csv"))
  file_path2 <- file.path(out_dir, paste0("Morbi-frailty_Labtns_cpss_", coding_system, "_", base, ".csv"))

  data.table::setDTthreads(percent = 100)

  # ---------- helpers ----------
  normalize_codes <- function(x) toupper(gsub("\\.", "", trimws(as.character(x))))

  # ---------- input ----------
  df <- data.table::fread(file_path_main, sep = ",", encoding = "UTF-8", showProgress = FALSE)
  data.table::setDT(df)

  # required cols
  req_cols <- c("patient_id","start_date","episode_id","updated_icd_codes")
  miss <- setdiff(req_cols, names(df))
  if (length(miss)) stop("Missing columns in input CSV: ", paste(miss, collapse=", "))

  # stable types for keys
  df[, patient_id := as.character(patient_id)]
  df[, episode_id := as.character(episode_id)]
  df[, updated_icd_codes := as.character(updated_icd_codes)]

  # ---------- mapping tables (frailty: df_fr) ----------
  if (coding_system == "ICD-10-CA") {
    data("Frailty_mapping", package = "LABTNSCPSS", envir = environment())
    Frailty_mapping <- get("Frailty_mapping", envir = environment())
    df_fr <- data.table::as.data.table(Frailty_mapping)
    data.table::setnames(
      df_fr,
      c("ICD_10_CA_LabTNS","frailty_Comorbidity"),
      c("ICD","frailty_category")
    )
  } else if (coding_system == "ICD-10-CM") {
    data("Frailty_ICD10CM", package = "LABTNSCPSS", envir = environment())
    Frailty_ICD10CM <- get("Frailty_ICD10CM", envir = environment())
    df_fr <- data.table::as.data.table(Frailty_ICD10CM)
    data.table::setnames(
      df_fr,
      c("CIM10CMcodes","frailty_Comorbidity"),
      c("ICD","frailty_category")
    )
  } else { # ICD-11
    data("Frailty_ICD11", package = "LABTNSCPSS", envir = environment())
    Frailty_ICD11 <- get("Frailty_ICD11", envir = environment())
    df_fr <- data.table::as.data.table(Frailty_ICD11)
    data.table::setnames(
      df_fr,
      c("ICD11codes","frailty_Comorbidity"),
      c("ICD","frailty_category")
    )
  }

  df_fr[, ICD := normalize_codes(ICD)]
  df_fr[, frailty_category := as.character(frailty_category)]
  unique_categories <- unique(df_fr$frailty_category)

  # ---------- mapping tables (morbi-frailty: df_fr_mo) ----------
  data("Frailty_Comorbidity_Mapping", package = "LABTNSCPSS", envir = environment())
  Frailty_Comorbidity_Mapping <- get("Frailty_Comorbidity_Mapping", envir = environment())
  df_fr_mo <- data.table::as.data.table(Frailty_Comorbidity_Mapping)

  if (coding_system == "ICD-10-CA") {
    data.table::setnames(df_fr_mo, "ICD_10_CA_Codes", "ICD")
  } else if (coding_system == "ICD-10-CM") {
    data.table::setnames(df_fr_mo, "CIM10CMcodes", "ICD")
  } else {
    data.table::setnames(df_fr_mo, "ICD11codes", "ICD")
  }

  df_fr_mo[, ICD := normalize_codes(ICD)]
  df_fr_mo[, MorbiFrailtyCategory := as.character(MorbiFrailtyCategory)]
  unique_categories_FM <- unique(df_fr_mo$MorbiFrailtyCategory)

  # ---------- split updated_icd_codes once (vectorized) ----------
  df[, row_id := .I]

  codes_list <- strsplit(df$updated_icd_codes, ",", fixed = TRUE)
  codes_list <- lapply(codes_list, function(v) {
    if (is.null(v)) return(character(0))
    v <- normalize_codes(v)
    v <- v[!(v %in% c("", "NA", "NAN", "NULL"))]
    unique(v)
  })

  total_codes <- sum(lengths(codes_list))
  if (total_codes == 0L) {
    fr_grouped <- df[, .(patient_id, start_date, episode_id)]
    for (cat in unique_categories) fr_grouped[, (cat) := 0L]
    fr_grouped[, Frailty_labtns_cpss := 0L]

    fr_grouped_como <- df[, .(patient_id, start_date, episode_id)]
    for (cat in unique_categories_FM) fr_grouped_como[, (cat) := 0L]
    fr_grouped_como[, Morbi_frailty_labtns_cpss := 0L]

    data.table::fwrite(fr_grouped, file_path1)
    data.table::fwrite(fr_grouped_como, file_path2)

    return(list(
      fr_grouped = tibble::as_tibble(fr_grouped),
      fr_grouped_como = tibble::as_tibble(fr_grouped_como)
    ))
  }

  # long table with minimal columns (saves RAM)
  codes_long <- data.table::data.table(
    row_id = rep.int(df$row_id, lengths(codes_list)),
    code   = unlist(codes_list, use.names = FALSE)
  )

  # ---------- function: map codes to categories via exact match ----------
  map_to_indicators <- function(map_dt, cat_col, nrows) {

    data.table::setDT(map_dt)
    map_dt[, ICD := normalize_codes(ICD)]
    map_dt[, category := as.character(map_dt[[cat_col]])]
    map_dt <- unique(map_dt[, .(ICD, category)])

    data.table::setkey(codes_long, code)
    data.table::setkey(map_dt, ICD)

    exact_hits <- map_dt[codes_long, on = .(ICD = code), nomatch = 0L][
      , .(row_id, category)
    ]

    if (nrow(exact_hits)) {
      exact_hits <- unique(exact_hits)[, present := 1L]
      ind_wide <- data.table::dcast(
        exact_hits,
        row_id ~ category,
        value.var = "present",
        fun.aggregate = function(x) as.integer(length(x) > 0L),
        fill = 0L
      )
    } else {
      ind_wide <- data.table::data.table(row_id = seq_len(nrows))
    }

    data.table::setDT(ind_wide)
    ind_wide
  }

  # ---------- FRAILTY ----------
  ind_fr <- map_to_indicators(
    map_dt = df_fr[, .(ICD, frailty_category)],
    cat_col = "frailty_category",
    nrows = nrow(df)
  )

  data.table::setkey(ind_fr, row_id)
  data.table::setkey(df, row_id)
  frailty_pop2 <- ind_fr[df]  # keep all rows

  for (cat in unique_categories) if (!cat %in% names(frailty_pop2)) frailty_pop2[, (cat) := 0L]
  frailty_pop2[, (unique_categories) := lapply(.SD, function(x) as.integer(replace(x, is.na(x), 0L))),
               .SDcols = unique_categories]

  frailty_pop2[, Frailty_labtns_cpss := as.integer(rowSums(.SD, na.rm = TRUE)), .SDcols = unique_categories]

  frag_final <- frailty_pop2[, c("patient_id","start_date","episode_id", unique_categories), with = FALSE]

  fr_grouped <- frag_final %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      Frailty_labtns_cpss = rowSums(dplyr::select(., dplyr::all_of(unique_categories)), na.rm = TRUE)
    )

  data.table::fwrite(fr_grouped, file_path1)

  # ---------- MORBI-FRAILTY ----------
  ind_mo <- map_to_indicators(
    map_dt = df_fr_mo[, .(ICD, MorbiFrailtyCategory)],
    cat_col = "MorbiFrailtyCategory",
    nrows = nrow(df)
  )

  data.table::setkey(ind_mo, row_id)
  data.table::setkey(df, row_id)
  frailty_pop_CO <- ind_mo[df]  # keep all rows

  for (cat in unique_categories_FM) if (!cat %in% names(frailty_pop_CO)) frailty_pop_CO[, (cat) := 0L]
  frailty_pop_CO[, (unique_categories_FM) := lapply(.SD, function(x) as.integer(replace(x, is.na(x), 0L))),
                 .SDcols = unique_categories_FM]

  # Ensure flags exist
  for (nm in c("DiabC","DiabNC","HBPComp","HBPNoComp")) {
    if (!nm %in% names(frailty_pop_CO)) frailty_pop_CO[, (nm) := 0L]
    frailty_pop_CO[, (nm) := as.integer(replace(get(nm), is.na(get(nm)), 0L))]
  }

  # Exclusivity
  frailty_pop_CO[, DiabNC    := data.table::fifelse(DiabC   == 1L & DiabNC    == 1L, 0L, DiabNC)]
  frailty_pop_CO[, HBPNoComp := data.table::fifelse(HBPComp == 1L & HBPNoComp == 1L, 0L, HBPNoComp)]

  frailty_pop_CO[, Morbi_frailty_labtns_cpss := as.integer(rowSums(.SD, na.rm = TRUE)),
                 .SDcols = unique_categories_FM]

  Frag_Co_final <- frailty_pop_CO[, c("patient_id","start_date","episode_id",
                                      unique_categories_FM,
                                      "DiabC","DiabNC","HBPComp","HBPNoComp",
                                      "Morbi_frailty_labtns_cpss"), with = FALSE]

  # Collapse duplicate-named columns (index-safe)
  nm <- names(Frag_Co_final)
  dups <- nm[duplicated(nm)]
  if (length(dups) > 0L) {
    for (n in unique(dups)) {
      idx <- which(nm == n)

      for (k in idx) {
        v <- as.integer(Frag_Co_final[[k]])
        v[is.na(v)] <- 0L
        data.table::set(Frag_Co_final, j = k, value = v)
      }

      merged <- Reduce(function(a, b) pmax(a, b, na.rm = TRUE),
                       lapply(idx, function(k) Frag_Co_final[[k]]))
      data.table::set(Frag_Co_final, j = idx[1], value = as.integer(merged))

      if (length(idx) > 1L) {
        for (k in rev(idx[-1])) data.table::set(Frag_Co_final, j = k, value = NULL)
        nm <- names(Frag_Co_final)
      }
    }
  }

  fm_flags      <- intersect(c("DiabC","DiabNC","HBPComp","HBPNoComp"), names(Frag_Co_final))
  cats_fm_clean <- setdiff(unique(unique_categories_FM), fm_flags)

  fr_grouped_como <- Frag_Co_final %>%
    tibble::as_tibble() %>%
    dplyr::select(
      patient_id, start_date, episode_id,
      dplyr::all_of(cats_fm_clean),
      tidyselect::any_of(fm_flags),
      Morbi_frailty_labtns_cpss
    )

  data.table::fwrite(fr_grouped_como, file_path2)

  return(list(fr_grouped = fr_grouped, fr_grouped_como = fr_grouped_como))
}
