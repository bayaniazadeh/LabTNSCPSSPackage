
library(dplyr)
library(lubridate)
library(data.table)
library(purrr)

Frailty_Calculation <- function(file_path_main){
  # Read the updated episode data with chronic pathologies

  data.table::setDTthreads(percent = 100)

  # ---------- helpers ----------
  normalize_codes <- function(x) toupper(gsub("\\.", "", trimws(as.character(x))))

  # ---------- input ----------
  df <- fread(file_path_main, sep = ",", encoding = "UTF-8", showProgress = FALSE)
  setDT(df)

  # required cols
  req_cols <- c("patient_id","start_date","episode_id","updated_icd_codes")
  miss <- setdiff(req_cols, names(df))
  if (length(miss)) stop("Missing columns in input CSV: ", paste(miss, collapse=", "))

  # stable types for keys
  df[, patient_id := as.character(patient_id)]
  df[, episode_id := as.character(episode_id)]
  # keep start_date class as-is (Date/POSIXct/char); joins are by value

  # ---------- mapping tables (frailty: df_fr) ----------
  if (coding_system == "ICD-10-CA") {
    data("Frailty_mapping")
    df_fr <- as.data.table(Frailty_mapping)
    setnames(df_fr, c("ICD_10_CA_LabTNS","frailty_Comorbidity"), c("ICD","frailty_category"))
  } else if (coding_system == "ICD-10-CM") {
    data("Frailty_ICD10CM")
    df_fr <- as.data.table(Frailty_ICD10CM)
    setnames(df_fr, c("CIM10CMcodes","frailty_Comorbidity"), c("ICD","frailty_category"))
  } else if (coding_system == "ICD-11") {
    data("Frailty_ICD11")
    df_fr <- as.data.table(Frailty_ICD11)
    setnames(df_fr, c("ICD11codes","frailty_Comorbidity"), c("ICD","frailty_category"))
  } else {
    stop("coding_system must be one of: 'ICD-10-CA','ICD-10-CM','ICD-11'")
  }
  df_fr[, ICD := normalize_codes(ICD)]
  df_fr[, frailty_category := as.character(frailty_category)]
  unique_categories <- unique(df_fr$frailty_category)

  # ---------- mapping tables (morbi-frailty: df_fr_mo) ----------
  data("Frailty_Comorbidity_Mapping")
  #df_fr_mo <- as.data.table(Frailty_Comorbidity_Mapping)
  df_fr_mo <- data.table::as.data.table(
    get("Frailty_Comorbidity_Mapping", envir = environment())
  )
  if (coding_system == "ICD-10-CA") {
    setnames(df_fr_mo, "ICD_10_CA_Codes", "ICD")
  } else if (coding_system == "ICD-10-CM") {
    setnames(df_fr_mo, "CIM10CMcodes", "ICD")
  } else {
    setnames(df_fr_mo, "ICD11codes", "ICD")
  }
  df_fr_mo[, ICD := normalize_codes(ICD)]
  df_fr_mo[, MorbiFrailtyCategory := as.character(MorbiFrailtyCategory)]
  unique_categories_FM <- unique(df_fr_mo$MorbiFrailtyCategory)

  # ---------- split updated_icd_codes once (vectorized) ----------
  df[, row_id := .I]
  df[, updated_icd_codes := as.character(updated_icd_codes)]
  codes_list <- strsplit(df$updated_icd_codes, ",", fixed = TRUE)
  codes_list <- lapply(codes_list, function(v) {
    if (is.null(v)) return(character(0))
    v <- normalize_codes(v)
    v <- v[!(v %in% c("", "NA", "NAN", "NULL"))]
    unique(v)
  })

  total_codes <- sum(lengths(codes_list))
  if (total_codes == 0L) {
    # fabricate zero outputs if no codes at all
    fr_grouped <- df[, .(patient_id, start_date, episode_id)]
    for (cat in unique_categories) fr_grouped[, (cat) := 0L]
    fr_grouped[, `:=`(Frailty_labtns_cpss = 0L, id = paste(patient_id, episode_id, sep = "_"))]

    fr_grouped_como <- df[, .(patient_id, start_date, episode_id)]
    for (cat in unique_categories_FM) fr_grouped_como[, (cat) := 0L]
    fr_grouped_como[, Morbi_frailty_labtns_cpss := 0L]

    # write CSVs exactly like your code
    file_path1 <- paste0("LABTNSCPSS_Data/Frailty_Labtns_cpss_", coding_system , input_basename, ".csv")
    fwrite(fr_grouped, file_path1)
    file_path2 <- paste0("LABTNSCPSS_Data/Morbi-frailty_Labtns_cpss_", coding_system, input_basename, ".csv")
    fwrite(fr_grouped_como, file_path2)

    return(list(fr_grouped = as_tibble(fr_grouped), fr_grouped_como = as_tibble(fr_grouped_como)))
  }

  # long table with minimal columns (saves RAM)
  codes_long <- data.table(
    row_id = rep.int(df$row_id, lengths(codes_list)),
    code   = unlist(codes_list, use.names = FALSE)
  )

  # ---------- function: map codes to categories via exact then prefix ----------
  map_to_indicators <- function(map_dt, cat_col, nrows) {
    # map_dt has columns: ICD and <cat_col> (category name column)
    data.table::setDT(map_dt)
    # normalize & keep only the two columns we need
    map_dt[, ICD := normalize_codes(ICD)]
    map_dt[, category := as.character(map_dt[[cat_col]])]
    map_dt <- unique(map_dt[, .(ICD, category)])

    # exact join: codes_long (row_id, code) X map_dt(ICD, category)
    data.table::setkey(codes_long, code)
    data.table::setkey(map_dt, ICD)

    exact_hits <- map_dt[codes_long, on = .(ICD = code), nomatch = 0L][
      , .(row_id, category)]

    # unmatched (row_id, code) pairs for prefix step
    if (nrow(exact_hits)) {
      # build matched pairs (row_id, code) via a light join
      matched_pairs <- unique(map_dt[codes_long, on = .(ICD = code), nomatch = 0L][
        , .(row_id, code = ICD)])
      data.table::setkey(matched_pairs, row_id, code)
      tmp_codes <- codes_long[, .(row_id, code)]
      data.table::setkey(tmp_codes, row_id, code)
      unmatched <- data.table::fsetdiff(tmp_codes, matched_pairs)
    } else {
      unmatched <- unique(codes_long[, .(row_id, code)])
    }

    # combine as data.table (avoid base rbind)
    all_hits <- exact_hits#data.table::rbindlist(list(exact_hits, prefix_hits), use.names = TRUE, fill = TRUE)

    if (nrow(all_hits)) {
      all_hits <- unique(all_hits)[, present := 1L]
      ind_wide <- data.table::dcast(
        all_hits,
        row_id ~ category,
        value.var = "present",
        fun.aggregate = function(x) as.integer(length(x) > 0L),
        fill = 0L
      )
    } else {
      # no categories matched anywhere: return a 1-col data.table with row_id 1..nrows
      ind_wide <- data.table::data.table(row_id = seq_len(nrows))
    }

    data.table::setDT(ind_wide)  # ensure class
    ind_wide
  }


  # ---------- FRAILTY (df_fr) ----------
  ind_fr <- map_to_indicators(df_fr[, .(ICD, frailty_category)], "frailty_category")
  setkey(ind_fr, row_id)
  setkey(df, row_id)
  frailty_pop2 <- ind_fr[df]  # keep all rows

  # ensure all category columns exist
  for (cat in unique_categories) if (!cat %in% names(frailty_pop2)) frailty_pop2[, (cat) := 0L]
  frailty_pop2[, (unique_categories) := lapply(.SD, function(x) as.integer(replace(x, is.na(x), 0L))),
               .SDcols = unique_categories]

  # Frailty_labtns_cpss
  frailty_pop2[, Frailty_labtns_cpss := as.integer(rowSums(.SD, na.rm = TRUE)), .SDcols = unique_categories]

  # build outputs consistent with your code
  frag_final <- frailty_pop2[, c("patient_id","start_date","episode_id", unique_categories), with = FALSE]
  fr_grouped <- frag_final %>%
    as_tibble() %>%
    dplyr::mutate(
      Frailty_labtns_cpss = rowSums(dplyr::select(., dplyr::all_of(unique_categories)), na.rm = TRUE),
      id = paste(patient_id, episode_id, sep = "_")
    )

  # write Frailt_categories_...
  file_path1 <- paste0("LABTNSCPSS_Data/Frailty_Labtns_cpss_", coding_system,"_" ,input_basename, ".csv")
  fwrite(fr_grouped, file_path1)

  # ---------- MORBI-FRAILTY (df_fr_mo) ----------
  ind_mo <- map_to_indicators(df_fr_mo[, .(ICD, MorbiFrailtyCategory)], "MorbiFrailtyCategory")
  setkey(ind_mo, row_id)
  frailty_pop_CO <- ind_mo[df]  # keep all rows

  # ensure all morbi-frailty columns exist
  for (cat in unique_categories_FM) if (!cat %in% names(frailty_pop_CO)) frailty_pop_CO[, (cat) := 0L]
  frailty_pop_CO[, (unique_categories_FM) := lapply(.SD, function(x) as.integer(replace(x, is.na(x), 0L))),
                 .SDcols = unique_categories_FM]

  # exclusivity (Diab/HBP)
  for (nm in c("DiabC","DiabNC","HBPComp","HBPNoComp")) {
    if (!nm %in% names(frailty_pop_CO)) frailty_pop_CO[, (nm) := 0L]
    frailty_pop_CO[, (nm) := as.integer(replace(get(nm), is.na(get(nm)), 0L))]
  }


  ### New
  # Exclusivity (unchanged)
  frailty_pop_CO[, DiabNC    := fifelse(DiabC   == 1L & DiabNC    == 1L, 0L, DiabNC)]
  frailty_pop_CO[, HBPNoComp := fifelse(HBPComp == 1L & HBPNoComp == 1L, 0L, HBPNoComp)]

  # Morbi_frailty_labtns_cpss (unchanged)
  frailty_pop_CO[, Morbi_frailty_labtns_cpss := as.integer(rowSums(.SD, na.rm = TRUE)),
                 .SDcols = unique_categories_FM]

  # Build Frag_Co_final as a data.table
  Frag_Co_final <- frailty_pop_CO[, c("patient_id","start_date","episode_id",
                                      unique_categories_FM,
                                      "DiabC","DiabNC","HBPComp","HBPNoComp",
                                      "Morbi_frailty_labtns_cpss"), with = FALSE]

  # --- Collapse duplicate-named columns (data.table-safe) ---
  # --- Collapse duplicate-named columns in Frag_Co_final (index-safe) ---
  nm   <- names(Frag_Co_final)
  dups <- nm[duplicated(nm)]

  if (length(dups) > 0L) {
    for (n in unique(dups)) {
      # positions of all columns with this duplicated name
      idx <- which(nm == n)

      # 1) coerce each duplicate col to integer 0/1 (NA -> 0), by index
      for (k in idx) {
        v <- as.integer(Frag_Co_final[[k]])
        v[is.na(v)] <- 0L
        data.table::set(Frag_Co_final, j = k, value = v)
      }

      # 2) merged = row-wise OR (pmax) across all duplicates
      merged <- Reduce(function(a, b) pmax(a, b, na.rm = TRUE),
                       lapply(idx, function(k) Frag_Co_final[[k]]))
      data.table::set(Frag_Co_final, j = idx[1], value = as.integer(merged))

      # 3) drop all but the first occurrence (remove by index, from right to left)
      if (length(idx) > 1L) {
        for (k in rev(idx[-1])) {
          data.table::set(Frag_Co_final, j = k, value = NULL)
        }
        nm <- names(Frag_Co_final)  # refresh names after deletions
      }
    }
  }

  # --- Select without duplicates into tibble ---
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


  # write Frailty_comorbidity_categories_...
  file_path2 <- paste0("LABTNSCPSS_Data/Morbi-frailty_Labtns_cpss_", coding_system,"_", input_basename, ".csv")
  fwrite(fr_grouped_como, file_path2)

  return(list(fr_grouped = fr_grouped, fr_grouped_como = fr_grouped_como))

}



