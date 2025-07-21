chronic_pathologies_v2 <- function(file_path_main) {
  library(data.table)
  library(dplyr)
  library(lubridate)
  library(tidyr)

  # Load main episode data
  df <- fread(file_path_main, sep = ",", stringsAsFactors = FALSE)

  # Load appropriate ICD category table
  codes_df <- switch(coding_system,
                     "ICD-10-CA" = { data("ICD10CA_categorisation"); get("ICD10CA_categorisation") },
                     "ICD-10-CM" = { data("ICD10CM_categorisation"); get("ICD10CM_categorisation") },
                     "ICD-11"    = { data("ICD11_categorisation"); get("ICD11_categorisation") },
                     stop("Invalid coding system")
  )
  setnames(codes_df, grep("codes", names(codes_df), value = TRUE), "ICD")

  # Prepare episodes and remove dots
  df <- df %>%
    arrange(patient_id, start_date) %>%
    mutate(ICD = gsub("\\.", "", ICD))

  df_summary <- df %>%
    group_by(patient_id, start_date, episode_id) %>%
    summarise(ICD = list(ICD), .groups = 'drop')

  # Prepare lookup table
  codes_df$ICD <- as.character(codes_df$ICD)
  codes_dict <- data.table(ICD = codes_df$ICD, category = codes_df$category)

  assign_categories <- function(icds) {
    icd_vec <- unlist(icds)
    if (length(icd_vec) == 0) return("None")
    dt <- data.table(ICD = icd_vec)
    result <- merge(dt, codes_dict, by = "ICD", all.x = TRUE)
    cat_vec <- result$category
    cat_vec[is.na(cat_vec)] <- "None"
    return(paste(cat_vec, collapse = ", "))
  }

  df_summary <- df_summary %>%
    rowwise() %>%
    mutate(category_codes = assign_categories(ICD),
           ICD = paste(unlist(ICD), collapse = ", ")) %>%
    ungroup()

  # Convert to data.table
  df <- as.data.table(df_summary)

  get_category_codes <- function(codes, categories, target) {
    icds <- unlist(strsplit(codes, ",\\s*"))
    cats <- as.numeric(gsub("None", NA, unlist(strsplit(categories, ",\\s*"))))
    icds[cats == target]
  }

  df[, chronique_code_cat2 := mapply(get_category_codes, ICD, category_codes, MoreArgs = list(target = 2))]
  df[, cleaned_chronique_code_cat2 := lapply(chronique_code_cat2, function(x) unique(na.omit(x)))]

  # Propagate category 2 codes forward per patient
  df$cleaned_chronique_code_cat2 <- ave(df$cleaned_chronique_code_cat2, df$patient_id, FUN = function(x) {
    sapply(seq_along(x), function(i) paste(unique(unlist(x[1:i])), collapse = ","))
  })

  df[, cleaned_chronique_code_cat2 := gsub("^c\\(|\\)$|\"", "", cleaned_chronique_code_cat2)]

  df[, chronique_code_cat1 := mapply(get_category_codes, ICD, category_codes, MoreArgs = list(target = 1))]
  df[, cleaned_chronique_code_cat1 := lapply(chronique_code_cat1, function(x) unique(na.omit(x)))]

  # Fix dates
  df$start_date <- ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", df$start_date),
                          as.Date(df$start_date),
                          as.Date(ymd(df$start_date)))

  # Propagate category 1 within 1 year
  df_combined <- df %>%
    group_by(patient_id) %>%
    arrange(start_date) %>%
    mutate(cleaned_chronique_code_cat1 = sapply(seq_along(start_date), function(i) {
      valid <- start_date <= start_date[i] & start_date >= start_date[i] - 365
      paste(unique(unlist(cleaned_chronique_code_cat1[valid])), collapse = ", ")
    })) %>%
    ungroup()

  # Load basal codes
  main_dt <- switch(coding_system,
                    "ICD-10-CA" = { data("Basal_Codes"); get("Basal_Codes") },
                    "ICD-10-CM" = { data("ICD10CM_Basal_Codes"); get("ICD10CM_Basal_Codes") },
                    "ICD-11"    = { data("ICD11_Basal_Codes"); get("ICD11_Basal_Codes") }
  )
  setnames(main_dt, grep("Basale", names(main_dt), value = TRUE), "B_ICD")
  setnames(main_dt, grep("Comp", names(main_dt), value = TRUE), "Comp_ICD")

  # Find basal codes
  find_basal_codes <- function(codes) {
    if (length(codes) == 0) return(NA)
    found <- main_dt[Comp_ICD %in% codes, unique(B_ICD)]
    if (length(found) == 0) return(NA)
    return(found)
  }

  df_combined <- as.data.table(df_combined)
  df_combined[, cleaned_chronique_code_cat1 := strsplit(cleaned_chronique_code_cat1, ",\\s*")]
  df_combined[, basal_codes := sapply(cleaned_chronique_code_cat1, function(x) paste(find_basal_codes(x), collapse = ","))]

  # Clean fields
  clean_field <- function(x) {
    x <- gsub('^c\\(\\s*\"|\"\\s*\\)$', '', x)
    x <- gsub('["()]', '', x)
    paste(unique(trimws(unlist(strsplit(x, ",")))), collapse = ",")
  }

  df_combined[, c("cleaned_chronique_code_cat1", "cleaned_chronique_code_cat2", "basal_codes", "ICD") :=
                lapply(.SD, function(col) sapply(col, clean_field)),
              .SDcols = c("cleaned_chronique_code_cat1", "cleaned_chronique_code_cat2", "basal_codes", "ICD")]

  # Final combined ICD field
  df_combined[, updated_icd_codes := mapply(function(a, b, c, d) clean_field(paste(a, b, c, d, sep = ",")),
                                            cleaned_chronique_code_cat1, cleaned_chronique_code_cat2, basal_codes, ICD)]

  # Expand final ICD codes into long format
  df_final <- df_combined %>%
    mutate(updated_icd_codes = strsplit(updated_icd_codes, ",")) %>%
    unnest(updated_icd_codes) %>%
    mutate(updated_icd_codes = trimws(updated_icd_codes)) %>%
    filter(!is.na(updated_icd_codes) & updated_icd_codes != "NA") %>%
    select(patient_id, start_date, updated_icd_codes)

  # Write output
  list_cols <- sapply(df_combined, is.list)

  # Step 2: Convert list columns to string format using toString row-wise
  df_combined[, names(df_combined)[list_cols] := lapply(.SD, function(col) {
    sapply(col, function(x) if (is.null(x)) NA else toString(x))
  }), .SDcols = names(df_combined)[list_cols]]

  write.csv(df_combined, "CHUM_Data/updated_episodes_carry_forward.csv", row.names = FALSE)


  return(df_combined)
}
