
library(dplyr)
library(lubridate)
library(data.table)
library(purrr)

Frailty_Calculation <- function(file_path_main){
  # Read the updated episode data with chronic pathologies

  df <- fread(file_path_main, sep = ',', encoding = 'UTF-8')

  # Read the correspondent table of ICD codes- frailty categories
  # Codes: ICD, Category: frailty_category

  if (coding_system == "ICD-10-CA") { # Other options: "ICD-10-CM", and "ICD-11"
    data("Frailty_mapping")
    df_fr <- Frailty_mapping

    names(df_fr)[names(df_fr) == "ICD_10_CA_LabTNS"] <- "ICD"
    names(df_fr)[names(df_fr) == "frailty_Comorbidity"] <- "frailty_category"


  }else if (coding_system == "ICD-10-CM"){

    data("Frailty_ICD10CM")
    df_fr <- Frailty_ICD10CM

    names(df_fr)[names(df_fr) == "CIM10CMcodes"] <- "ICD"
    names(df_fr)[names(df_fr) == "frailty_Comorbidity"] <- "frailty_category"

  }else if (coding_system == "ICD-11"){

    data("Frailty_ICD11")
    df_fr <- Frailty_ICD11

    names(df_fr)[names(df_fr) == "ICD11codes"] <- "ICD"
    names(df_fr)[names(df_fr) == "frailty_Comorbidity"] <- "frailty_category"

  }



  # Load the correspondent table containing frailty_category categories

  data("Frailty_Comorbidity_Mapping")
  df_fr_mo <- Frailty_Comorbidity_Mapping

  if (coding_system == "ICD-10-CA") { # Other options: "ICD-10-CM", and "ICD-11"

    names(df_fr_mo)[names(df_fr_mo) == "ICD_10_CA_Codes"] <- "ICD"

  }else if (coding_system == "ICD-10-CM"){

    names(df_fr_mo)[names(df_fr_mo) == "CIM10CMcodes"] <- "ICD"

  }else if (coding_system == "ICD-11"){

    names(df_fr_mo)[names(df_fr_mo) == "ICD11codes"] <- "ICD"

  }


  ################################  Preprocessing the data  #########################

  # clean icd codes
  remove_dots <- function(x) {
    gsub("\\.", "", x)
  }

  # Convert 'Code' column to character type and remove dots
  df_fr$ICD <- as.character(df_fr$ICD)
  df_fr$ICD <- remove_dots(df_fr$ICD)


  # Sort data by 'Patient_id' and 'start_date'
  df_sorted <- df %>%
    arrange(patient_id, start_date)


  filtered_df_fr <- df_sorted #[!is.na(df_sorted$date_t0), ]
  filtered_df <- df_sorted

  ################################### Calculate Frailty scores ###################

  # Get unique categories from df_fr
  unique_categories <- unique(df_fr$frailty_category)

  # Add new columns to filtered_df for each category with initial values of 0, prefixed with "fr_"
  for (category in unique_categories) {
    filtered_df_fr[[category]] <- 0
  }

  update_columns <- function(df_fr, filtered_df) {
    # ---- Normalize the mapping table (df_fr) ----
    df_fr$ICD <- toupper(gsub("\\.", "", trimws(as.character(df_fr$ICD))))
    df_fr$frailty_category <- as.character(df_fr$frailty_category)

    # category -> vector of ICD codes
    category_codes_list <- split(df_fr$ICD, df_fr$frailty_category)
    unique_categories <- names(category_codes_list)

    # Ensure indicator columns exist, initialized to 0L
    for (cat in unique_categories) {
      if (!(cat %in% names(filtered_df))) filtered_df[[cat]] <- 0L
    }

    # ---- Parse and normalize the updated_icd_codes column (comma-separated text) ----
    # e.g., "I100, E119, NA"  -> c("I100","E119")
    filtered_df$updated_icd_codes <- lapply(
      strsplit(as.character(filtered_df$updated_icd_codes), ","),
      function(tokens) {
        codes <- trimws(tokens)
        codes <- codes[!(codes %in% c("", "NA", "NaN", "NULL"))]
        codes <- toupper(gsub("\\.", "", codes))
        unique(codes)
      }
    )

    # Precompute 3-char prefixes for each category to speed up fallback matching
    cat_prefixes <- lapply(category_codes_list, function(v) substr(v, 1, 3))

    # ---- Row-wise mapping: exact first, then 3-char prefix ----
    n <- nrow(filtered_df)
    for (i in seq_len(n)) {
      codes <- filtered_df$updated_icd_codes[[i]]
      if (length(codes) == 0L) next

      for (code in codes) {
        matched <- FALSE

        # Exact match: set any category that contains this code
        for (cat in unique_categories) {
          if (code %in% category_codes_list[[cat]]) {
            filtered_df[i, cat] <- 1L
            matched <- TRUE
            # do NOT break out entirely; a code might map to multiple cats,
            # but if you want first-hit only, uncomment the next line:
            # break
          }
        }
        if (matched) next

        # Fallback: 3-char prefix match
        cp <- substr(code, 1, 3)
        for (cat in unique_categories) {
          if (any(cp == cat_prefixes[[cat]])) {
            filtered_df[i, cat] <- 1L
            # similarly, don't break if multi-map is allowed
            # break
          }
        }
      }
    }

    # Make sure indicators are integer 0/1 (and no NAs)
    for (cat in unique_categories) {
      filtered_df[[cat]] <- as.integer(replace(filtered_df[[cat]], is.na(filtered_df[[cat]]), 0L))
    }

    filtered_df
  }

  # Apply the function
  filtered_df_fr <- update_columns(df_fr, filtered_df_fr)


  ### Check sum of a frailty score
  #sum(filtered_df$`Cardiac and vascular`, na.rm = TRUE)

  # Have to convert columns containing lists before writing in csv!


  # Compute frailty score by summing only the selected columns from unique_categories
  # Compute frailty score by summing only the selected columns from unique_categories
  frailty_pop2 <- filtered_df_fr %>%
    mutate(frailty_score = rowSums(select(., all_of(unique_categories)), na.rm = TRUE))  # Exclude NA values

  frailty_pop2 <- frailty_pop2 %>%
    mutate(across(where(is.list), ~ sapply(., function(x) paste(unlist(x), collapse = ","))))

  frag_final <- frailty_pop2 %>%
    select(patient_id, start_date, episode_id, all_of(unique_categories))


  cols_to_exclude <- c("ICD", "category_codes", "chronique_code_cat2", "cleaned_chronique_code_cat2",
                       "chronique_code_cat1", "cleaned_chronique_code_cat1", "basal_codes", "updated_icd_codes")

  frailty_pop2 <- frailty_pop2 %>%
    select(-all_of(cols_to_exclude))

  # Write the final data, could be used for verifications

  #write.csv(frailty_pop2, file = file_path, row.names = FALSE) # or filtered_df
  file_path <- paste0("LABTNSCPSS_Data/Frailt_categories_", input_basename, ".csv")
  # Write to file
  write.csv(frailty_pop2, file = file_path, row.names = FALSE)

  # You can proceed with the rest of your operations as needed
  #fr_grouped <- frag_final %>%
  #  select(patient_id, start_date, episode_id, all_of(unique_categories)) %>%  # Select episode_id and columns in unique_categories
  #  mutate(frailty_score = rowSums(select(., all_of(unique_categories)), na.rm = TRUE))  # Calculate frailty_score

  fr_grouped <- frag_final %>%
    select(patient_id, start_date, episode_id, all_of(unique_categories)) %>%
    mutate(
      frailty_score = rowSums(select(., all_of(unique_categories)), na.rm = TRUE),
      id = paste(patient_id, episode_id, sep = "_")  # Create id column
    )

  ############ Calculate frailty_category
  unique_categories_FM <- unique(df_fr_mo$MorbiFrailtyCategory)

  # Add new columns to filtered_df for each category with initial values of 0, prefixed with "fr_"
  for (category in unique_categories_FM) {
    filtered_df[[category]] <- 0
  }


  # --- helpers
  .normalize_codes <- function(x) toupper(gsub("\\.", "", trimws(as.character(x))))
  .parse_updated_icd <- function(x) {
    # Accept list-column or character
    if (is.list(x)) {
      lapply(x, function(v) {
        v <- .normalize_codes(v)
        v <- v[!(v %in% c("", "NA", "NAN", "NULL"))]
        unique(v)
      })
    } else {
      # character vector (one string per row, comma-separated)
      lapply(strsplit(as.character(x), ","), function(v) {
        v <- .normalize_codes(v)
        v <- v[!(v %in% c("", "NA", "NAN", "NULL"))]
        unique(v)
      })
    }
  }

  Update_Comorbidity_Frailty <- function(df_fr_mo, filtered_df) {
    # --- 1) Normalize mapping table
    df_fr_mo <- data.frame(
      ICD = .normalize_codes(df_fr_mo$ICD),
      MorbiFrailtyCategory = as.character(df_fr_mo$MorbiFrailtyCategory),
      stringsAsFactors = FALSE
    )

    # Build fast lookup maps:
    # exact_map: code -> vector of categories
    exact_map <- split(df_fr_mo$MorbiFrailtyCategory, df_fr_mo$ICD)

    # prefix_map: 3-char prefix -> vector of categories
    df_fr_mo$prefix <- substr(df_fr_mo$ICD, 1, 3)
    prefix_map <- split(df_fr_mo$MorbiFrailtyCategory, df_fr_mo$prefix)

    # Unique category set
    unique_categories <- sort(unique(df_fr_mo$MorbiFrailtyCategory))

    # --- 2) Ensure indicator columns exist as integer 0/1
    for (cat in unique_categories) {
      if (!cat %in% names(filtered_df)) filtered_df[[cat]] <- 0L
      # coerce to integer and fill NAs with 0
      if (!is.integer(filtered_df[[cat]])) {
        filtered_df[[cat]] <- as.integer(filtered_df[[cat]])
      }
      filtered_df[[cat]][is.na(filtered_df[[cat]])] <- 0L
    }

    # --- 3) Ensure updated_icd_codes is a clean list-column of codes
    filtered_df$updated_icd_codes <- .parse_updated_icd(filtered_df$updated_icd_codes)

    # --- 4) Row-wise assignment with exact > prefix fallback
    n <- nrow(filtered_df)
    for (i in seq_len(n)) {
      codes <- filtered_df$updated_icd_codes[[i]]
      if (length(codes) == 0L) next

      for (code in codes) {
        # Try exact
        cats_exact <- exact_map[[code]]
        if (!is.null(cats_exact) && length(cats_exact)) {
          # set all exact categories
          for (cat in unique(cats_exact)) {
            filtered_df[i, cat] <- 1L
          }
          next  # skip prefix for this code
        }

        # Fallback to prefix
        pref <- substr(code, 1, 3)
        cats_pref <- prefix_map[[pref]]
        if (!is.null(cats_pref) && length(cats_pref)) {
          for (cat in unique(cats_pref)) {
            filtered_df[i, cat] <- 1L
          }
        }
      }
    }

    # Make sure nothing is NA and all are integers 0/1
    for (cat in unique_categories) {
      v <- filtered_df[[cat]]
      v[is.na(v)] <- 0L
      filtered_df[[cat]] <- as.integer(v > 0L)
    }

    filtered_df
  }


  # Apply the function
  filtered_df_FR_CO <- Update_Comorbidity_Frailty(df_fr_mo, filtered_df)

  # Compute frailty score by summing only the selected columns from unique_categories
  frailty_pop_CO <- filtered_df_FR_CO %>%
    mutate(frailty_score = rowSums(select(., all_of(unique_categories_FM)), na.rm = TRUE))  # Exclude NA values

  frailty_pop_CO <- frailty_pop_CO %>%
    mutate(across(where(is.list), ~ sapply(., function(x) paste(unlist(x), collapse = ","))))


  # Select columns based on unique_categories
  Frag_Co_final <- frailty_pop_CO %>%
    select(patient_id, start_date, episode_id, all_of(unique_categories_FM))



  # You can proceed with the rest of your operations as needed
  # fr_grouped_como <- Frag_Co_final %>%
  #  select(patient_id, start_date, episode_id, all_of(unique_categories_FM)) %>%
  # mutate(
  # keep Diab = 1; if both Diab & DiabNC are 1, set DiabNC = 0
  #  DiabC   = coalesce(.data[["DiabC"]], 0L),
  # DiabNC = if_else(.data[["DiabC"]] == 1L & coalesce(.data[["DiabNC"]], 0L) == 1L,
  #                 0L, coalesce(.data[["DiabNC"]], 0L))
  #) %>%
  #mutate(
  # morbi_frailty_score = rowSums(dplyr::select(., all_of(unique_categories_FM)), na.rm = TRUE)
  #)
  fr_grouped_como <- Frag_Co_final %>%
    dplyr::select(
      patient_id, start_date, episode_id,
      all_of(unique_categories_FM),
      DiabC, DiabNC, HBPComp, HBPNoComp
    ) %>%
    dplyr::mutate(
      # Force 0/1 integers, replace NA with 0
      dplyr::across(
        c(DiabC, DiabNC, HBPComp, HBPNoComp),
        ~ as.integer(dplyr::coalesce(as.integer(.x), 0L))
      )
    ) %>%
    dplyr::mutate(
      # Exclusivity rules:
      # 1) If both diabetes flags are 1, keep DiabC and zero DiabNC
      DiabNC = dplyr::if_else(DiabC == 1L & DiabNC == 1L, 0L, DiabNC),
      # 2) If both hypertension flags are 1, keep HBPComp and zero HBPNoComp
      HBPNoComp = dplyr::if_else(HBPComp == 1L & HBPNoComp == 1L, 0L, HBPNoComp)
    ) %>%
    dplyr::mutate(
      morbi_frailty_score =
        rowSums(dplyr::select(., all_of(unique_categories_FM)), na.rm = TRUE) |> as.integer()
    )


  #write.csv(fr_grouped_como, file = file_path, row.names = FALSE) # or filtered_df
  file_path <- paste0("LABTNSCPSS_Data/Frailty_comorbidity_categories_", input_basename, ".csv")

  # Write to CSV
  write.csv(fr_grouped_como, file = file_path, row.names = FALSE)

  return(list(fr_grouped = fr_grouped, fr_grouped_como = fr_grouped_como))

}



