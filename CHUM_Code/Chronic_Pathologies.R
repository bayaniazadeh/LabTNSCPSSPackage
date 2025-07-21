
# file_path : CHUM_Data/input_data_cleaned.csv
chronic_pathologies <- function(file_path_main){


  #file_path_main <- "CHUM_Data/input_data_cleaned.csv"
  # Load the episodes data
  df <- fread(file_path_main, sep = ",", stringsAsFactors = FALSE)


  # Load the correspondent table containing ICD codes and their chronic categories
  if (coding_system == "ICD-10-CA") {
    data("ICD10CA_categorisation")
    codes_df <- get("ICD10CA_categorisation")
    names(codes_df)[names(codes_df) == "ICD10CAcodes"] <- "ICD"

  } else if (coding_system == "ICD-10-CM") {
    data("ICD10CM_categorisation")
    codes_df <- get("ICD10CM_categorisation")
    names(codes_df)[names(codes_df) == "CIM10CMcodes"] <- "ICD"

  } else if (coding_system == "ICD-11") {
    data("ICD11_categorisation")
    codes_df <- get("ICD11_categorisation")
    names(codes_df)[names(codes_df) == "ICD11codes"] <- "ICD"
  }


  colnames(codes_df)
  ############################ Step 1: detect chronic categories for patients' ICD codes ####################

  # Ensure ICD10CAcodes are read as strings
  codes_df$ICD <- as.character(codes_df$ICD)


  # Sort episodes by patient_id and start date
  df <- df[order(df$patient_id, df$start_date), ]



  # Function to remove dots from strings
  remove_dots <- function(lst) {
    return(gsub("\\.", "", lst))
  }

  # Apply the operations
  df <- df %>%
    mutate(ICD = remove_dots(ICD))

  df_summary <- df %>%
    group_by(patient_id, start_date, episode_id) %>%
    summarise(ICD = list(ICD), .groups = 'drop')



  # Create a fast lookup corresponding table using data.table
  codes_dict <- data.table::rbindlist(
    lapply(seq_len(nrow(codes_df)), function(i) {
      data.table(ICD = codes_df$ICD[[i]], category = codes_df$category[i])
    })
  )

  # Function to assign categories ensuring the length matches the input ICD codes
  assign_categories <- function(icd_codes) {
    # Convert the list of ICD codes to a data.table
    icd_codes_list <- unlist(strsplit(icd_codes, ", "))

    # Check if icd_codes_list is empty
    if (length(icd_codes_list) == 0) return(character(0))

    icd_codes_dt <- data.table(ICD = icd_codes_list)

    # Perform a join with codes_dict to find matching categories
    result <- merge(icd_codes_dt, codes_dict, by = "ICD", all.x = TRUE)


    categories <- result[order(match(result$ICD, icd_codes_list)), category]

    # Replace NA values with "None"
    categories[is.na(categories)] <- "None"

    # Return a character vector with the same length as the input ICD codes
    return(categories)

  }


  #  assign categories and put in category_codes column
  df_summary$category_codes <- lapply(df_summary$ICD, assign_categories)


  # Convert list columns to character vectors (concatenate list elements into a single string)
  df_summary$category_codes <- sapply(df_summary$category_codes, function(x) paste(x, collapse = ", "))
  df_summary$ICD <- sapply(df_summary$ICD, function(x) paste(x, collapse = ", "))



  ############################ Step 2: Checking episodes for Category 2 ####################

  # Convert to data.table for efficient manipulation
  df <- as.data.table(df_summary)

  # Function to get Category 2 codes
  get_category2_codes <- function(episode_codes, category_codes) {
    icd_list <- unlist(strsplit(episode_codes, ",\\s*"))
    category_list <- unlist(strsplit(category_codes, ",\\s*"))

    # Convert "None" to NA
    category_list[category_list == "None"] <- NA

    # Convert category_list to numeric (to handle NA and numeric categories)
    category_list <- as.numeric(category_list)

    # Extract the codes where category is 2
    category2_codes <- icd_list[category_list == 2]

    return(category2_codes)

  }

  # Put the ICD codes that are in Category2 in a column called: chronique_code_cat2
  df[, chronique_code_cat2 := mapply(get_category2_codes, ICD, category_codes)]

  # Function to clean and get unique ICD codes
  clean_unique_codes <- function(codes) {
    unique_codes <- unique(na.omit(codes))
    unique_codes
  }


  df[, cleaned_chronique_code_cat2 := lapply(chronique_code_cat2, clean_unique_codes)]


  ### repeat ICD codes in category 2 for the all the following episodes
  df$cleaned_chronique_code_cat2 <- ave(
    df$cleaned_chronique_code_cat2,
    df$patient_id,
    FUN = function(x) {
      sapply(seq_along(x), function(i) {
        paste(unique(na.omit(x[1:i])), collapse = ",")
      })
    }
  )



  clean_and_extract_values <- function(x) {
    # Remove "character(0)" and trim white spaces
    x <- gsub('^character\\(0\\)', '', x)
    x <- trimws(x)

    # Extract values from c("...") format
    if (grepl('^c\\(', x)) {
      # Remove leading 'c(' and trailing ')'
      x <- gsub('^c\\(\\s*\"|\"\\s*\\)$', '', x)
      # Replace '","' with ','
      x <- gsub('","', ',', x)
    }

    # Trim leading and trailing white spaces and remove any remaining quotes
    x <- gsub('^"|"$', '', x)
    x <- trimws(x)

    # Remove empty strings
    values <- unlist(strsplit(x, ","))
    values <- trimws(values)
    values <- values[values != ""]

    # Combine unique values into a single string separated by commas
    result <- paste(unique(values), collapse = ",")

    return(result)
  }

  # Clean values after propagation
  df[, cleaned_chronique_code_cat2 := sapply(cleaned_chronique_code_cat2, clean_and_extract_values)]


  ############################### Step 3: Checking for Category 1 ################

  # Function to get Category codes based on the target category
  get_category_codes <- function(cim, category_codes, target_category) {
    icd_list <- unlist(strsplit(cim, ",\\s*"))
    category_list <- unlist(strsplit(category_codes, ",\\s*"))

    # Convert "None" to NA
    category_list[category_list == "None"] <- NA

    # Convert category_list to numeric (to handle NA and numeric categories)
    category_list <- as.numeric(category_list)

    # Extract the codes where category matches the target_category
    target_codes <- icd_list[category_list == target_category]

    return(target_codes)
  }

  # Extract Category 1 codes
  df[, chronique_code_cat1 := mapply(get_category_codes, ICD, category_codes, target_category = 1)]

  # Function to clean and get unique ICD codes
  clean_unique_codes <- function(codes) {
    unique_codes <- unique(na.omit(codes))
    unique_codes
  }


  df[, cleaned_chronique_code_cat1 := lapply(chronique_code_cat1, clean_unique_codes)]



  # Function to check and convert date_debut
  df$start_date <- sapply(df$start_date, function(x) {
    # Check if the date is in YYYY-MM-DD format
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
      return(x)  # Return as is if already in correct format
    } else if (grepl(" ", x)) {
      return(x)  # Return as is if it has a time component
    } else {
      # Attempt to convert from YYYYMMDD or other formats
      date_parsed <- ymd(paste0(substring(x, 1, 4), "-",  # Year
                                substring(x, 5, 6), "-",  # Month
                                substring(x, 7, 8))      # Day
      )
      if (is.na(date_parsed)) {
        warning(paste("Failed to parse date:", x))
        return(NA)  # Return NA for failed parses
      } else {
        return(as.character(date_parsed))  # Return the parsed date as character
      }
    }
  })

  # Convert the result back to Date class
  df$start_date <- as.Date(df$start_date)






  ## add ICD codes in category 1 to the following 1 year
  df_combined <- df %>%
    group_by(patient_id) %>%
    arrange(start_date) %>%
    mutate(
      # Initialize the cumulative codes
      cleaned_chronique_code_cat1 = sapply(seq_along(start_date), function(i) {
        codes_within_year <- cleaned_chronique_code_cat1[start_date <= start_date[i] & start_date >= start_date[i] - 365]
        paste(na.omit(unique(c(codes_within_year))), collapse = ", ")
      })
    ) %>%
    ungroup()




  # Convert list columns to character strings
  df_combined <- df_combined %>%
    mutate(across(where(is.list), ~ sapply(., function(x) paste(unlist(x), collapse = ","))))



  ############################# Step 4: check for Basal codes ######################

  ### Load the corresponding Basal codes

  if (coding_system == "ICD-10-CA") {
    data("Basal_Codes")
    main_dt   <- get("Basal_Codes")
    names(main_dt)[names(main_dt) == "B_ICD10CAcodesBasale"] <- "B_ICD"
    names(main_dt)[names(main_dt) == "Comp_ICD10CAcodes"] <- "Comp_ICD"

  } else if (coding_system == "ICD-10-CM") {
    data("ICD10CM_Basal_Codes")
    main_dt   <- get("ICD10CM_Basal_Codes")
    names(main_dt)[names(main_dt) == "B_CIM10CMcodesbasale"] <- "B_ICD"
    names(main_dt)[names(main_dt) == "Comp_CIM10CMcodes"] <- "Comp_ICD"

  } else if (coding_system == "ICD-11") {
    data("ICD11_Basal_Codes")
    main_dt   <- get("ICD11_Basal_Codes")
    names(main_dt)[names(main_dt) == "B_CIM11Basale"] <- "B_ICD"
    names(main_dt)[names(main_dt) == "Comp_CIM11codes"] <- "Comp_ICD"
  }



  find_basal_codes <- function(code_list, main_dt) {
    result <- main_dt[Comp_ICD %in% code_list, unique(B_ICD)]
    if (length(result) == 0) {
      return(NA)  # Return NA if no matches are found
    }
    return(result)
  }

  # Convert to data.table
  if (!is.data.table(df_combined)) {
    df_combined <- as.data.table(df_combined)
  }


  # Convert code_list to a proper list column if it's a character vector with comma-separated values
  df_combined[, cleaned_chronique_code_cat1 := lapply(cleaned_chronique_code_cat1, function(x) if (x == "character(0)") character(0) else unlist(strsplit(x, ",")))]



  # Find the corresponding basal codes for category 1 icd codes
  df_combined[, basal_codes := sapply(cleaned_chronique_code_cat1, function(x) paste(find_basal_codes(x, main_dt), collapse = ","))]


  clean_value <- function(x) {
    # Remove leading and trailing "c(" and ")"
    x <- gsub('^c\\(\\s*\"|\"\\s*\\)$', '', x)
    # Remove unwanted characters
    x <- gsub('^\\s*character\\(0\\)\\s*', '', x)  # Remove "character(0)"
    x <- gsub('^\\s*\"\\s*|\\s*\"\\s*$', '', x)     # Remove leading and trailing quotes
    # Remove any extra unwanted characters
    x <- gsub('[\\s*\"\\(\\)]', '', x)
    # Split by comma, trim each part, and join back with commas
    x <- paste(trimws(unlist(strsplit(x, ","))), collapse = ",")

    return(x)
  }



  # Apply the cleaning function to each relevant column
  df_combined[, cleaned_chronique_code_cat1 := sapply(cleaned_chronique_code_cat1, clean_value)]
  df_combined[, cleaned_chronique_code_cat2 := sapply(cleaned_chronique_code_cat2, clean_value)]
  df_combined[, basal_codes := sapply(basal_codes, clean_value)]
  df_combined[, ICD := sapply(ICD, clean_value)]



  ######################### Step 5: combine the updated ICD codes in one column and write the data#################

  # Combine the cleaned columns into updated_icd_codes(containing final updated codes)
  df_combined[, updated_icd_codes := paste(cleaned_chronique_code_cat1, cleaned_chronique_code_cat2, basal_codes, ICD, sep = ",")]


  clean_and_concatenate <- function(...){
    # Combine all input vectors into a single vector
    combined <- unlist(list(...))

    # Remove NA values
    cleaned <- na.omit(combined)

    # Remove leading and trailing whitespace from each element
    cleaned <- trimws(cleaned)

    # Remove any empty strings
    cleaned <- cleaned[cleaned != ""]

    # Remove any leading 'c' characters in the middle of strings
    cleaned <- gsub('\\bc', '', cleaned)

    # Remove any remaining leading and trailing whitespace
    cleaned <- trimws(cleaned)

    # Combine into a single string with commas
    result <- paste(unique(cleaned), collapse = ", ")

    return(result)
  }




  df_combined[, updated_icd_codes := apply(
    df_combined[, .(cleaned_chronique_code_cat1, cleaned_chronique_code_cat2, basal_codes, ICD)],
    1,
    function(row) clean_and_concatenate(row)
  )]

  df_combined <- df_combined %>%
    mutate(across(where(is.list), ~ sapply(., function(x) paste(unlist(x), collapse = ","))))


  df_final <- df_combined %>%
    mutate(updated_icd_codes = strsplit(updated_icd_codes, ",")) %>% # Split codes into lists
    unnest(updated_icd_codes) %>% # Unnest to create separate rows
    mutate(updated_icd_codes = trimws(updated_icd_codes)) %>% # Trim whitespace
    filter(!is.na(updated_icd_codes) & updated_icd_codes != "NA") %>% # Remove NA and empty strings
    select(patient_id, start_date, updated_icd_codes) # Select only the desired columns


  #out_path <- "CHUM_Data/updated_episodes_carry_forward.csv"

  #write.csv(df_combined, file = out_path, row.names = FALSE)
  out_path <- paste0("CHUM_Data/updated_episodes_carry_forward_", input_basename, ".csv")

  # Write the file
  write.csv(df_combined, file = out_path, row.names = FALSE)

  return(df_combined)
}










