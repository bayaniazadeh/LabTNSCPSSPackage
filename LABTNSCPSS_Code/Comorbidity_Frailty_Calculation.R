
Comorbidity_Frailty_Calculation <- function(file_path_main, fr_grouped, fr_grouped_como){
  ### Step 1: Read Dataset from CSV file
  # Read the dataset that contains patient ICD codes and nam

  X_CHUM <- fread( file_path_main, sep = ",")


  ### Step 2: Data Preparation
  # Rename columns based on the data structure in your input:
  # - 'cim' refers to ICD codes
  # - 'nam' refers to patient IDs

  df_combined <- data.table(
    id = 1:length(X_CHUM$patient_id),       # Create a unique ID for each row
    updated_icd_codes = X_CHUM$updated_icd_codes,  # Assign ICD codes to a new column
    patient_id = X_CHUM$patient_id,
    start_date = X_CHUM$start_date,
    episode_id = X_CHUM$episode_id# Assign patient IDs to a new column
  )


  # Step 3: Prepare data for comorbidity function
  # Convert the combined ICD codes data into a format required by the comorbidity function.

  df_combined <- data.frame(id = paste(df_combined$patient_id, df_combined$episode_id,df_combined$start_date, sep = "_"),
                            code = df_combined$updated_icd_codes,
                            start_date = df_combined$start_date,
                            episode_id = df_combined$episode_id)

  df_cleaned <- df_combined %>%
    # Separate the combined codes into multiple rows
    separate_rows(code, sep = ",") %>%  # Use 'code' here instead of 'combined_icd_codes'
    # Remove any leading/trailing whitespace from codes
    mutate(code = trimws(code)) %>%
    # Filter out rows with 'NA' and empty strings
    filter(code != "NA" & code != "")

  # Prepare the data for comorbidity calculation
  df_comrbidity <- data.frame(id = df_cleaned$id, code = df_cleaned$code,
                              start_date = df_cleaned$start_date, episode_id = df_cleaned$episode_id)


  ### Step 6: Calculate Elixhauser Comorbidity Score
  # Apply the 'comorbidity' function to calculate the Elixhauser comorbidity score.
  # The map argument specifies that ICD-10 codes are being used.
  # pour le charlson avec ICD10CA map = charlson_icd10ca_labtns, pour le ELixhauser map = elixhauser_icd10ca_labtns
  if (coding_system == "ICD-10-CA") {
    mapping_Elix <- "elixhauser_icd10ca_labtns"
    mapping_Ch <- "charlson_icd10ca_labtns"
    mapping_combined <- "combined_icd10ca_labtns"

  } else if (coding_system == "ICD-10-CM") {
    mapping_Elix <- "elixhauser_icd10_cm"
    mapping_Ch <- "charlson_icd10_cm"
    mapping_combined <- "combined_icd10_cm"

  } else if (coding_system == "ICD-11") {
    mapping_Elix <- "elixhauser_icd11"
    mapping_Ch <- "charlson_icd11"
    mapping_combined <- "combined_icd11"
  }


  #mapping_Elix = "elixhauser_icd10ca_labtns"# "elixhauser_icd10ca_labtns"
  elixhauser_popICD10CA <- comorbidity(x = df_comrbidity, id = "id", code = "code", map = mapping_Elix, assign0 = FALSE)


  final_data_elixhauser <- elixhauser_popICD10CA %>%
    mutate(
      # make sure missing are treated as 0 for the logic
      Diab_NC  = if_else(coalesce(Diab_NC,  0L) == 1L & coalesce(Diab_C, 0L) == 1L, 0L, coalesce(Diab_NC, 0L)),
      Diab_C = coalesce(Diab_C, 0L)
    ) %>%
    mutate(
      Elixhauser_labtns_cpss = rowSums(across(-id), na.rm = TRUE)
    ) %>%
    separate(id, into = c("patient_id", "episode_id", "start_date"), sep = "_", remove = TRUE)



  file_path <- glue("LABTNSCPSS_Data/ECI_Labtns_cpss_{coding_system}_{input_basename}.csv")

  file_path

  write.csv(final_data_elixhauser, file = file_path, row.names = FALSE) # or filtered_df

  ## Create coexisting comorbidity
  cm_melted <- melt(elixhauser_popICD10CA, id.vars = "id",
                    variable.name = "comorbidity", value.name = "presence")

  cm_melted <- cm_melted[cm_melted$presence == 1, ]



  # weight = readmission_elix_hcup, ou mortality_elix_hcup (pour utiliser les poids fournis par HCUP)

  if (mapping_Elix == "elixhauser_icd10ca_labtns") {
    # Assuming elixhauser_popICD10CA is already defined in your environment
    score_pop_Elixh <- score(x = elixhauser_popICD10CA, weights = "readmission_elix_hcup", assign0 = FALSE)

  }


  ### Step 7: Calculate Charlson Comorbidity Score
  #mapping_Ch = "charlson_icd10ca_labtns"#"charlson_icd10ca_labtns"
  chalrson_popICD10CA <- comorbidity(x = df_comrbidity, id = "id", code = "code", map = mapping_Ch, assign0 = FALSE)


  # Write the final data

  chalson_labels <- c(labels <- c(
    "Patient ID",
    "Episode ID",
    "Start date",
    "Myocardial infarction",
    "Congestive heart failure",
    "Peripheral vascular disease",
    "Cerebrovascular disease",
    "Dementia",
    "Chronic pulmonary disease",
    "Rheumatic disease",
    "Peptic ulcer disease",
    "Mild liver disease",
    "Diabetes",
    "Diabetes with chronic complications",
    "Hemiplegia or paraplegia",
    "Renal disease",
    "Cancer",
    "moderate or severe liver disease",
    "Metastatic solid tumor",
    "AIDS/HIV",
    "Charlson comorbidity score"))


  # Assign labels using a loop

  final_data_charlson <- chalrson_popICD10CA %>%
    mutate(
      # treat NA as 0 for the logic
      Diab_NC   = if_else(coalesce(Diab_NC,   0L) == 1L & coalesce(Diab_C, 0L) == 1L, 0L, coalesce(Diab_NC, 0L)),
      Diab_C = coalesce(Diab_C, 0L),
      Charlson_labtns_cpss = rowSums(across(-id), na.rm = TRUE)
    ) %>%
    separate(id, into = c("patient_id", "episode_id", "start_date"), sep = "_")
  #df_with_labels <- rbind(chalson_labels, final_data_charlson)

  file_path <- glue("LABTNSCPSS_Data/CCI_Labtns_cpss_{coding_system}_{input_basename}.csv")
  # Write to Excel
  write.csv(final_data_charlson, file = file_path, row.names = FALSE, na = "")


  ## Create coexisting comorbidity for Charlson Scores
  cm_melted <- melt(chalrson_popICD10CA, id.vars = "id",
                    variable.name = "comorbidity", value.name = "presence")

  cm_melted <- cm_melted[cm_melted$presence == 1, ]


  ## Calculate combined comorbidity scores of Charlson, Elixhauser


  Combined_popICD10CA <- comorbidity(x = df_comrbidity, id = "id", code = "code", map = mapping_combined, assign0 = FALSE)

  # Write the final data

  combined_labels <- c(labels <- c(
    "Patient ID",
    "Episode ID",
    "Start date",
    "Myocardial infarction",
    "Alcohol abuse",
    "Deficiency anemia",
    "Blood loss anemia",
    "Cardiac arrhythmia",
    "Hematologic cancer",
    "Metastatic cancer",
    "Solid tumour without metastasis",
    "Congestive heart failure",
    "Coagulopathy",
    "Cerebrovascular disease",
    "Dementia",
    "Depression",
    "Diabetes with complications",
    "Diabetes without complications",
    "Drug abuse",
    "Fluid and electrolyte disorder",
    "HIV",
    "Hypertension with complications",
    "Hypertension without complications",
    "Hypothyroidism",
    "Liver disease",
    "Liver disease, severe",
    "Neurological disorder",
    "Obesity",
    "Paralytic syndrome",
    "Psychosis",
    "Peptic ulcer disease",
    "Chronic pulmonary disease",
    "Pulmonary circulation disorder",
    "Peripheral vascular disease",
    "Renal disease",
    "Rheumatoid disease",
    "Valvular disease",
    "Weight loss",
    "combined_labtns_score"))


  # Assign labels using a loop

  final_data_combined <- Combined_popICD10CA %>%
    mutate(
      # treat NAs as 0 for the logic
      Diab_C  = coalesce(Diab_C,  0L),
      Diab_NC = if_else(Diab_C == 1L & coalesce(Diab_NC, 0L) == 1L, 0L, coalesce(Diab_NC, 0L)),
      Combined_comorb_labtns_cpss = rowSums(across(-id), na.rm = TRUE)
    ) %>%
    separate(id, into = c("patient_id", "episode_id", "start_date"), sep = "_")
  #df_with_labels <- rbind(combined_labels, final_data_combined) # new

  file_path <- glue("LABTNSCPSS_Data/Combined_Comorb_Labtns_cpss_{coding_system}_{input_basename}.csv")
  # Write to Excel
  write.csv(final_data_combined, file = file_path, row.names = FALSE, na = "")



  ### Step 8: combine final scores together
  final_data_elixhauser <- final_data_elixhauser %>%
    mutate(across(c(patient_id, episode_id, start_date), as.character))

  final_data_charlson <- final_data_charlson %>%
    mutate(across(c(patient_id, episode_id, start_date), as.character))

  fr_grouped <- fr_grouped %>%
    mutate(across(c(patient_id, episode_id, start_date), as.character))

  fr_grouped_como <- fr_grouped_como %>%
    mutate(across(c(patient_id, episode_id, start_date), as.character))



  # Merge the three data frames on episode_id
  scores_final <- final_data_charlson %>%
    select(patient_id, episode_id, start_date, Charlson_labtns_cpss) %>%
    left_join(
      final_data_elixhauser %>%
        select(patient_id, episode_id, start_date, Elixhauser_labtns_cpss),
      by = c("patient_id", "episode_id", "start_date")
    ) %>%
    left_join(
      final_data_combined %>%
        select(episode_id, Combined_comorb_labtns_cpss),
      by = "episode_id"
    ) %>%
    left_join(
      fr_grouped %>%
        select(patient_id, episode_id, start_date, Frailty_labtns_cpss), ## Change
      by = c("patient_id", "episode_id", "start_date")
    ) %>%
    left_join(
      fr_grouped_como %>%
        select(patient_id, episode_id, start_date, Morbi_frailty_labtns_cpss),
      by = c("patient_id", "episode_id", "start_date")
    )



  file_path <- glue::glue("LABTNSCPSS_Data/Final_scores_comorbidity_frailty_Labtns_cpss_{coding_system}_{input_basename}.csv")

  write.csv(scores_final, file = file_path, row.names = FALSE)

  return(list(scores_final = scores_final,
              final_data_combined = final_data_combined,
              final_data_charlson = final_data_charlson,
              final_data_elixhauser = final_data_elixhauser))

}


