
# file_path : "CHUM_Data/episode_test.csv"
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
    mutate(Elix_score = rowSums(select(., -id)))


  final_data_elixhauser <- final_data_elixhauser %>%
    separate(id, into = c("patient_id", "episode_id", "start_date"), sep = "_")

  # Write the final data

  #file_path <- glue("CHUM_Data/Comorbidity_{mapping_Elix}.csv")
  file_path <- glue("CHUM_Data/Comorbidity_{mapping_Elix}_{input_basename}.csv")

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


  final_data_charlson <- chalrson_popICD10CA %>%
    mutate(Charlson_score := rowSums(select(., -id)))


  final_data_charlson <- final_data_charlson %>%
    separate(id, into = c("patient_id", "episode_id", "start_date"), sep = "_")

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
  #column_names <- colnames(final_data_charlson)  # Get column names

  # Loop through columns and assign labels
 # for (i in 1:length(column_names)) {
  #  attr(final_data_charlson[[column_names[i]]], "label") <- chalson_labels[i]
  #}

  #df_with_labels <- rbind(chalson_labels, final_data_charlson)

  file_path <- glue("CHUM_Data/Comorbidity_{mapping_Ch}_{input_basename}.csv")
  # Write to Excel
  write.csv(final_data_charlson, file = file_path, row.names = FALSE, na = "")


  ## Create coexisting comorbidity for Charlson Scores
  cm_melted <- melt(chalrson_popICD10CA, id.vars = "id",
                    variable.name = "comorbidity", value.name = "presence")

  cm_melted <- cm_melted[cm_melted$presence == 1, ]


  #final_data_elixTest <- final_data_elixhauser %>%
  # group_by(patient_id, episode_id) %>%
  #summarise(elixhauser_icd10ca_labtns_score = mean(elixhauser_icd10ca_labtns_score, na.rm = TRUE)) %>%
  #ungroup()


  ## Calculate combined comorbidity scores of Charlson, Elixhauser

  #combined_icd10ca_labtns
  #mapping_combined = "combined_icd10ca_labtns"#"charlson_icd10ca_labtns"
  Combined_popICD10CA <- comorbidity(x = df_comrbidity, id = "id", code = "code", map = mapping_combined, assign0 = FALSE)


  final_data_combined <- Combined_popICD10CA %>%
    mutate(Combined_score := rowSums(select(., -id)))

  final_data_combined <- final_data_combined %>%
    separate(id, into = c("patient_id", "episode_id", "start_date"), sep = "_")

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
  column_names <- colnames(final_data_combined)  # Get column names

  # Loop through columns and assign labels
  #for (i in 1:length(column_names)) {
  #  attr(final_data_combined[[column_names[i]]], "label") <- combined_labels[i]
  #}

  #df_with_labels <- rbind(combined_labels, final_data_combined) # new

  file_path <- glue("CHUM_Data/Comorbidity_{mapping_combined}_{input_basename}.csv")
  # Write to Excel
  write.csv(final_data_combined, file = file_path, row.names = FALSE, na = "")



  ############ New Step

  ### Step 8: combine final scores together
  final_data_elixhauser <- final_data_elixhauser %>%
    mutate(across(c(patient_id, episode_id, start_date), as.character))

  final_data_charlson <- final_data_charlson %>%
    mutate(across(c(patient_id, episode_id, start_date), as.character))

  fr_grouped <- fr_grouped %>%
    mutate(across(c(patient_id, episode_id, start_date), as.character))

  fr_grouped_como <- fr_grouped_como %>%
    mutate(across(c(patient_id, episode_id, start_date), as.character))

  # create risk_groups and risk_measure for frailty score
  #scores_final <- scores_final %>%
  #  mutate(
  #    frailty_risk_group = case_when(
  #      frailty_score %in% c(0, 1) ~ 1,
  #      frailty_score %in% c(2, 3) ~ 2,
  #      frailty_score %in% c(4, 5) ~ 3,
  #      frailty_score %in% c(6, 7) ~ 4,
  #      frailty_score %in% c(8, 9) ~ 5,
  #      frailty_score %in% c(10, 12) ~ 6,
  #      frailty_score %in% c(13, 15) ~ 7,
  #      frailty_score >= 16 ~ 8,
  #      TRUE ~ NA_real_
#      ),
 #     frailty_risk_measure = if_else(frailty_score >= 6, 1, 0)
  #  )




  # Merge the three data frames on episode_id
  scores_final <- final_data_charlson %>%
    select(patient_id, episode_id, start_date, Charlson_score) %>%
    left_join(
      final_data_elixhauser %>%
        select(patient_id, episode_id, start_date, Elix_score),
      by = c("patient_id", "episode_id", "start_date")
    ) %>%
    left_join(
      final_data_combined %>%
        select(episode_id, Combined_score),
      by = "episode_id"
    ) %>%
    left_join(
      fr_grouped %>%
        select(patient_id, episode_id, start_date, frailty_score),
      by = c("patient_id", "episode_id", "start_date")
    ) %>%
    left_join(
      fr_grouped_como %>%
        select(patient_id, episode_id, start_date, morbi_frailty_score),
      by = c("patient_id", "episode_id", "start_date")
    )


  #write.csv(scores_final, file = "CHUM_Data/final_scores_comorbidities.csv", row.names = FALSE)
  file_path <- glue::glue("CHUM_Data/final_scores_comorbidities_{input_basename}.csv")

  write.csv(scores_final, file = file_path, row.names = FALSE)

  return(list(scores_final = scores_final,
              final_data_combined = final_data_combined,
              final_data_charlson = final_data_charlson,
              final_data_elixhauser = final_data_elixhauser))

}


