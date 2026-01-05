
source("./LABTNSCPSS_Code/setup_package.R")  # Load and install required packages
source("./LABTNSCPSS_Code/source_scripts.R")  # Source external scripts



coding_system <- get_coding_system()


invisible(sapply(list.files("LABTNSCPSS_Code", pattern = "\\.R$", full.names = TRUE), source))



input_file <- "./LABTNSCPSS_Data/testpackage.csv"
input_basename <- tools::file_path_sans_ext(basename(input_file))
cleaned_path <- paste0("LABTNSCPSS_Data/input_data_cleaned_", input_basename, ".csv")
updated_path <- paste0("LABTNSCPSS_Data/updated_episodes_carry_forward_", input_basename, ".csv")

############## Provide your data column names and coding system ############

col_mapping <- list(patient_id = "trajectoire_id",
                    ICD = "diagnostic_code",
                    start_date = "date_debut",
                    end_date = "date_fin",
                    episode_id = "episode_id")

run_pipeline <- function(input_file) {
  Create_data(input_file)
  chronic_pathologies(cleaned_path)

}

# Run the pipeline
df_result <- run_pipeline(input_file)

df_final_long <- df_result %>%
  # 1. Split comma-separated strings
  mutate(updated_icd_codes = strsplit(updated_icd_codes, ",")) %>%

  # 2. Unnest into one code per row
  unnest(updated_icd_codes) %>%

  # 3. Clean whitespace
  mutate(updated_icd_codes = trimws(updated_icd_codes)) %>%

  # 4. Remove NA, empty, or invalid codes
  filter(
    !is.na(updated_icd_codes),
    updated_icd_codes != "",
    updated_icd_codes != "character(0)"
  ) %>%

  # 5. Remove duplicates
  distinct(patient_id, start_date, updated_icd_codes, .keep_all = TRUE)

df_to_write <- df_final_long %>%
  select(patient_id, start_date, episode_id, updated_icd_codes)

write.csv(df_to_write, "LABTNSCPSS_Data/updated_episodes_carry_forward_long_format.csv", row.names = FALSE)
