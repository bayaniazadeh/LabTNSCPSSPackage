
source("./CHUM_Code/setup_package.R")  # Load and install required packages
source("./CHUM_Code/source_scripts.R")  # Source external scripts

#source("./CHUM_Code/chronic_pathologies_v2.R")

coding_system <- get_coding_system()


invisible(sapply(list.files("CHUM_Code", pattern = "\\.R$", full.names = TRUE), source))


#input_file <- "./CHUM_Data/data_test1.csv", mimic_test2.csv
input_file <- "./CHUM_Data/testpackage.csv"
input_basename <- tools::file_path_sans_ext(basename(input_file))
cleaned_path <- paste0("CHUM_Data/input_data_cleaned_", input_basename, ".csv")
updated_path <- paste0("CHUM_Data/updated_episodes_carry_forward_", input_basename, ".csv")

############## Provide your data column names and coding system ############
#col_mapping <- list(patient_id = "nam",
#                    ICD = "cim",
#                    start_date = "start_date",
#                    end_date = "end_date",
#                    episode_id = "episode_id")

col_mapping <- list(patient_id = "trajectoire_id",
                    ICD = "diagnostic_code",
                    start_date = "date_debut",
                    end_date = "date_fin",
                    episode_id = "episode_id")

run_pipeline <- function(input_file) {
  Create_data(input_file)
  chronic_pathologies(cleaned_path)
  frailty_results <- Frailty_Calculation(updated_path)

  fr_grouped <- frailty_results$fr_grouped
  fr_grouped_como <- frailty_results$fr_grouped_como


  # Capture results and convert to data frames
  comorbidity_results <- Comorbidity_Frailty_Calculation(updated_path, fr_grouped, fr_grouped_como)#updated_episodes_V2.csv
  comorbidity_results <- lapply(comorbidity_results, function(x) {
    if (!is.data.frame(x)) as.data.frame(x) else x
  })

  return(comorbidity_results)
}

# Run the pipeline
df_result <- run_pipeline(input_file)

list2env(df_result, envir = .GlobalEnv)

write.xlsx(final_data_charlson, "my_data.xlsx", rowNames = FALSE)

#Create_data(input_file)
#chronic_pathologies("CHUM_Data/input_data_cleaned.csv")
#fr_grouped <- Frailty_Calculation('CHUM_Data/updated_episodes_V2.csv')
#Comorbidity_Frailty_Calculation("CHUM_Data/episode_test.csv")

