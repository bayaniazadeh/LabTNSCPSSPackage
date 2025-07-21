# Main Execution
run_pipeline <- function(input_file) {
  df <- load_data(input_file)
  codes_df <- load_code_categories()
  codes_df$ICD10CAcodes <- as.character(codes_df$ICD10CAcodes)
  df <- sort_episodes(df)
  codes_dict <- create_code_dict(codes_df)
  df_summary <- process_icd_categories(df, codes_dict)
  df_final <- process_category2(df_summary)
  return(df_final)
}

# Example Execution
df_result <- run_pipeline("CHUM_Data/input_data_cleaned.csv")
