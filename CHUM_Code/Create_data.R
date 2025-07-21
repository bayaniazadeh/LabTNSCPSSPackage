#' Create CHUM Data
#'
#' This script reads raw data from a CSV file, processes it by renaming columns based on a provided
#' mapping, performs date transformations, cleans ICD codes, and saves the cleaned dataset to a new CSV file.
#'
#' @details
#' The script assumes that the input CSV file has specific columns that are mapped to standard names
#' using the `col_mapping` list. After reading the data, it performs the following transformations:
#'
#' - Renames columns based on the `col_mapping`.
#' - Parses and converts the `start_date` column to a proper Date format.
#' - Cleans up the `ICD` codes by removing periods.
#'
#' The resulting cleaned dataset is then written to `CHUM_Data/input_data_cleaned.csv`.
#'
#' @param CHUM_Data
#' @param col_mapping
#'
#' @examples
#' source('./CHUM_Code/create_CHUMData_v2.R')
#'
#' # Or you could call the read_csv function directly:
#' df <- read_csv("./CHUM_Data/data_test.csv", col_mapping)
#'
#' @note This script is intended to be sourced and executed directly. It does not return a value
#' but saves the processed data as a CSV file.
NULL


library(dplyr)
library(data.table)
library(lubridate)

# file_path : "./CHUM_Data/data_test.csv"
Create_data <-function(file_path){

  read_csv <- function(file_path, col_mapping) {
    # Read CSV file
    df <- read.csv(file_path, stringsAsFactors = FALSE)

    # Check if all user-specified columns exist in the data
    missing_cols <- setdiff(unname(col_mapping), colnames(df))

    if (length(missing_cols) > 0) {
      stop(paste("Missing columns in CSV:", paste(missing_cols, collapse = ", ")))
    }

    # Rename columns using the provided mapping
    for (std_name in names(col_mapping)) {
      user_col <- col_mapping[[std_name]]
      if (user_col %in% names(df)) {
        names(df)[names(df) == user_col] <- std_name
      }
    }

    return(df)
  }


  df <- read_csv(file_path, col_mapping)


  df <- df %>%
    mutate(start_date = as.Date(parse_date_time(start_date, orders = c("ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM", "dmy"), quiet = TRUE)))

  df <- df %>%
    mutate(end_date = as.Date(parse_date_time(end_date, orders = c("ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM", "dmy"), quiet = TRUE)))

  # Convert date_debut as Date variables
  df <- df %>%
    mutate(start_date = as.Date(start_date))  # Specify the format


  df <- df %>%
    mutate(end_date = as.Date(end_date))  # Specify the format

  df$ICD <- gsub("\\.", "", df$ICD)

  # Write the cleaned DataFrame to CSV
  output_file <- paste0("CHUM_Data/input_data_cleaned_", input_basename, ".csv")

  # Save your data frame
  fwrite(df, output_file)
  #fwrite(df, 'CHUM_Data/input_data_cleaned.csv')
}
