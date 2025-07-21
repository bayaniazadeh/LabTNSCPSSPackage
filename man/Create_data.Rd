\name{create_CHUMData_v2}
\alias{create_CHUMData_v2}
\title{Create CHUM Data}
\description{
  This script reads raw data from a CSV file, processes it by renaming columns based on a provided
  mapping, performs date transformations, cleans ICD codes, and saves the cleaned dataset to a new CSV file.
}
\details{
  The script assumes that the input CSV file has specific columns that are mapped to standard names
  using the `col_mapping` list. After reading the data, it performs the following transformations:

  - Renames columns based on the `col_mapping`.
  - Parses and converts the `start_date` column to a proper Date format.
  - Cleans up the `ICD` codes by removing periods.

  The resulting cleaned dataset is then written to `CHUM_Data/input_data_cleaned.csv`.
}
\note{
  This script is intended to be sourced and executed directly. It does not return a value
  but saves the processed data as a CSV file.
}
\examples{
  # To run the script:
  source('./R/create_CHUMData_v2.R')
}
