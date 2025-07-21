
library(dplyr)
library(lubridate)
library(data.table)
library(purrr)


# Read the updated episode data with chronic pathologies
df <- fread('CHUM_Data/updated_episodes_V2.csv', sep = ',', encoding = 'UTF-8')

# Read the correspondent table of ICD codes- frailty categories
# Codes: ICD_10_CA_LabTNS, Category: frailty_Comorbidity
df_fr <- read.csv('CHUM_Data/Frailty_ICD10_CA_Mapping.csv', sep = ',', fileEncoding = 'UTF-8')
df_fr <- na.omit(df_fr)

data("Frailty_mapping")
df_fr <- Frailty_mapping



# Load the correspondent table containing ICD - chronic categories

data("Code_Categories")
codes_df <- get("Code_Categories")




################################  Preprocessing the data  #########################

# clean icd codes
remove_dots <- function(x) {
  gsub("\\.", "", x)
}

# Convert 'Code' column to character type and remove dots
df_fr$ICD_10_CA_LabTNS <- as.character(df_fr$ICD_10_CA_LabTNS)
df_fr$ICD_10_CA_LabTNS <- remove_dots(df_fr$ICD_10_CA_LabTNS)


# Sort data by 'Patient_id' and 'start_date'
df_sorted <- df %>%
  arrange(patient_id, start_date)


filtered_df <- df_sorted #[!is.na(df_sorted$date_t0), ]


################################### Calculate Frailty scores ###################

# Get unique categories from df_fr
unique_categories <- unique(df_fr$frailty_Comorbidity)

# Add new columns to filtered_df for each category with initial values of 0, prefixed with "fr_"
for (category in unique_categories) {
  filtered_df[[category]] <- 0
}

# Function to update filtered_df based on df_fr, prioritizing exact matches and skipping partial match if exact found
update_columns <- function(df_fr, filtered_df) {
  # Create a named list where each category is associated with its codes
  category_codes_list <- split(df_fr$ICD_10_CA_LabTNS, df_fr$frailty_Comorbidity)

  # Ensure all unique categories are columns in filtered_df and initialized to 0
  unique_categories <- names(category_codes_list)
  for (category in unique_categories) {
    if (!(category %in% colnames(filtered_df))) {
      filtered_df[[category]] <- 0
    }
  }

  # Iterate over each row in filtered_df
  for (i in seq_len(nrow(filtered_df))) {

    # Get the list of codes in the current row
    codes <- unlist(filtered_df$updated_icd_codes[[i]])  # Convert list to a flat vector

    # Check each code in 'codes'
    for (code in codes) {
      # Loop through each category and check for matches
      for (category in unique_categories) {
        # Get all codes for the current category
        category_codes <- category_codes_list[[category]]

        # Check for an exact match first
        if (code %in% category_codes) {
          filtered_df[[category]][i] <- 1
          break  # Skip to the next code since exact match was found
        } else {
          # If no exact match, fall back to a three-character prefix match
          code_prefix <- substr(code, 1, 3)
          category_code_prefixes <- substr(category_codes, 1, 3)

          if (any(code_prefix == category_code_prefixes)) {
            filtered_df[[category]][i] <- 1
          }
        }
      }
    }
  }

  return(filtered_df)
}
# Apply the function
filtered_df <- update_columns(df_fr, filtered_df)


### Check sum of a frailty score
#sum(filtered_df$`Cardiac and vascular`, na.rm = TRUE)

# Have to convert columns containing lists before writing in csv!


# Compute frailty score by summing only the selected columns from unique_categories
frailty_pop2 <- filtered_df %>%
  mutate(frailty_score = rowSums(select(., all_of(unique_categories)), na.rm = TRUE))  # Exclude NA values

frailty_pop2 <- frailty_pop2 %>%
  mutate(across(where(is.list), ~ sapply(., function(x) paste(unlist(x), collapse = ","))))



# Write the final data, could be used for verifications
file_path <- "CHUM_Data/frailty_codes_V2.csv"

write.csv(frailty_pop2, file = file_path, row.names = FALSE) # or filtered_df

############### Calculate the final score
# Select columns based on unique_categories
frag_final <- frailty_pop2 %>%
  select(patient_id, start_date, episode_id, all_of(unique_categories))


# You can proceed with the rest of your operations as needed
fr_grouped <- frag_final %>%
  select(patient_id, start_date, episode_id, all_of(unique_categories)) %>%  # Select episode_id and columns in unique_categories
  mutate(frailty_score = rowSums(select(., all_of(unique_categories)), na.rm = TRUE))  # Calculate frailty_score




