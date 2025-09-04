
library(dplyr)
library(data.table)
library(lubridate)

source('CHUM_Code/ProcessPatientFile.R')


# Read T0 information using fread
df_t0 <- fread('./CHUM_Data/premier_episode.csv') 

# Read main DataFrame using fread
df <- process_csv_data('./CHUM_Data/chum_hanche_comorbidites_espum_2024-10-01.csv') 

# Convert date_t0 to Date (removing time part)
df_t0 <- df_t0 %>%
  mutate(date_t0 = as.Date(date_t0))


df <- df %>%
  mutate(date_debut = parse_date_time(date_debut, orders = "dmy", quiet = TRUE))


# Convert date_debut to Date like date_t0
df <- df %>%
  mutate(date_debut = as.Date(date_debut))  # Specify the format


merged_df <- df %>%
  left_join(df_t0, by = c("nam" = "nam",  "date_debut" = "date_t0" ))



# Update date_t0 when episode_id is not null
# Create a new column date_t0_final based on the condition
final_df <- merged_df %>%
  mutate(date_t0 = ifelse(!is.na(episode_id), date_debut, NA)) %>%
  select(nam, date_debut, episode_id, date_t0, everything())


# Convert date_t0 to Date (removing time part)
final_df <- final_df %>%
  mutate(date_t0 = as.Date(date_t0))



# Filter out rows where date_t0 is not NA, just select the first fracture 
#filtered_df <- final_df %>%
#  filter(!is.na(date_t0))

# Write the cleaned DataFrame to CSV
fwrite(final_df, 'CHUM_Data/chum_hanche_comorbidites_espum_2024_cleaned.csv')
