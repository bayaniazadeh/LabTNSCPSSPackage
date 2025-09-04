

### Step 1: Read Dataset from CSV file
# Read the dataset that contains patient ICD codes and nam
X_CHUM <- fread("CHUM_Data/episode_test.csv", sep = ",")


### Step 2: Data Preparation
# Rename columns based on the data structure in your input:
# - 'cim' refers to ICD codes
# - 'nam' refers to patient IDs

X_CHUM1 <- data.table(
  id = 1:length(X_CHUM$patient_id),       # Create a unique ID for each row
  updated_icd_codes = X_CHUM$updated_icd_codes,  # Assign ICD codes to a new column
  patient_id = X_CHUM$patient_id,
  start_date = X_CHUM$start_date,
  episode_id = X_CHUM$episode_id# Assign patient IDs to a new column
)


# Step 3: Remove rows with missing or 'NA' values in the 'updated_icd_codes' column

df_combined <- X_CHUM1#[!is.na(updated_icd_codes) & updated_icd_codes != "NA", ]


# Step 5: Prepare data for comorbidity function
# Convert the combined ICD codes data into a format required by the comorbidity function.

df_combined <- data.frame(id = paste(df_combined$patient_id, df_combined$episode_id, sep = "_"),
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
mapping = "elixhauser_icd10ca_labtns"# "elixhauser_icd10ca_labtns"
elixhauser_popICD10CA <- comorbidity(x = df_comrbidity, id = "id", code = "code", map = mapping, assign0 = FALSE)


final_data_elixhauser <- elixhauser_popICD10CA %>%
  mutate(!!paste0(mapping, "_score") := rowSums(select(., -id)))


final_data_elixhauser <- final_data_elixhauser %>%
  separate(id, into = c("patient_id", "episode_id"), sep = "_")

# Write the final data

file_path <- glue("CHUM_Data/Comorbidity_{mapping}.csv")
file_path

write.csv(final_data_elixhauser, file = file_path, row.names = FALSE) # or filtered_df

## Create coexisting comorbidity
cm_melted <- melt(elixhauser_popICD10CA, id.vars = "id",
                  variable.name = "comorbidity", value.name = "presence")

cm_melted <- cm_melted[cm_melted$presence == 1, ]

# Create the coincidence matrix by finding comorbidity pairs for each patient
coincidence_pairs <- ddply(cm_melted, c('id'), function(x) {
  if (nrow(x) > 1) {
    # Generate all possible pairs of comorbidities
    data.frame(t(combn(as.character(x$comorbidity), 2)))
  } else {
    return(NULL)  # Return NULL if less than 2 comorbidities
  }
})

#  Remove any NULL results (patients with less than 2 comorbidities)
coincidence_pairs <- na.omit(coincidence_pairs)

t_elix <- table(coincidence_pairs[c('X1', 'X2')])

t_elix <- t_elix[order(rownames(t_elix)), order(colnames(t_elix))]
# Save comorbidity coincidence Table for Elix:
write.csv(t_elix, file = "CHUM_Data/ELixhauser_coincidence_table.csv", row.names = FALSE) # or filtered_df


m <- melt(t_elix)

p <- ggplot(m[m$value > 0,], aes(X1, X2)) +
  stat_sum(aes(group = value), geom = "point", alpha = 0.5) +  # Adjust fill and alpha as needed
  labs(title = "Elixhauser Coincidence Matrix Plot",
       x = "Elixhauser 1",
       y = "Elixhauser 2") +
  theme_minimal()

# Save the plot to a file
ggsave("CHUM_Data/Elixhauser_coincidence_matrix_plot.png", plot = p, width = 8, height = 6, dpi = 300)



# weight = readmission_elix_hcup, ou mortality_elix_hcup (pour utiliser les poids fournis par HCUP)

if (mapping == "elixhauser_icd10ca_labtns") {
  # Assuming elixhauser_popICD10CA is already defined in your environment
  score_pop_Elixh <- score(x = elixhauser_popICD10CA, weights = "readmission_elix_hcup", assign0 = FALSE)

}


### Step 7: Calculate Charlson Comorbidity Score
mapping = "charlson_icd10ca_labtns"#"charlson_icd10ca_labtns"
chalrson_popICD10CA <- comorbidity(x = df_comrbidity, id = "id", code = "code", map = mapping, assign0 = FALSE)


final_data_charlson <- chalrson_popICD10CA %>%
  mutate(!!paste0(mapping, "_score") := rowSums(select(., -id)))


final_data_charlson <- final_data_charlson %>%
  separate(id, into = c("patient_id", "episode_id"), sep = "_")

# Write the final data


chalson_labels <- c(labels <- c(
  "Patient ID",
  "Episode ID",
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
  "Charlson ICD-10CA comorbidity score"))


# Assign labels using a loop
column_names <- colnames(final_data_charlson)  # Get column names

# Loop through columns and assign labels
for (i in 1:length(column_names)) {
  attr(final_data_charlson[[column_names[i]]], "label") <- chalson_labels[i]
}

for (i in 1:length(column_names)){
  #print(i)
  attr(final_data_charlson[column_names[i]], "label") <- chalson_labels[i]
}


file_path <- glue("CHUM_Data/Comorbidity_{mapping}.csv")
file_path

write.csv(final_data_charlson, file = file_path, row.names = FALSE)

## Create coexisting comorbidity for Charlson Scores
cm_melted <- melt(chalrson_popICD10CA, id.vars = "id",
                  variable.name = "comorbidity", value.name = "presence")

cm_melted <- cm_melted[cm_melted$presence == 1, ]

# Create the coincidence matrix by finding comorbidity pairs for each patient
coincidence_pairs <- ddply(cm_melted, c('id'), function(x) {
  if (nrow(x) > 1) {
    # Generate all possible pairs of comorbidities
    data.frame(t(combn(as.character(x$comorbidity), 2)))
  } else {
    return(NULL)  # Return NULL if less than 2 comorbidities
  }
})

#  Remove any NULL results (patients with less than 2 comorbidities)
coincidence_pairs <- na.omit(coincidence_pairs)

t <- table(coincidence_pairs[c('X1', 'X2')])
t
t <- t[order(rownames(t)), order(colnames(t))]

# Save comorbidity coincidence Table for Charlson:
write.csv(t, file = "CHUM_Data/Charlson_coincidence_table.csv", row.names = FALSE) # or filtered_df


m <- melt(t)

p <- ggplot(m[m$value > 0,], aes(X1, X2)) +
  stat_sum(aes(group = value), geom = "point", alpha = 0.5) +  # Adjust fill and alpha as needed
  labs(title = "Charlson Coincidence Matrix Plot",
       x = "Comorbidity 1",
       y = "Comorbidity 2") +
  theme_minimal()

# Save the plot to a file
ggsave("CHUM_Data/Charlson_coincidence_matrix_plot.png", plot = p, width = 8, height = 6, dpi = 300)

final_data_elixTest <- final_data_elixhauser %>%
  group_by(patient_id, episode_id) %>%
  summarise(elixhauser_icd10ca_labtns_score = mean(elixhauser_icd10ca_labtns_score, na.rm = TRUE)) %>%
  ungroup()


## Calculate combined comorbidity scores of Charlson, Elixhauser

#combined_icd10ca_labtns
mapping = "combined_icd10ca_labtns"#"charlson_icd10ca_labtns"
Combined_popICD10CA <- comorbidity(x = df_comrbidity, id = "id", code = "code", map = mapping, assign0 = FALSE)


final_data_combined <- Combined_popICD10CA %>%
  mutate(!!paste0(mapping, "_score") := rowSums(select(., -id)))

final_data_combined <- final_data_combined %>%
  separate(id, into = c("patient_id", "episode_id"), sep = "_")

# Write the final data

file_path <- glue("CHUM_Data/Comorbidity_{mapping}.csv")
file_path

write.csv(final_data_charlson, file = file_path, row.names = FALSE)


############ New Step

### Step 8: combine final scores together

scores_final <- final_data_elixhauser %>%
  select(patient_id, episode_id, elixhauser_icd10ca_labtns_score) %>%
  left_join(final_data_charlson %>%
              select(patient_id, episode_id, charlson_icd10ca_labtns_score),
            c("patient_id", "episode_id")) %>%
  left_join(fr_grouped %>%
              select(patient_id, episode_id, frailty_score),
            by = c("patient_id", "episode_id"))



# create risk_groups and risk_measure for frailty score
scores_final <- scores_final %>%
  mutate(
    frailty_risk_group = case_when(
      frailty_score %in% c(0, 1) ~ 1,
      frailty_score %in% c(2, 3) ~ 2,
      frailty_score %in% c(4, 5) ~ 3,
      frailty_score %in% c(6, 7) ~ 4,
      frailty_score %in% c(8, 9) ~ 5,
      frailty_score %in% c(10, 12) ~ 6,
      frailty_score %in% c(13, 15) ~ 7,
      frailty_score >= 16 ~ 8,
      TRUE ~ NA_real_
    ),
    frailty_risk_measure = if_else(frailty_score >= 6, 1, 0)
  )




# Merge the three data frames on episode_id
final_scores <- final_data_charlson %>%
  select(episode_id, charlson_icd10ca_labtns_score) %>%
  left_join(final_data_elixhauser %>% select(episode_id, elixhauser_icd10ca_labtns_score), by = "episode_id") %>%
  left_join(final_data_combined %>% select(episode_id, combined_icd10ca_labtns_score), by = "episode_id")

write.csv(final_scores, file = "CHUM_Data/final_scores_v3.csv", row.names = FALSE)



