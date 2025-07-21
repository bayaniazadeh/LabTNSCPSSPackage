# LabTNS CPSS Package

This R package implements a pipeline to process clinical episode data, identify chronic pathologies, and calculate frailty and comorbidity scores based on patient diagnosis codes.

---

## Overview

The pipeline performs the following steps:

1. **Setup environment**  
   Load and install required R packages and source supporting scripts.

2. **Data Preparation**  
   Clean and format input episode data based on user-specified column mappings.

3. **Chronic Pathologies Identification**  
   Apply algorithms to detect and propagate chronic conditions within episodes.

4. **Frailty Calculation**  
   Calculate frailty indices from updated episode data.

5. **Comorbidity and Frailty Summary**  
   Combine frailty and comorbidity measures into final result tables.

---

## Usage

### Input data

- The input dataset should be a CSV file with episode-level patient data.
- Required columns (default mapping):  
  - `Patient_id` — patient ID  
  - `ICD` — ICD coding system diagnosis codes  
  - `start_date` — episode start date  
  - `end_date` — episode end date  
  - `episode_id` — unique episode identifier  

You can customize these column names by modifying the `col_mapping` list in the pipeline.

### Running the pipeline

1. Place your input CSV in `CHUM_Data/`, e.g., `CHUM_Data/testpackage.csv`

2. Run the pipeline from your R session:

```r
source("./CHUM_Code/setup_package.R")      # Load/install packages
source("./CHUM_Code/source_scripts.R")     # Load pipeline functions

input_file <- "./CHUM_Data/testpackage.csv"

df_result <- run_pipeline(input_file)

