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
### Installation

You can either **clone the repository** or **download the ZIP file**.

```bash
git clone https://github.com/bayaniazadeh/LabTNSCPSSPackage.git
cd LabTNSCPSSPackage
```

## Running the pipeline

### Running the pipeline

1. Place your input CSV in `LABTNSCPSS_Data/`, e.g., `LABTNSCPSS_Data/testpackage.csv`

2. Go to your directory that the LabTNSCPSSPackage folder exists and open the file `LabTNSCPSSPackage.Rproj`

In the file edit these information: 
- The input dataset should be a CSV file with episode-level patient data.
- Required columns (default mapping):  
  - `Patient_id` — patient ID  
  - `ICD` — ICD coding system diagnosis codes  
  - `start_date` — episode start date  
  - `end_date` — episode end date  
  - `episode_id` — unique episode identifier  

You can customize these column names by modifying the `col_mapping` list in the pipeline.

2. Run the pipeline from your R session line by line:

```r
source("./LABTNSCPSS_Code/setup_package.R")      # Load/install packages
source("./LABTNSCPSS_Code/source_scripts.R")     # Load pipeline functions

coding_system <- get_coding_system()
```
Here you should select the ICD vesion of according to your data: ICD-10-CA, ICD-10-CM, ICD-11, write it in the console part and press enter. 

3. Run the rest of the code line by line. 

4. Finally you can find the generated files at `LABTNSCPSS_Data/`

## Important resources

 In the folder `data/` you can find all the mapping files and categorizations in ".rda"" format. To be able to explore the mappings in your R studio browser use this code, and replace "file_name" with your desired data file : 
 
```r
df <- as.data.frame(file_name)
View(df)
```
