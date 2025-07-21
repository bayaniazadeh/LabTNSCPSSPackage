# Function to source scripts with error handling
source_with_error_handling <- function(script_path) {
  tryCatch({
    source(script_path)
  }, error = function(e) {
    message(paste("Error in script:", script_path))
    message(e)
  })
}

# List of scripts to source
scripts_to_source <- c( "R/utils_coding_system.R", "R/assign0.R", "R/check_output.R", "R/codes_to_regex.R",
                       "R/comorbidity.R", "R/labelled.R", "R/matchit.R",
                       "R/sample_diag.R", "R/tidy.R", "R/score.R",
                       "data-raw/make-mapping.R", "data-raw/make-weights.R")

# Source each script with error handling
lapply(scripts_to_source, source_with_error_handling)

# Load comorbidity maps and weights
usethis::use_data(.maps, .weights, internal = TRUE, overwrite = TRUE)
