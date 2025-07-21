# Install packages if not installed
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if (length(new_packages)) install.packages(new_packages)
}

# List of required packages
required_packages <- c("usethis", "devtools", "tidyr", "data.table", "glue", 
                       "plyr", "reshape2", "ggplot2", "dplyr", "tidyr", 
                       "openxlsx", "lubridate")

# Install missing packages
install_if_missing(required_packages)

# Load required packages
lapply(required_packages, library, character.only = TRUE)
