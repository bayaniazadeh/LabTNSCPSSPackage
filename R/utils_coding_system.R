# CHUM_Code/utils_coding_system.R

get_coding_system <- function(valid_systems = c("ICD-10-CA", "ICD-10-CM", "ICD-11")) {
  repeat {
    user_input <- trimws(readline(prompt = paste0("Enter coding system (", paste(valid_systems, collapse = ", "), "): ")))

    if (user_input %in% valid_systems) {
      return(user_input)
    } else {
      cat("Invalid input. Please choose from:", paste(valid_systems, collapse = ", "), "\n")
    }
  }
}
