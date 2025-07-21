\name{Pathologies_chronic.R}

\title{Propagating Chronic Pathologies}
\description{
 \enumerate{
 This script processes patient episode data, identifies ICD codes,
  ssigns chronic categories, propagates relevant codes, and extracts basal codes. It follows a structured approach:
   \item	Load and Preprocess Data
   \item Assign Chronic Categories to ICD Codes
   \item	Identify and Propagate Category 2 Codes
   \item	Identify and Propagate Category 1 Codes
   \item	Extract Basal Codes for Category 1 ICDs
    }
}

\note{
\itemize{
Ensure that:
  \item Dates are automatically converted to YYYY-MM-DD format.
  \item	Missing category assignments are labeled as None.
 }
}
\examples{
  # To run the script:
  source('./R/Pathologie_chronic.R')
}

