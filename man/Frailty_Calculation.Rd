\name{Frailty_Calculation}
\alias{Frailty_Calculation}
\title{Calculating Frailty Scores}

\description{
This script processes patient episode data, assigns frailty categories to ICD codes, and calculates a frailty score based on categorized ICD codes.

The script follows these main steps:
\enumerate{
  \item Load and preprocess patient episode data from a CSV file.
  \item Load the ICD-to-frailty mapping data.
  \item Remove dots from ICD codes to standardize format.
  \item Sort data by patient ID and start date.
  \item Assign frailty categories based on ICD codes.
  \item Prioritize exact matches, otherwise use prefix-based matching.
  \item Calculate the frailty score as the sum of all relevant frailty categories per patient episode.
  \item Export processed data to a CSV file.
}
}

\examples{
# Process and calculate frailty scores
source("R/Frailty_score.R")
}
