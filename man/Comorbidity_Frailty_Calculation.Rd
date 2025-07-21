\name{Comorbidity_Frailty_Calculation}
\alias{Comorbidity_Frailty_Calculation}
\title{Compute Charlson, Elixhauser and Combined Comorbidity Scores, Including Frailty Score}
\description{
This function processes patient data containing ICD-10CA codes to compute comorbidity scores
using the Elixhauser and Charlson methods. The calculated scores are then used to create
a coincidence matrix and visualize comorbidity relationships. The function also calculates
frailty scores for patients.
}
\examples{
# Process and calculate frailty scores
source("R/Comorbidity_Frailty_Calculation.R")
}
