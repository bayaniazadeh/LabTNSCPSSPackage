% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{icd9_2015}
\alias{icd9_2015}
\title{ICD-9 Diagnostic Codes, 2015 Version (v32)}
\format{
A data frame with 14,567 rows and 3 variables:
\describe{
  \item{Code}{ICD-9 diagnostic code}
  \item{Long_description}{Long description of each code}
  \item{Short_description}{Short description of each code}
}
}
\source{
CMS.gov Website: https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
}
\usage{
icd9_2015
}
\description{
A dataset containing the version of the ICD-9 codes effective October 1, 2014.
}
\note{
The R code used to download and process the dataset from the CMS.gov website is available [here](https://raw.githubusercontent.com/ellessenne/comorbidity/master/data-raw/make-data.R).
}
\keyword{datasets}
