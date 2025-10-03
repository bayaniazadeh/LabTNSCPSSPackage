#' A Package for Chronic Pathology and Comorbidity Analysis
#'
#' This package provides tools for preprocessing patient data, managing chronic pathologies,
#' calculating frailty scores, and computing comorbidity indices (Charlson and Elixhauser).
#' The package sources various scripts to automate these processes.
#'
#' @section Main Scripts:
#' - \code{setup_package.R}: Installs and loads required dependencies.
#' - \code{source_scripts.R}: Sources external scripts required for data processing.
#' - \code{create_Data.R}: Cleans and prepares input data.
#' - \code{Pathologie_chronic.R}: Updates episodes with chronic pathology information.
#' - \code{comorbidity_ICD10CA_v2.R}: Calculates comorbidity indices.
#'
#' @section Usage:
#' The package should be used by sourcing the main functions in the order specified above.
#'
#' @docType _PACKAGE
#' @name LabTNSCPSSPackage
#' @importFrom utils read.csv write.csv
#' @importFrom dplyr mutate filter select
#' @importFrom ggplot2 ggplot aes geom_point
NULL
