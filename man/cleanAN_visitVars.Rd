% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanAN_visitVars.R
\name{cleanAN_visitVars}
\alias{cleanAN_visitVars}
\title{Clean and Harmonize Provider Variables for SPA Survey Data}
\usage{
cleanAN_visitVars(ANdata)
}
\arguments{
\item{ANdata}{A data frame containing SPA survey data with at least the 
variables `svyID` and `c024`. The variable `c024` is used as the 
provider code, and `svyID` indicates the survey country and year.}
}
\value{
A modified version of the input data frame with a new factor variable 
  \code{anProv_h}, which represents the harmonized provider categories. 
  The factor levels are labeled as follows:
  \itemize{
    \item \code{"Medical Doctors"}
    \item \code{"Advanced practice clinicians, paramedical"}
    \item \code{"Nurse, midwife"}
    \item \code{"Others—pharm, lab, dental, non-clinical"}
  }
}
\description{
The `cleanAN_visitVars` function takes a dataset from the SPA survey and 
harmonizes the provider qualification codes into a simplified and categorized 
variable (`anProv_h`). This new variable categorizes providers into four 
groups: Medical Doctors, Advanced Practice Clinicians/Paramedical Professionals, 
Nurse/Midwife, and Other (pharmacists, lab staff, dental, non-clinical).
}
\details{
The function works by first assigning provider qualifications based 
  on the `providerCode` variable, which is initialized from `c024`. 
  Different provider codes are mapped to specific provider qualifications 
  depending on the survey country (`svyID`). Then, a harmonized provider 
  category variable (`anProv_h`) is created with four levels:
  \itemize{
    \item \code{1}: Medical Doctors (e.g., generalist and specialist doctors, pediatricians, surgeons)
    \item \code{2}: Advanced Practice Clinicians, Paramedical Professionals (e.g., clinical officers, medical assistants, anesthetic assistants)
    \item \code{3}: Nurse/Midwife (e.g., registered nurses, enrolled nurses, midwives)
    \item \code{4}: Other - Pharm, Lab, Dental, Non-clinical (e.g., pharmacists, health assistants, nurse assistants)
  }

The function includes mappings for Afghanistan (`AF_SPA18`), Haiti (`HT_SPA17`), 
Malawi (`MW_SPA13`), Nepal (`NP_SPA15`), and Tanzania (`TZ_SPA14`). 
An error is raised if the `svyID` is not one of these.
}
\examples{
# Example usage
cleaned_data <- cleanAN_visitVars(ANdata)

}
