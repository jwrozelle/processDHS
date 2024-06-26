% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanKR_foodIntake.R
\name{cleanKR_foodIntake}
\alias{cleanKR_foodIntake}
\title{Clean and Process Food Intake Data for Children}
\usage{
cleanKR_foodIntake(KRdata)
}
\arguments{
\item{KRdata}{A dataframe containing the KR dataset.}
}
\value{
A dataframe with new variables for food intake indicators, composite variables, dietary diversity scores, and minimum dietary diversity.
}
\description{
This function processes food intake data for children in the KR dataset. It creates various indicators for food intake and calculates composite variables such as vitamin A rich foods and iron rich foods. It also computes dietary diversity scores and minimum dietary diversity for children aged 6-23 months.
}
\details{
The function first checks if the dataset is from a specific country (in this case, "RW7") and warns the user if it is not. It then processes the interview date and creates basic food intake indicators. Composite variables for vitamin A rich foods and iron rich foods are created. The function also creates indicators for each food group defined in the Minimum Dietary Diversity for Infants and Young Children (MDD-IYCF) and calculates the dietary diversity score. Finally, it determines whether children aged 6-23 months meet the minimum dietary diversity criteria.
}
\note{
This function has been verified only for the "RW7" dataset. The nutrition questions are country-specific, so the function might not work correctly with datasets from other countries.
}
\examples{
\dontrun{
# Assuming KRdata is already loaded
cleaned_data <- cleanKR_foodIntake(KRdata)
}
}
