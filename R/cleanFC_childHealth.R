

cleanFC_childHealth <- function(FCdata) {
  
  # Service availability
  SA_vars <- c(
    "svc_curativeCareChild", # Preventive and curative care for children under 5
    "svc_childMalnutritionDxTx", # Malnutrition diagnosis and treatment
    "svc_vitaminASupp", # Vitamin A supplementation
    "svc_ironSupp", # Iron supplementation
    "svc_zincSupp", # ORS and zinc supplementation
    "svc_growthMonitor", # Growth monitoring
    # Treatment of pneumonia
    "med_amoxicillinsyr", # Administration of amoxicillin for the treatment of pneumonia in children
    "svc_malariaDxTx", # item in SARA reference specifies in children, but this is only available in the staff questionnaire # Treatment of malaria in children
    )
  
  # Service readiness
  SR_vars <- c(
    # Guidelines for IMCI
    # Guidelines for growth monitoring
    # Staff trained in IMCI
    # Staff trained in growth monitoring
    
  )
  
  
}







