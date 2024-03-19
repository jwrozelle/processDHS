
# FCdata <- htSPA.list$FC

cleanFC_fees <- function(FCdata) {
  
  # Any Fees
    # label define V144    
    # 0 "No"
    # 1 "Yes, fixed fee covering all services"
    # 2 "Yes, separate fees for separate items"
  
  FCdata$clientFees_any <- NA 
  FCdata$clientFees_any <- ifelse(FCdata$v144 == 0, 0, FCdata$clientFees_any)
  FCdata$clientFees_any <- ifelse(FCdata$v144 == 1 | FCdata$v144 == 2, 1, FCdata$clientFees_any)
  
  # Fixed fees
  FCdata$clientFees_fixed <- NA
  FCdata$clientFees_fixed <- ifelse(FCdata$v144 == 1, 1, 0)
  
  # Service specific fees
  FCdata$clientFees_perService <- NA
  FCdata$clientFees_perService <- ifelse(FCdata$v144 == 2, 1, 0)
  
  
  # Service specific fees
  # Client fee:Consultation
  FCdata$fee_consultation <- ifelse(FCdata$v144b == 1, 1, 0)
  # Client fee:Medications
  FCdata$fee_medications <- ifelse(FCdata$v144e == 1, 1, 0)
  # Client fee:Any tests
  FCdata$fee_tests <- ifelse(FCdata$v144f == 1, 1, 0)
  # Client fee:Registration
  FCdata$fee_registration <- ifelse(FCdata$v144g == 1, 1, 0)
  # Client fee:Vaccines
  FCdata$fee_vaccines <- ifelse(FCdata$v144j == 1, 1, 0)
  # Client fee:Contraceptive commodities
  FCdata$fee_contraceptives <- ifelse(FCdata$v144k == 1, 1, 0)
  # Client fee:Normal deliveries
  FCdata$fee_normalDeliveries <- ifelse(FCdata$v144l == 1, 1, 0)
  # Client fee:Syringes and needles
  FCdata$fee_syringesNeedles <- ifelse(FCdata$v144m == 1, 1, 0)
  # Client fee:Cesarean section
  FCdata$fee_cesareanSection <- ifelse(FCdata$v144n == 1, 1, 0)
  # Client fee:HIV diagnostic test
  FCdata$fee_hivDiagnostic <- ifelse(FCdata$v144o == 1, 1, 0)
  # Client fee:Malaria rapid diagnostic test
  FCdata$fee_malariaRDT <- ifelse(FCdata$v144p == 1, 1, 0)
  # Client fee:Malaria microscopy
  FCdata$fee_malariaMicroscopy <- ifelse(FCdata$v144q == 1, 1, 0)
  # Client fee:Other lab tests
  FCdata$fee_otherLabTests <- ifelse(FCdata$v144r == 1, 1, 0)
  # Client fee:ARVs for treatment
  FCdata$fee_arvsTreatment <- ifelse(FCdata$v144s == 1, 1, 0)
  # Client fee:ARVs for PMTCT (Prevention of Mother-To-Child Transmission)
  FCdata$fee_arvsPMTCT <- ifelse(FCdata$v144t == 1, 1, 0)
  # Client fee:Minor surgical procedures
  FCdata$fee_minorSurgery <- ifelse(FCdata$v144u == 1, 1, 0)
  
  
  # what if client cannot pay?
  #   Exempt / discounted payment - no payment expected
  FCdata$nopay_exempt <- NA
  FCdata$nopay_exempt <- ifelse(FCdata$clientFees_any == 0, 0, FCdata$v145a) # This will be 0 if there are no client fees
  #   Exempt / discounted payment - expected later
  FCdata$nopay_later <- NA
  FCdata$nopay_later <- ifelse(FCdata$clientFees_any == 0, 0, FCdata$v145b) # This will be 0 if there are no client fees
  #   Service refused / ask to come back later
  FCdata$nopay_refused <- NA
  FCdata$nopay_refused <- ifelse(FCdata$clientFees_any == 0, 0, FCdata$v145c) # This will be 0 if there are no client fees
  #   Accept payment in kind
  FCdata$nopay_inKind <- NA
  FCdata$nopay_inKind <- ifelse(FCdata$clientFees_any == 0, 0, FCdata$v145d) # This will be 0 if there are no client fees
  #   Other
  FCdata$nopay_other <- NA
  FCdata$nopay_other <- FCdata$v145x
  
  return(FCdata)
  
}














