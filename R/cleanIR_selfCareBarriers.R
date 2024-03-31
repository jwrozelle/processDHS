

cleanIR_selfCareBarriers <- function(IRdata) {
  if (IRdata$v000[1] == "MW7") {
    
    
    #    0 "No problem" (None in data)
    #    1 "Big problem"
    #    2 "Not a big problem"
    
    # label variable v467a    "NA - Getting medical help for self: know where to go"
    IRdata.df$selfMedBarrier_where <- NA
    # IRdata.df$selfMedBarrier_where <- ifelse(IRdata.df$v467a == 1, 1, 0)
    
    # label variable v467b    "Getting medical help for self: getting permission to go"
    IRdata.df$selfMedBarrier_permission <- NA
    IRdata.df$selfMedBarrier_permission <- ifelse(IRdata.df$v467b == 1, 1, 0)
    
    # label variable v467c    "Getting medical help for self: getting money needed for treatment"
    IRdata.df$selfMedBarrier_money <- NA
    IRdata.df$selfMedBarrier_money <- ifelse(IRdata.df$v467c == 1, 1, 0)
    
    # label variable v467d    "Getting medical help for self: distance to health facility"
    IRdata.df$selfMedBarrier_distance <- NA
    IRdata.df$selfMedBarrier_distance <- ifelse(IRdata.df$v467d == 1, 1, 0)
    
    # label variable v467e    "NA - Getting medical help for self: having to take transport"
    IRdata.df$selfMedBarrier_transport <- NA
    # IRdata.df$selfMedBarrier_transport <- ifelse(IRdata.df$v467e == 1, 1, 0)
    
    # label variable v467f    "Getting medical help for self: not wanting to go alone"
    IRdata.df$selfMedBarrier_alone <- NA
    IRdata.df$selfMedBarrier_alone <- ifelse(IRdata.df$v467f == 1, 1, 0)
    
    # label variable v467g    "Getting medical help for self: concern no female health provider"
    IRdata.df$selfMedBarrier_providerFemale <- NA
    IRdata.df$selfMedBarrier_providerFemale <- ifelse(IRdata.df$v467g == 1, 1, 0)
    
    # label variable v467h    "Getting medical help for self: concern no provider"
    IRdata.df$selfMedBarrier_provider <- NA
    IRdata.df$selfMedBarrier_provider <- ifelse(IRdata.df$v467h == 1, 1, 0)
    # label variable v467i    "Getting medical help for self: concern no drugs available"
    #    0 "No problem" (None in data)
    #    1 "Big problem"
    #    2 "Not a big problem"
    IRdata.df$selfMedBarrier_drugs <- NA
    IRdata.df$selfMedBarrier_drugs <- ifelse(IRdata.df$v467i == 1, 1, 0)
    
    data()
    
    
    
  }
  
  return(IRdata)
  
}
