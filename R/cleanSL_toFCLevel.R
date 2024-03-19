

cleanSL_toFCLevel <- function(SLdata) {
  
  # require survey identifier
  if (!"svyID" %in% names(SLdata) | length(unique(SLdata$svyID)) > 1) {
    stop("Must include a column svyID with identical values for all observations")
  }
  
  # make facID variable if there is not one
  if(!"facID" %in% names(SLdata)) {
    SLdata$facID <- SLdata$inv_id
  }
  
  # Haiti 2017
  if (SLdata$svyID[1] == "HT_SPA17") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(21, 22 ,24), na.rm=TRUE), # does not include auxiliary nurses / medical assistants
                surgeon = sum(vu13 %in% c(1,2) & vu27 == 1, na.rm=TRUE) # medical doctor that provides surgery
      )
    
  # Haiti 2013
  } else if (SLdata$svyID[1] == "HT_SPA13") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2, 3), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(21, 22), na.rm = TRUE), # does not include auxiliary nurses / medical assistants
                surgeon = sum((vu13 %in% c(1,3) & vu27 == 1) | (vu13 %in% c(2)), na.rm=TRUE), # medical doctor that provides surgery
                surgeon2 = sum(vu13 == 2, na.rm = TRUE)
      )
    
  # Afghanistan 2018
  } else if (SLdata$svyID[1] == "AF_SPA18") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2), na.rm = TRUE), # 1 Genearlist, 2 Specialist medical doctor
                nurse_bsn = sum(vu13 %in% c(22), na.rm = TRUE), # does not include registered nurse with diploma, or community nurse
                surgeon = sum((vu13 %in% c(1,2) & vu27 == 1), na.rm = T) # medical doctor that provides surgery
      )
  # Malawi 2013-14
  } else if (SLdata$svyID[1] == "MW_SPA13") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(21, 22), na.rm = TRUE), # does NOT include registered nurse with diploma, enrolled nurse, or psychiatric nurse
                surgeon = sum((vu13 %in% c(1,2) & vu27 == 1), na.rm = T) # medical doctor that provides surgery
      )
  # Nepal 2015
  } else if (SLdata$svyID[1] == "NP_SPA15") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(15), na.rm = TRUE), # Note that nepal has one label: NURSE/AUXILLARY NURSE MIDWIFE (ANM)
                surgeon = sum((vu13 %in% c(1, 2, 3, 4, 5, 6, 8) & vu27 == 1) | (vu13 %in% c(5)), na.rm = T) # medical doctor that provides surgery
      )
    
  # Tanzania 2014
  } else if (SLdata$svyID[1] == "TZ_SPA14") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(21), na.rm = TRUE), # Includes registered nurse, does not include enrolled nurse
                surgeon = sum((vu13 %in% c(1, 2) & vu27 == 1), na.rm = T) # medical doctor that provides surgery
      )
    
  } else {
      stop("Value in svyID not recognized")
  }
  
  return(SLdata_FCLevel)
}




























