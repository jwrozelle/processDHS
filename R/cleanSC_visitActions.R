

cleanSC_visitActions <- function(SCdata) {
  
  # physical exam
  # c205 RECORD WHETHER A PROVIDER PERFORMED ANY OF THE FOLLOWING PHYSICAL EXAMINATIONS ON THE SICK CHILD


  #** label variable c205g    "Physical exam: Check for pallor by looking at conjunctiva or mouth"
  SCdata$exam_pallor <- NA
  SCdata$exam_pallor <- ifelse(SCdata$c205f == 1 | SCdata$c205g == 1, 1, 0)
  
  
  #** c205a "Physical exam: temperature by thermometer"
  SCdata$pe_feverTherm <- NA
  SCdata$pe_feverTherm <- ifelse(SCdata$c205a %in% 1, 1, 0)
  # c205b "Physical exam: feel for fever or hotness"
  SCdata$pe_feverFeel <- NA
  SCdata$pe_feverFeel <- ifelse(SCdata$c205b %in% 1, 1, 0)
  #** c205c "Physical exam: Count respirations"
  SCdata$pe_respCount <- NA
  SCdata$pe_respCount <- ifelse(SCdata$c205c %in% 1, 1, 0)
  #   auscultate child (stethoscope) OR # c205d "Physical exam: Auscultate child"
  SCdata$pe_steth <- NA
  SCdata$pe_steth <- ifelse(SCdata$c205d %in% 1, 1, 0)
  # c205e "Physical exam: Check turgor for dehydration"
  SCdata$pe_turgor <- NA
  SCdata$pe_turgor <- ifelse(SCdata$c205e %in% 1, 1, 0)
  #   Check pallor by looking at palms
  SCdata$pe_pallorPalms <- NA
  SCdata$pe_pallorPalms <- ifelse(SCdata$c205f %in% 1, 1, 0)
  #   Check for pallor by looking at conjunctiva or mouth
  SCdata$pe_pallorMouth <- NA
  SCdata$pe_pallorMouth <- ifelse(SCdata$c205g %in% 1, 1, 0)
  #   Look in ear
  SCdata$pe_lookInEar <- NA
  SCdata$pe_lookInEar <- ifelse(SCdata$c205h %in% 1, 1, 0)
  #   feel behind ear
  SCdata$pe_feelBehindEar <- NA
  SCdata$pe_feelBehindEar <- ifelse(SCdata$c205i %in% 1, 1, 0)
  #   undress child to examine
  SCdata$pe_undress <- NA
  SCdata$pe_undress <- ifelse(SCdata$c205j %in% 1, 1, 0)
  #   Press feet to look for edema
  SCdata$pe_edema <- NA
  SCdata$pe_edema <- ifelse(SCdata$c205k %in% 1, 1, 0)
  #   Weigh the child
  SCdata$pe_weigh <- NA
  SCdata$pe_weigh <- ifelse(SCdata$c205m %in% 1, 1, 0)
  #   Plot weight on growth chart
  SCdata$pe_plotWeight <- NA
  SCdata$pe_plotWeight <- ifelse(SCdata$c205n %in% 1, 1, 0)
  #   Look in child's mouth
  SCdata$pe_lookMouth <- NA
  SCdata$pe_lookMouth <- ifelse(SCdata$c205p %in% 1, 1, 0)
  #   Check the neck for stiffness
  SCdata$pe_checkNeck <- NA
  SCdata$pe_checkNeck <- ifelse(SCdata$c205q %in% 1, 1, 0)
  #   Check for enlarged lymphnodes
  SCdata$pe_checkLymph <- NA
  SCdata$pe_checkLymph <- ifelse(SCdata$c205r %in% 1, 1, 0)
  #   Any physical exam check
  SCdata$pe_anyCheck <- NA
  SCdata$pe_anyCheck <- ifelse(SCdata$c205y %in% 0, 1, 0)
  
  # variable names
  peVars <- c(
    "pe_feverTherm",
    "pe_feverFeel",
    "pe_respCount",
    "pe_steth",
    "pe_turgor",
    "pe_pallorPalms",
    "pe_pallorMouth",
    "pe_lookInEar",
    "pe_feelBehindEar",
    "pe_undress",
    "pe_edema",
    "pe_weigh",
    "pe_plotWeight",
    "pe_lookMouth",
    "pe_checkNeck",
    "pe_checkLymph"
  )
  
  SCdata$pe_visitActions_count <- NA
  SCdata$pe_visitActions_count <- rowSums(SCdata[,peVars], na.rm = T)
  
  # Get the complaints index
  mca_result <- ExPosition::epMCA(SCdata[peVars], graphs = F)
  # create cp index
  SCdata$visitAction_index <- mca_result$ExPosition.Data$fi[,1]
  
  if (cor(SCdata$visitAction_index, SCdata$pe_visitActions_count) < 0) {
    SCdata$visitAction_index <- -SCdata$visitAction_index
  }
  
  # SCdata$symptoms_index <- scale(SCdata$symptoms_index)
  SCdata$visit_index_pct <- Hmisc::cut2(SCdata$symptoms_index, g = 3) |> as.numeric()
  
  return(SCdata)
  
}



