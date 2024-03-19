# BRdata <- BRdata.list$IA7
# BRdata <- BRdata.list$HT7
# BRdata <- BRdata.list$MW7


# # Make sure to pull full family lists
# BRdata$femaleID <- NA
# 
# BRdata$femaleID <- paste(BRdata$v001, BRdata$v002, BRdata$v003, sep="_")
# 
# femaleID.vec <- unique(BRdata$femaleID)
# 
# 
# BRdata <- dplyr::filter(BRdata, femaleID %in% femaleID.vec[1:5000])



cleanBR_mortalityLoop <- function(BRdata, n.cores = NULL) {
  
  if (!"U5_death" %in% names(BRdata)) {
    stop("You must process ", BRdata$v000[1], " with cleanBR before using this function.")
  }
  
  if (!is.null(n.cores)) {
    # Step 0: Create a new environment for parallel computation
    envForParallel <- new.env()
  }
  
  require(labelled)
  require(dplyr)
  require(iotools)
  if (!is.null(n.cores)) {
    require(parallel)
  }
  
  BRdata_looper.df <- BRdata[c(
    "femaleID", 
    "childID",
    "do_death", 
    "neonatal_death", 
    "infant_death",
    "U5_death",
    "c_dobR",
    "bidx",
    "rh_del_fbd",
    "rh_del_skilledDelivery",
    "bord",
    # "multiBirthOrder",
    "alive"
    
  )]
  
  # initialize blank variables
  BRdata_looper.df$prec_anyDeath <- 0
  BRdata_looper.df$prec_neoDeath <- 0
  BRdata_looper.df$prec_infDeath <- 0
  BRdata_looper.df$prec_u5Death <- 0
  # first facility based delivery
  BRdata_looper.df$first_fbd <- NA
  # First birth skilled delivery
  BRdata_looper.df$first_skilledDelivery <- NA
  
  BRdata_looper.df <- BRdata_looper.df %>%
    arrange(femaleID, bord)
  
  # subset in advance
  BRdata_looper.list <- lapply(unique(BRdata_looper.df$femaleID), function(f_id) {
    BR_f_sub <- dplyr::filter(BRdata_looper.df, femaleID == f_id) |> as.data.frame()
    return(BR_f_sub)
  })
  
  # parallelize, if requested
  
  if (is.null(n.cores)) {
    
    PrecedingPredictors <- lapply(BRdata_looper.list, function(BR_f_sub) {
    # PrecedingPredictors <- lapply(unique(BRdata_looper.df$femaleID), function(f_id) {
      
      # # get the subset for each female ID
      # BR_f_sub <- dplyr::filter(BRdata_looper.df, femaleID == f_id)
      # 
      # # sort by birth order
      # BR_f_sub <- BR_f_sub[order(BR_f_sub$bord),]
      # 
      # # first facility based delivery
      # BR_f_sub$first_fbd <- (NA)
      
      
      if (nrow(BR_f_sub) > 1 ) {
        firstBirth.df <- dplyr::filter(BR_f_sub, bord == 1)
        BR_f_sub$first_fbd[2:nrow(BR_f_sub)] <- ifelse(rep(firstBirth.df$rh_del_fbd, nrow(BR_f_sub)-1) == 1, TRUE, FALSE)
        BR_f_sub$first_skilledDelivery[2:nrow(BR_f_sub)] <- ifelse(rep(firstBirth.df$rh_del_skilledDelivery, nrow(BR_f_sub)-1) == 1, TRUE, FALSE)
      }
      
      BR_f_sub$first_fbd <- as.logical(BR_f_sub$first_fbd)
      
      # If the family has at least one death
      if (sum(BR_f_sub$alive == 0) > 0) { 
        
        # then for each birth
        for(birth in 2:nrow(BR_f_sub)) {
          # get deaths in birth history that precede child's birth
          preceding_deaths.df <- dplyr::filter(BR_f_sub, do_death < BR_f_sub$c_dobR[birth])
          
          # preceding neonatal deaths
          if(sum(preceding_deaths.df$neonatal_death, na.rm = T) > 0) {
            BR_f_sub$prec_neoDeath[birth] <- 1
          }
          # preceding infant deaths
          if(sum(preceding_deaths.df$infant_death, na.rm = T) > 0) {
            BR_f_sub$prec_infDeath[birth] <- 1
          }
          
          # preceding u5 deaths
          if(sum(preceding_deaths.df$U5_death, na.rm = T) > 0) {
            BR_f_sub$prec_u5Death[birth] <- 1
          }
          
        }
        
      } 
      
      # View(BR_f_sub[c("bidx", "c_dobR", "do_death", "b5", "b0")]) #!!!
      # View(preceding_deaths.df[c("bidx", "c_dobR", "do_death", "b5", "b0")]) #!!!
      
      return(BR_f_sub)
      
      
    })
  } else {

    
    ## Step 1: Create a cluster of child processes 
    
    cl <- makeCluster(n.cores)
    ## Step 2: Load the necessary R package(s)
    ## N.B. length(cl) is the number of child processes
    ##      in the cluster 
    par.setup <- parLapplyLB( cl, 1:length(cl),
                              function(xx) {
                                require(labelled)
                                require(dplyr)
                              })
    ## Step 3: Distribute the necessary R objects 
    clusterExport(cl, varlist =  c('BRdata_looper.list'), envir = envForParallel)
    ## Step 4: Do the computation
    PrecedingPredictors <- parLapplyLB(cl, BRdata_looper.list, function(BR_f_sub) {
      
      # f_id <- "101_45_4" # India
      
      # PrecedingPredictors <- lapply(unique(BRdata_looper.df$femaleID), function(f_id) {
      
      # # get the subset for each female ID
      # BR_f_sub <- dplyr::filter(BRdata_looper.df, femaleID == f_id)
      # 
      # # sort by birth order
      # BR_f_sub <- BR_f_sub[order(BR_f_sub$bord),]
      # 
      # # first facility based delivery
      # BR_f_sub$first_fbd <- (NA)
      
      
      if (nrow(BR_f_sub) > 1 ) {
        firstBirth.df <- dplyr::filter(BR_f_sub, bord == 1)
        BR_f_sub$first_fbd[2:nrow(BR_f_sub)] <- ifelse(rep(firstBirth.df$rh_del_fbd, nrow(BR_f_sub)-1) == 1, TRUE, FALSE)
        BR_f_sub$first_skilledDelivery[2:nrow(BR_f_sub)] <- ifelse(rep(firstBirth.df$rh_del_skilledDelivery, nrow(BR_f_sub)-1) == 1, TRUE, FALSE)
      }
      
      BR_f_sub$first_fbd <- as.logical(BR_f_sub$first_fbd)
      
      # If the family has at least one death
      if (sum(BR_f_sub$alive == 0) > 0) { 
        
        # then for each birth
        for(birth in 2:nrow(BR_f_sub)) {
          # get deaths in birth history that precede child's birth
          preceding_deaths.df <- dplyr::filter(BR_f_sub, do_death < BR_f_sub$c_dobR[birth])
          
          # preceding neonatal deaths
          if(sum(preceding_deaths.df$neonatal_death, na.rm = T) > 0) {
            BR_f_sub$prec_neoDeath[birth] <- 1
          }
          # preceding infant deaths
          if(sum(preceding_deaths.df$infant_death, na.rm = T) > 0) {
            BR_f_sub$prec_infDeath[birth] <- 1
          }
          
          # preceding u5 deaths
          if(sum(preceding_deaths.df$U5_death, na.rm = T) > 0) {
            BR_f_sub$prec_u5Death[birth] <- 1
          }
          
        }
        
      } 
      
      # View(BR_f_sub[c("bidx", "c_dobR", "do_death", "b5", "b0")]) #!!!
      # View(preceding_deaths.df[c("bidx", "c_dobR", "do_death", "b5", "b0")]) #!!!
      
      return(BR_f_sub)
      
      
    })
    
    
    
    
    ## Step 5: Remember to stop the cluster!
    stopCluster(cl)
  }
  
  # rm(BRdata_looper.df, BRdata_looper.list)
  
  
  PrecedingPredictors.df <- iotools::fdrbind(PrecedingPredictors)
  rm(PrecedingPredictors)
  
  # my function remove the duplicate columns
  PrecedingPredictors.df <- preMergeTrim(BRdata, PrecedingPredictors.df, by = "childID")
  
  
  BRdata <- merge(BRdata, 
                  PrecedingPredictors.df,
                  by = "childID",
                  all.x = T,
                  all.y = F
  )
  
  rm(PrecedingPredictors.df)
  
  return(BRdata)
  
}