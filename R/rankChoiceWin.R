
# data2use <- htSPA.list$PV
# rankVotes <- c("w172a", "w172b", "w172c")
# 
# genMD.df <- filter(data2use, w101 %in% c(1,2))
# nurseBSN.df <- filter(data2use, w101 %in% c(21, 22))
# nurseEnroll.df <- filter(data2use, w101 %in% c(25))
# men.df <- filter(data2use, w030 %in% 1)
# women.df <- filter(data2use, w030 %in% 2)


rankedChoiceWin <- function(data2use, rankVotes, missingVal = 0) {
  require(uuid)
  
  voteData.df <- data2use[rankVotes]
  voteData.df$ID <- UUIDgenerate(n = nrow(data2use))
  voteData.df$currentRank <- 1
  voteData.df$currentChoice <- voteData.df[[rankVotes[1]]]
  voteData.df$currentChoice <- as.character(voteData.df$currentChoice)
  
  # swap out the missing values 
  for (rank in rankVotes) {
    voteData.df[[rank]] <- replace(voteData.df[[rank]], voteData.df[[rank]] == missingVal, NA)
  }
  
  
  voteRound <- 1
  voteRound_tally <- as.vector(table(voteData.df$currentChoice))
  names(voteRound_tally) <- names(table(voteData.df$currentChoice))
  
  winnerProp <- max(voteRound_tally, na.rm = T) / sum(voteRound_tally, na.rm = T)
  winnerName <- names(voteRound_tally)[voteRound_tally == max(voteRound_tally, na.rm = T)]
  loserName <- names(voteRound_tally)[voteRound_tally == min(voteRound_tally, na.rm = T)]
  
  roundTallies <- list()
  roundTallies[[1]] <- sort(voteRound_tally, decreasing = T)
  
  while (winnerProp < 0.5) {
    
    loserName <- c(loserName, names(voteRound_tally)[voteRound_tally == min(voteRound_tally, na.rm = T)])
    loserName <- unique(loserName)
    
    stillLosers <- 1
    
    while (stillLosers == 1) {
      voteData.df$currentRank <- ifelse(voteData.df$currentChoice %in% loserName, voteData.df$currentRank + 1, voteData.df$currentRank)
      voteData.df$currentRank <- ifelse(voteData.df$currentRank > length(rankVotes), NA, voteData.df$currentRank)
      
      # table(voteData.df$currentRank, useNA = "ifany")
      
      for (rank in 2:max(voteData.df$currentRank, na.rm = T)) {
        voteData.df$currentChoice <- ifelse(voteData.df$currentRank %in% rank, voteData.df[[rankVotes[rank]]], voteData.df$currentChoice)
      }
      voteData.df$currentChoice <- ifelse(is.na(voteData.df$currentRank), NA, voteData.df$currentChoice)
      
      # go back again with the same loser if ther
      if (sum(voteData.df$currentChoice %in% loserName) == 0) {
        stillLosers <- 0
      }
    }
    
    
    
    # recalculate stats
    voteRound <- voteRound + 1
    voteRound_tally <- as.vector(table(voteData.df$currentChoice))
    names(voteRound_tally) <- names(table(voteData.df$currentChoice))
    
    winnerProp <- max(voteRound_tally, na.rm = T) / sum(voteRound_tally, na.rm = T)
    winnerName <- names(voteRound_tally)[voteRound_tally == max(voteRound_tally, na.rm = T)]
    
    
    
    # voteRound
    # winnerName
    # winnerProp
    # loserName
    # table(voteData.df$currentRank, useNA = "ifany")
    # table(voteData.df$currentChoice, useNA = "ifany")
    
    roundTallies[[voteRound]] <- sort(voteRound_tally, decreasing = T)
    
  }
  
  return(list(winnerName= winnerName, winnerProp = winnerProp, finalRound_tally = voteRound_tally, allTallies = roundTallies))
  
}














