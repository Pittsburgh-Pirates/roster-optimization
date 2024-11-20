

# pitPlayersvR <- pvtvR %>% filter(OrgCode == 'PIT') 
# playerConstraints <- data.frame(PiratesID = c("3AC9A4ED-9A4E-41D0-9E5E-050D793EB145", # Endy  
#                                               "86D4DD35-6065-4C58-B179-142B3EB1F261"), # IKF
#                                 Position = c("Total", '6'),
#                                 Operator = c("<=", "<="),
#                                 RHS = c(.5, .1))

# adjPlayerOffense <- data.frame(
#   PiratesID = c("3AC9A4ED-9A4E-41D0-9E5E-050D793EB145", # Endy  
#                "86D4DD35-6065-4C58-B179-142B3EB1F261"), # IKF,
#   split = c('vL', 'vB'),
#   adj = c(20, -50)
# )

optimize_rosters <- function(playerData, splitType = 'EPSvsR', Org = 'PIT', playerAdditions = c(), playerRemovals = c(), playerConstraints = NULL, 
                             addPlayerPositions = NULL, adjPlayerOffense = NULL){
  
  
  if(!is.null(addPlayerPositions)){
    for(i in 1:nrow(addPlayerPositions)){
      playerData[playerData$PiratesID == toupper(addPlayerPositions[i,'PiratesID']) & playerData$PositionTypeID == addPlayerPositions[i,'PositionTypeID'], 'FRAA'] <- addPlayerPositions[i,'FRAA']
    }
    
    playerData <- playerData %>% mutate(Value = RAA_635 + FRAA)
  }
  
  if(!is.null(adjPlayerOffense)){
    for(i in 1:nrow(adjPlayerOffense)){
      
      
      
      if(adjPlayerOffense$split[i] == 'vB'){
        playerData[playerData$PiratesID == toupper(adjPlayerOffense[i,'PiratesID']), 'RAA_635'] <- playerData[playerData$PiratesID == toupper(adjPlayerOffense[i,'PiratesID']), 'RAA_635'] + adjPlayerOffense[i,'adj']
      } else{
        splitCode <- ifelse(adjPlayerOffense[i,'split'] == 'vL', 'EPSvsL', ifelse(adjPlayerOffense[i,'split'] == 'vR', 'EPSvR', 'vB'))
        
        playerData[playerData$PiratesID == toupper(adjPlayerOffense[i,'PiratesID']) 
                   & playerData$SplitType == splitCode, 'RAA_635'] <- playerData[playerData$PiratesID == toupper(adjPlayerOffense[i,'PiratesID']) 
                                                                                 & playerData$SplitType == splitCode, 'RAA_635'] + adjPlayerOffense[i,'adj']
      }
      
    }
    
    playerData <- playerData %>% mutate(Value = RAA_635 + FRAA)
  }
  
  
  
  pvtSplit <- playerData %>% filter(SplitType == splitType & !(PiratesID %in% playerRemovals) & (OrgCode == Org | PiratesID %in% toupper(playerAdditions))) %>% select(-SplitType, -RAA_635, -FRAA) %>% pivot_wider(names_from = PositionTypeID, values_from = Value, values_fn = mean) 
  pvtSplit <- pvtSplit %>% filter(!(PiratesID %in% toupper(playerRemovals)))
  
  pitPlayersFilt <- pvtSplit[apply(pvtSplit %>% select('2':'10'), 1, max) >= -30,]
  total_projections <- pitPlayersFilt %>% select('2':'10')
  objective_coeffs <- as.vector((as.matrix(total_projections))) #+ 35
  
  n_players <- nrow(pitPlayersFilt)
  positions <- unique(posFrame$PositionTypeID)
  # Total number of decision variables (one for each player-position pair)
  num_vars <- n_players * length(positions)
  
  # Position constraints (100% playing time for each position)
  position_constraints <- matrix(0, nrow = length(positions), ncol = num_vars)
  for (j in 1:length(positions)) {
    
    startJ <- n_players*(j-1) + 1
    endJ <- startJ + n_players -1
    position_constraints[j,startJ:endJ] <- 1
    # position_constraints[j, seq(j, num_vars, by = length(positions))] <- 1
  }
  
  position_rhs <- rep(1, length(positions))
  
  # Player constraints (each player ≤ 95% playing time)
  player_constraints <- matrix(0, nrow = n_players, ncol = num_vars)
  for (i in 1:n_players) {
    playerIndex <- seq(from = i, by = n_players, length.out = length(positions))
    player_constraints[i, playerIndex] <- 1
  }
  player_rhs <- rep(0.95, n_players)
  
  
  # Combine constraints
  if(is.null(playerConstraints)){
    constraints <- rbind(position_constraints, player_constraints)
    rhs <- c(position_rhs, player_rhs)
    constraint_directions <- c(rep("=", length(position_rhs)), rep("<=", length(player_rhs)))
  } else{
    # Custom Consraints
    custom_constraints <- matrix(0, nrow = nrow(playerConstraints), ncol = num_vars)
    for(k in 1:nrow(playerConstraints)){
      playerID <-playerConstraints[k,'PiratesID']
      playerRow <- which(pitPlayersFilt$PiratesID == toupper(playerID))
      if(playerConstraints[k,'Position'] == 'Total'){
        customPlayerIndex <- seq(from = playerRow, by = n_players, length.out = length(positions))  
      } else{
        customPlayerIndex <- seq(from = playerRow, by = n_players, length.out = length(positions))[which(positions == playerConstraints[k,'Position'])]
      }
      
      custom_constraints[k, customPlayerIndex] <- 1
      
      
    }
    custom_rhs <- playerConstraints$RHS
    
    # Combine Constraints
    constraints <- rbind(position_constraints, player_constraints, custom_constraints)
    rhs <- c(position_rhs, player_rhs, custom_rhs)
    constraint_directions <- c(rep("=", length(position_rhs)), rep("<=", length(player_rhs)), playerConstraints$Operator)
    
  } 
  
  
  ### Optimize ### 
  result <- lp(
    direction = "max",
    objective.in = objective_coeffs,
    const.mat = constraints,
    const.dir = constraint_directions,
    const.rhs = rhs,
    all.bin = FALSE, # Set to TRUE if you want binary decision variables, FALSE for continuous
    all.int = FALSE # Set to TRUE if you want integer values only
  )
  
  res <- matrix(result$solution, nrow = nrow(total_projections), ncol = ncol(total_projections), byrow =F) %>% as_tibble()
  colSums(res)
  max(rowSums(res))
  colnames(res) <- c('C','1B','2B','3B','SS','LF','CF','RF','DH')
  res$Total <- rowSums(res)
  res$PlayerName <- pitPlayersFilt$PlayerName
  res$PiratesID <- pitPlayersFilt$PiratesID
  return(list(playing_time = res %>% relocate(PiratesID, PlayerName), playerValues = pvtSplit, obj = result$objval))
}



combineSplits <- function(vL, vR){
  res <- bind_rows(vL, vR)
  res2 <- res
  for(p in positions){
    res2[,p] <- res[,p]*ifelse(res$split == 'vR', .7, .3)
  }
  
  res_combined <- res2 %>% group_by(PiratesID, PlayerName) %>% select(-split) %>% summarise_all(sum)
  res_combined$Total <- rowSums(res_combined %>% ungroup() %>% select('C':'DH'))
  res_combined$split = 'vB'
  
  return(bind_rows(res_combined, res))
}

optimize_rosters_agg <- function(df_repeated, Org = "PIT", splitType, playerAdditions = c(), playerRemovals = c(), playerConstraints = NULL, 
                                 addPlayerPositions = NULL, adjPlayerOffense = NULL){
  rets <- lapply(1:max(df_repeated$iteration), function(iter){
    simIter <- df_repeated %>% filter(iteration == iter)
    res <- optimize_rosters(playerData = simIter, splitType = splitType, Org = Org, playerAdditions = playerAdditions, playerRemovals = playerRemovals, playerConstraints = playerConstraints, addPlayerPositions = addPlayerPositions, adjPlayerOffense = adjPlayerOffense) 
    
    resPT <- res$playing_time %>%
      mutate(split = ifelse(splitType == 'EPSvsL', 'vL','vR'),
             iteration = iter)
    return(list(playing_time = resPT, obj = res$obj))
  })
  
  playing_time <- map(rets, "playing_time")
  
  simBind <- bind_rows(playing_time) 
  simAgg <- simBind %>% group_by(PiratesID, PlayerName, split) %>% 
    summarise(across(positions, ~mean(.)))
  simAgg$Total <- rowSums(as.data.frame(simAgg)[,positions])
  
  obj <- map(rets, "obj") %>% unlist() %>% mean()
  
  adjustments <- list(
    playerAdditions = playerAdditions,
    playerRemovals = playerRemovals,
    playerConstraints = playerConstraints,
    addPlayerPositions = addPlayerPositions,
    adjPlayerOffense = adjPlayerOffense
  )
  
  return(list(playing_time = simAgg, obj = obj, adjustments = adjustments))
}

optimize_rosters_agg_all <- function(df_repeated, Org = "PIT", playerAdditions = c(), playerRemovals = c(), playerConstraints = NULL, 
                                     addPlayerPositions = NULL, adjPlayerOffense = NULL){
  roster_vR <- optimize_rosters_agg(df_repeated, "EPSvsR", Org = Org
                                    , playerAdditions = playerAdditions
                                    , playerRemovals = playerRemovals
                                    , addPlayerPositions = addPlayerPositions
                                    , playerConstraints = playerConstraints
                                    , adjPlayerOffense = adjPlayerOffense
  )
  roster_vL <- optimize_rosters_agg(df_repeated, "EPSvsL", Org = Org
                                    , playerAdditions = playerAdditions
                                    , playerRemovals = playerRemovals
                                    , addPlayerPositions = addPlayerPositions
                                    , playerConstraints = playerConstraints
                                    , adjPlayerOffense = adjPlayerOffense
  )
  
  obj <- (roster_vL$obj*.3 + roster_vR$obj*.7)
  
  combined_playing_time <-  combineSplits(roster_vL$playing_time, roster_vR$playing_time)
  return(list(obj = obj, playing_time = combined_playing_time))
}
# 
# ret <- optimize_rosters(playerData = fullProj, Org = "CWS", splitType = 'EPSvsL')
# ret$obj
# resvL <- optimize_rosters(playerData = fullProj, splitType = 'EPSvsL', playerAdditions = 'fd3dc6b8-92c4-46e2-9417-7a55e7c0ff83',
#                         playerConstraints = data.frame(PiratesID = toupper("fd3dc6b8-92c4-46e2-9417-7a55e7c0ff83"), Position = "Total", Operator = ">=", RHS = .7),
#                         addPlayerPositions = addPlayerPositions) %>%
#   mutate(split = 'vL')
# 
# resvR <- optimize_rosters(playerData = fullProj, splitType = 'EPSvsR', playerAdditions = 'fd3dc6b8-92c4-46e2-9417-7a55e7c0ff83',
#                           playerConstraints = data.frame(PiratesID = toupper("fd3dc6b8-92c4-46e2-9417-7a55e7c0ff83"), Position = "Total", Operator = ">=", RHS = .3),
#                           addPlayerPositions = addPlayerPositions) %>% 
#   mutate(split = 'vR')
# 
# res <- bind_rows(resvR, resvL)
# res2 <- res
# for(p in positions){
#   res2[,p] <- res[,p]*ifelse(res$split == 'vR', .7, .3)
# }
# 
# res_combined <- res2 %>% group_by(PiratesID, PlayerName) %>% select(-split) %>% summarise_all(sum) 
# res_combined$Total <- rowSums(res_combined %>% ungroup() %>% select('C':'DH')) 
# res_combined$split = 'vB'
# 
# res_all <- bind_rows(res, res_combined)
# write.csv(res_all, file = '~/Documents/Research/Roster_Optimization/Optimized_Rosters_PIT.csv', row.names = FALSE)
# View(res_all)



