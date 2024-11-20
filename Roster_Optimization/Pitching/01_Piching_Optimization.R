library(dplyr)
library(lpSolve)
library(tidyr)
library(purrr)
library(ggplot2)



### simulate pitching data ### 

# num_pitchers <- 20
# 
# pitcherFrame <- data.frame(
#   PiratesID = LETTERS[1:num_pitchers],
#   OrgCode = "PIT",
#   RA9_raw = sort(rnorm(num_pitchers, 4.50, 1.5)),
#   SP = rbinom(num_pitchers, size = 1, prob = .4)
# ) %>% mutate(
#   RA_adj = RA9_raw + .5*SP
# )

# playerData <- pitcherFrame
#### Pitcher Optimization ###### 
optimize_starting_pitching <- function(playerData, Org = 'PIT', playerAdditions = c(), playerRemovals = c(), playerConstraints = NULL, 
                             addPlayerPositions = NULL, adjPlayerOffense = NULL){
  
  
  SP_roles <- data.frame(
    role = paste0('SP', 1:5),
    IP = c(170, 165, 160, 155, 145),
    LI = 1
  ) %>% mutate(lev_IP = IP*LI)
  
  
  pvtSplit <- playerData %>% filter(SP == 1 & (OrgCode == Org | PiratesID %in% toupper(playerAdditions))) %>% select(-SP) %>% 
    arrange(PiratesID)
  
  joined <- expand.grid(PiratesID = unique(pvtSplit$PiratesID), role = SP_roles$role) %>% 
    inner_join(pvtSplit, by = 'PiratesID') %>% arrange(PiratesID, role) %>% 
    inner_join(SP_roles, by = 'role') %>% 
    mutate(levRuns = RA_adj*lev_IP/9) %>% 
    arrange(PiratesID)
  
  objective_coeffs <- as.vector((as.matrix(joined$levRuns))) 
  
  n_players <- nrow(pvtSplit)
  positions <- SP_roles$role
  # Total number of decision variables (one for each player-position pair)
  num_vars <- n_players * length(positions)
  
  # Position constraints (100% playing time for each position)
  position_constraints <- matrix(0, nrow = length(positions), ncol = num_vars)
  for (j in 1:length(positions)) {
    startJ <- length(positions)*(j-1) + 1
    posIndex <- seq(from = j, by = length(positions), length.out = n_players)
    position_constraints[j,posIndex] <- 1
    # position_constraints[j, seq(j, num_vars, by = length(positions))] <- 1
  }
  
  position_rhs <- rep(1, length(positions))
  
  # Player constraints (each player ≤ 100% playing time)
  player_constraints <- matrix(0, nrow = n_players, ncol = num_vars)
  for (i in 1:n_players) {
    playerIndex <- seq(from = (i-1)*length(positions)+1, length.out = length(positions))
    player_constraints[i, playerIndex] <- 1
  }
  player_rhs <- rep(1, n_players)
  
  
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
    direction = "min",
    objective.in = objective_coeffs,
    const.mat = constraints,
    const.dir = constraint_directions,
    const.rhs = rhs,
    all.bin = FALSE, # Set to TRUE if you want binary decision variables, FALSE for continuous
    all.int = FALSE # Set to TRUE if you want integer values only
  )
  
  
  
  res <- matrix(result$solution, nrow = nrow(pvtSplit), ncol = length(positions), byrow =T) %>% as_tibble()
  colSums(res)
  max(rowSums(res))
  colnames(res) <- SP_roles$role
  res$Total <- rowSums(res)
  res$PlayerName <- pvtSplit$PlayerName
  res$PiratesID <- pvtSplit$PiratesID
  
  pvtSplit$IP <- as.matrix(res[,1:nrow(SP_roles)]) %*% SP_roles$IP
  pvtSplit$LI <- as.matrix(res[,1:nrow(SP_roles)]) %*% SP_roles$LI
  pvtSplit$Lev_Innings <- pvtSplit$IP*pvtSplit$LI
  
  return(list(playerValues = pvtSplit, obj = result$objval))
}


optimize_relief_pitching <- function(playerData, Org = 'PIT', playerAdditions = c(), playerRemovals = c(), playerConstraints = NULL, 
                             addPlayerPositions = NULL, adjPlayerOffense = NULL){
  
  
  RP_roles <- data.frame(
    role = paste0('RP', 1:7),
    IP = c(55, 52, 48, 43, 40, 37, 50),
    LI = c(1.4, 1.2, 1.1, 1.05, .95, .8, .4)
  ) %>% mutate(
    lev_IP = IP*LI
  )
  
  
  pvtSplit <- playerData %>% filter((OrgCode == Org | PiratesID %in% toupper(playerAdditions))) %>% select(-SP) %>%
    arrange(PiratesID)
  
  joined <- expand.grid(PiratesID = unique(pvtSplit$PiratesID), role = RP_roles$role) %>% 
    inner_join(pvtSplit, by = 'PiratesID') %>% arrange(PiratesID, role) %>% 
    inner_join(RP_roles, by = 'role') %>% 
    mutate(levRuns = RA_adj*lev_IP/9) %>% 
    arrange(PiratesID)
  
  objective_coeffs <- as.vector((as.matrix(joined$levRuns))) 
  
  n_players <- nrow(pvtSplit)
  positions <- RP_roles$role
  # Total number of decision variables (one for each player-position pair)
  num_vars <- n_players * length(positions)
  
  # Position constraints (100% playing time for each position)
  position_constraints <- matrix(0, nrow = length(positions), ncol = num_vars)
  for (j in 1:length(positions)) {
    startJ <- length(positions)*(j-1) + 1
    posIndex <- seq(from = j, by = length(positions), length.out = n_players)
    position_constraints[j,posIndex] <- 1
    # position_constraints[j, seq(j, num_vars, by = length(positions))] <- 1
  }
  
  position_rhs <- rep(1, length(positions))
  
  # Player constraints (each player ≤ 100% playing time)
  player_constraints <- matrix(0, nrow = n_players, ncol = num_vars)
  for (i in 1:n_players) {
    playerIndex <- seq(from = (i-1)*length(positions)+1, length.out = length(positions))
    player_constraints[i, playerIndex] <- 1
  }
  player_rhs <- rep(1, n_players)
  
  
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
    direction = "min",
    objective.in = objective_coeffs,
    const.mat = constraints,
    const.dir = constraint_directions,
    const.rhs = rhs,
    all.bin = FALSE, # Set to TRUE if you want binary decision variables, FALSE for continuous
    all.int = FALSE # Set to TRUE if you want integer values only
  )
  
  
  
  res <- matrix(result$solution, nrow = nrow(pvtSplit), ncol = length(positions), byrow =T) %>% as_tibble()
  colSums(res)
  max(rowSums(res))
  colnames(res) <- RP_roles$role
  res$Total <- rowSums(res)
  res$PlayerName <- pvtSplit$PlayerName
  res$PiratesID <- pvtSplit$PiratesID
  
  pvtSplit$IP <- as.matrix(res[,1:nrow(RP_roles)]) %*% RP_roles$IP
  pvtSplit$LI <- as.matrix(res[,1:nrow(RP_roles)]) %*% RP_roles$LI
  pvtSplit$Lev_Innings <- pvtSplit$IP*pvtSplit$LI
  
  return(list(playerValues = pvtSplit, obj = result$objval))
}



optimize_pitching <- function(playerData, Org = 'PIT', playerAdditions = c(), playerRemovals = c(), playerConstraints = NULL){
  
  # playerData <- pitcherFrame
  SP_opt <- optimize_starting_pitching(playerData, Org = Org
                                       , playerAdditions = playerAdditions
                                       , playerRemovals = playerRemovals
                                       , playerConstraints = playerConstraints)
  
  playerData2 <- playerData %>% anti_join(SP_opt$playerValues %>% filter(IP > 0) %>% select(PiratesID), by = 'PiratesID') %>% 
    mutate(RA_adj = RA_adj - SP*.5) # remove SP adjustment for pitching out of pen
  RP_opt <- optimize_relief_pitching(playerData2, Org = Org
                                     , playerAdditions = playerAdditions
                                     , playerRemovals = playerRemovals
                                     , playerConstraints = playerConstraints)
  
  
  pitcher_playing_time <- bind_rows(
  SP_opt$playerValues %>% mutate(SP = 1) %>% filter(IP > 0),
  RP_opt$playerValues %>% mutate(SP = 0)
  )  %>% arrange(desc(Lev_Innings)) %>% 
    mutate(Runs = RA_adj*Lev_Innings)
  
  obj <- sum(pitcher_playing_time$Runs)
  return(list(pitcher_playing_time = pitcher_playing_time, obj = obj))
}


optimize_pitching_agg <- function(df_repeated, Org = "PIT", playerAdditions = c(), playerRemovals = c(), playerConstraints = NULL){
  rets <- lapply(1:max(df_repeated$iteration), function(iter){
    simIter <- df_repeated %>% filter(iteration == iter)
    res <- optimize_pitching(playerData = simIter, Org = Org, playerAdditions = playerAdditions, playerRemovals = playerRemovals, playerConstraints = playerConstraints) 
    
    resPT <- res$pitcher_playing_time %>%
      mutate(iteration = iter)
    return(list(playing_time = resPT, obj = res$obj))
  })
  
  playing_time <- map(rets, "playing_time")
  
  simBind <- bind_rows(playing_time) 
  simAgg <- simBind %>% group_by(PiratesID, PlayerName) %>% 
    summarise(RA_adj = mean(RA_adj),
              SP = mean(SP),
              IP = mean(IP),
              LI = mean(LI),
              Lev_Innings = mean(Lev_Innings),
              Runs = mean(Runs))
  
  obj <- map(rets, "obj") %>% unlist() %>% mean()
  
  adjustments <- list(
    playerAdditions = playerAdditions,
    playerRemovals = playerRemovals,
    playerConstraints = playerConstraints
  )
  
  return(list(playing_time = simAgg, obj = obj, adjustments = adjustments))
}

