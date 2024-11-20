#' this simulation works. I want to make the sims be the same for each player across handedness in each iteration (I think). 
#' I should think about if this matters or we can just treat them as separate samples.
#' I also want to do a more informed simulation to look at how variance changes by player.

# injury_rate <- .1
# sampleVar <- 6
# 
# numSamples <- 100

# TODO: Add in injury simulation
talent_simulation <- function(fullProj, sampleVar=6, numSamples = 50){
  df_repeated <- replicate(numSamples, fullProj, simplify = FALSE) %>% bind_rows()
  
  df_repeated$RAA_635 <- df_repeated$RAA_635 + rnorm(nrow(df_repeated), mean = 0, sd = sampleVar) 
  df_repeated$Value <- df_repeated$RAA_635 + df_repeated$FRAA
  df_repeated$iteration <- sort(rep(1:numSamples, nrow((fullProj))))
  
  return(df_repeated)
}

df_repeated <- talent_simulation(fullProj, sampleVar = 6)

# addPlayerPositions <- data.frame(
#   PiratesID = "2dac7862-21dd-4926-8452-075757d9cccb",
#   PositionTypeID = 3,
#   FRAA = -2
# )
# roster_vR <- optimize_rosters_agg(df_repeated, "EPSvsR", Org = 'PIT'
#                                   , playerAdditions = c('fd3dc6b8-92c4-46e2-9417-7a55e7c0ff83', "935038e0-1b8a-4f00-b976-19eacc042aaa")
#                                   , addPlayerPositions = data.frame(
#                                       PiratesID = "2dac7862-21dd-4926-8452-075757d9cccb",
#                                       PositionTypeID = 3,
#                                       FRAA = -2
#                                       )
#                                   )
# roster_vL <- optimize_rosters_agg(df_repeated, "EPSvsL", Org = 'PIT'
#                                   , playerAdditions = c('fd3dc6b8-92c4-46e2-9417-7a55e7c0ff83'
#                                                         , "935038e0-1b8a-4f00-b976-19eacc042aaa"
#                                                         )
#                                   , addPlayerPositions = data.frame(
#                                     PiratesID = "2dac7862-21dd-4926-8452-075757d9cccb",
#                                     PositionTypeID = 3,
#                                     FRAA = -2
#                                   )
# )
# 
# roster_vL$playing_time
# roster_vL$adjustments
# 
# obj_base <- roster_vL$obj
# 
# write.csv(roster_vL$playing_time, file = 'PIT_Optimal_Roster_Judge_vL.csv', row.names = FALSE)

# playerAdditions = c('fd3dc6b8-92c4-46e2-9417-7a55e7c0ff83', "935038e0-1b8a-4f00-b976-19eacc042aaa")
# playerConstraints = data.frame(PiratesID = toupper("fd3dc6b8-92c4-46e2-9417-7a55e7c0ff83"), Position = "Total", Operator = ">=", RHS = .7),
# addPlayerPositions = addPlayerPositions


