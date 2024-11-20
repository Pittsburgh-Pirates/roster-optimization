players <- c(
  "c1afb3ca-8e83-42d6-a970-775e9e52f96c" # Austin Shenton
  ,  "920aebff-8966-4696-aa37-275a785fd06b" # Taylor Ward
  , "38147acb-e01f-4dba-bd59-5fa9adf7834e" # Alec Bohm
  , "eb7c0429-883e-4e38-b76e-3bd894e136f4" # Yandy Diaz
  , "72c66910-6dd5-4810-95cc-f244faea5d2c" # Brandon Lowe
  , "d2b8d21c-3359-43eb-a39b-89d21a4bfa63" # Nate Lowe
  , "72f94915-e477-4e2e-b2c8-b7febe2484a2" # Brandon Marsh
  , "1b6e4920-d730-42b3-8f0d-4f1559638cfa" # Ryan Mountcastle
  , "0c329903-4730-4c20-bd5c-de6d7b599ee7" # Josh Naylor
  , "da6c5b56-34b2-4bd0-a417-6c9da345e58a" # Luis Rengifo
  , "fdaf8b60-1165-42b6-b3b1-d2340ff42dbb" # Pavin Smith
  , "1c40ff76-048b-4ff2-914a-70a0856f0f06" # Bryson Stott
  , "82fbd839-c165-45bc-81fb-51bc52a1d12f" # Lane Thomas
  , "a31d7683-ce8d-429b-9fbe-dab5d1dcfe34" # Lamonte Wade
)


addPlayerPositions <- data.frame(
  PiratesID = c(#"2dac7862-21dd-4926-8452-075757d9cccb",
                "50ec0fe4-8960-4188-a1a3-36aae400e1e3",
                 "48b0cda6-abea-40ec-8120-c7ee12ceb3e6",
                "c1afb3ca-8e83-42d6-a970-775e9e52f96c"),
  PositionTypeID = c(#3,
                     8,
                     8,
                     3),
  FRAA = c(#-2, 
           -7, 
           5,
           -9)
)

playerConstraints <- data.frame(
  PiratesID = "48b0cda6-abea-40ec-8120-c7ee12ceb3e6",
  Position = '6',
  Operator = "<=",
  RHS = 0
)

adjPlayerOffense <- data.frame(
  PiratesID = c("72f94915-e477-4e2e-b2c8-b7febe2484a2","72f94915-e477-4e2e-b2c8-b7febe2484a2"),
  split = c('vL', 'vR'),
  adj = c(7, -3)
)




base <- optimize_rosters_agg_all(df_repeated, Org = 'PIT'
                                  , playerAdditions = c('fd3dc6b8-92c4-46e2-9417-7a55e7c0ff83')
                                 , playerRemovals = '4ceb4b6e-f394-4027-b84e-6f02fb0b758a'
                                 
                                 # , addPlayerPositions = data.frame(
                                 #   PiratesID = c("2dac7862-21dd-4926-8452-075757d9cccb", "50ec0fe4-8960-4188-a1a3-36aae400e1e3"),
                                 #   PositionTypeID = c(3, 8),
                                 #   FRAA = c(-2, -7)
                                 # )
                                 , addPlayerPositions = addPlayerPositions[1:3,]
                                 , playerConstraints = playerConstraints
                                 # , adjPlayerOffense = adjPlayerOffense
                                 )


# write.csv(allBase, "basePT.csv", row.names = FALSE)


player_output <- lapply(head(players,1), function(pl){
  player_name <- df_repeated %>% filter(PiratesID == toupper(pl)) %>% pull(PlayerName) %>% unique()
  cat(player_name, '\n')
  # pl <- "c1afb3ca-8e83-42d6-a970-775e9e52f96c"
  pl <- c("fcbbe69a-e6ea-464d-ade7-76606878be83", "920aebff-8966-4696-aa37-275a785fd06b")
  roster <- optimize_rosters_agg_all(df_repeated, Org = 'PIT'
                           , playerAdditions = c('fd3dc6b8-92c4-46e2-9417-7a55e7c0ff83', pl)
                           , playerRemovals = '4ceb4b6e-f394-4027-b84e-6f02fb0b758a'
                           , addPlayerPositions = addPlayerPositions
                           , playerConstraints = playerConstraints)
  
  
  marginalGain <- roster$obj - base$obj
  
  pt_diff <- roster$playing_time %>% filter(split == 'vB') %>% inner_join(base$playing_time %>% filter(split == 'vB'), by = c('PlayerName','PiratesID', 'split')) %>% 
    mutate(Diff = Total.x - Total.y)  %>% arrange((Diff)) %>% select(PiratesID, PlayerName, Playing_Time_new = Total.x, Playing_Time_old = Total.y, Diff) %>% 
    mutate(diff_text = paste0(PlayerName, round(Diff*100), "%"))
  
  movers <- paste0(pt_diff %>% filter(abs(Diff) > .05) %>% top_n(5) %>% pull(diff_text), collapse = ', ')
  
  return(list(PiratesID = pl, PlayerName = player_name,runs = marginalGain, pt_diff = pt_diff, movers = movers))
})


player_output
output_frame <- data.frame(
    PlayerName = unlist(map(player_output, "PlayerName"))
  , MarginalRuns = unlist(map(player_output, "runs"))
  , BiggestMovers = unlist(map(player_output, "movers"))
  
)

# write.csv(output_frame, "Player_Additions.csv", row.names = FALSE)

# 
orgs <- unique(fullProj %>% filter(!(OrgCode %in% c('KBO','BOC','NPB', "NULL"))) %>% pull(OrgCode))
# paste0("c('", paste0(sort(orgs), collapse = "', '"), "')")


findOrgBaselines <- function(orgs, playerAdditions = c(), playerConstraints = NULL, 
                             addPlayerPositions = NULL, adjPlayerOffense = NULL){
  orgBase <- lapply(orgs, function(org){
    cat(org, '\n')
    roster <- optimize_rosters_agg_all(df_repeated = df_repeated
                                       , Org = org
                                       , playerAdditions = playerAdditions
                                       , addPlayerPositions = addPlayerPositions
                                       , playerConstraints = playerConstraints
                                       , adjPlayerOffense = adjPlayerOffense
    )
    
    roster$playing_time$Org <- org
    return(roster)
  })
  
  orgBaselines <- data.frame(Org = orgs, runVal = unlist(map(orgBase, "obj")))
  orgPT <- map(orgBase, "playing_time") %>% bind_rows()
  
  
  return(list(obj = orgBaselines, playing_time = orgPT))
}

findPlayerImpacts <- function(players
                              , orgBaselines, orgs = c('ARI', 'ATH', 'ATL', 'BAL', 'BOS', 'CHI', 'CIN', 'CLE', 'COL', 'CWS', 'DET', 'HOU', 'KC', 'LA', 'LAA', 'MIA', 'MIL', 'MIN', 'NY', 'NYY', 'PHI', 'PIT', 'SD', 'SEA', 'SF', 'STL', 'TB', 'TEX', 'TOR', 'WSH')
                              , playerAdditions = c(), playerConstraints = NULL, 
                              addPlayerPositions = NULL, adjPlayerOffense = NULL
                              ){
  playerImpacts <- lapply(players, function(pl){
    player_name <- df_repeated %>% filter(PiratesID == toupper(pl)) %>% pull(PlayerName) %>% unique()
    orgRuns <- lapply(orgs, function(org){
      cat(player_name, "-", org, "\n")
      roster <- optimize_rosters_agg_all(df_repeated = df_repeated
                                         , Org = org
                                         , playerAdditions = c(playerAdditions, pl)
                                         , addPlayerPositions = addPlayerPositions
                                         , playerConstraints = playerConstraints
                                         , adjPlayerOffense = adjPlayerOffense
      )
      
      roster$player_time <- roster$playing_time %>% filter(PiratesID == toupper(pl)) %>% mutate(Org = org)
      roster$plate_appearances <- (roster$playing_time %>% filter(PiratesID == toupper(pl) & split == 'vB') %>% mutate(Org = org) %>% pull(Total))*635
      return(roster)
    })
    
    
    
    
    orgFrame <- data.frame(Org = orgs, 
                           PiratesID = pl,
                           PlayerName = player_name,
                           PA = unlist(map(orgRuns, "plate_appearances")),
                           runVal = unlist(map(orgRuns, "obj")))
    
    return(orgFrame)
}) %>% bind_rows()
  player_run_values <- playerImpacts %>% inner_join(orgBaselines, by = 'Org', suffix = c('_addition', '_baseline')) %>% 
    mutate(runsGained = runVal_addition - runVal_baseline)
  return(player_run_values)
}

orgBaselines <- findOrgBaselines(orgs = orgs)
lgAvgBaseline <- mean(orgBaselines$obj$runVal)
orgWinProj <- orgBaselines$obj %>% mutate(RAA = runVal - lgAvgBaseline,
                        WAA = RAA/10,
                        Wins = 81 + WAA) 


pos_lkup <- data.frame(
  Position = positions,
  PosID = 2:10
)
pvtPT <- (orgBaselines$playing_time) %>% pivot_longer(cols = 'C':'DH', names_to = "Position", values_to = 'playing_time') %>% 
  inner_join(pos_lkup, by = 'Position') %>% filter(split != 'vB')
### TODO: Find percentiles of player values at each position
impacts <- pvtPT %>% select(-Total) %>% 
  inner_join(fullProj %>% filter(SplitType != 'EPS') %>% select(PiratesID, PlayerName, Value, SplitType, PosID = PositionTypeID, Value) %>% mutate(split = ifelse(SplitType == 'EPSvsL', 'vL','vR')), by = c('PiratesID', 'split', 'PlayerName', 'PosID')) %>% 
  group_by(PiratesID, PlayerName) %>% 
  filter(Value >= -100) %>%
  summarise( playing_time = sum(ifelse(split == 'vL', .3, .7)*playing_time)
            , total_value = sum(ifelse(split == 'vL', .3, .7)*playing_time*Value)) 


ggplot(data = impacts, aes(x = playing_time, y = total_value)) + geom_point()

impacts %>% ungroup() %>% 
  mutate(player_group = case_when(
    between(playing_time, .05, .2) ~ 'Depth',
    between(playing_time, .4, .6) ~ 'Avg',
    between(playing_time, .6, .8) ~ 'Abv_Avg',
    between(playing_time, .8, 1) ~ 'Elite',
    .default = 'Other')
    ) %>% 
  group_by(player_group) %>% 
  summarise(val = mean(total_value))

write.csv(orgWinProj, file = "Org_Win_Probs.csv", row.names = FALSE)

player_run_values <- findPlayerImpacts(pl, orgBaselines, 'PIT', addPlayerPositions = data.frame(
  PiratesID = c("2dac7862-21dd-4926-8452-075757d9cccb", "50ec0fe4-8960-4188-a1a3-36aae400e1e3"),
  PositionTypeID = c(3, 8),
  FRAA = c(-2, -7)
)
# , adjPlayerOffense = adjPlayerOffense
)

avg_run_vals <- player_run_values %>% group_by(PiratesID, PlayerName) %>% summarise(AvgRuns = mean(runsGained),
                                                                                    SDRuns = sd(runsGained))

avg_run_vals %>% inner_join(player_run_values %>% filter(Org == 'PIT'), by = c('PiratesID','PlayerName')) %>% View()

ggplot(data = avg_run_vals, aes(x = sqrt(AvgRuns), y = SDRuns)) + geom_point()
