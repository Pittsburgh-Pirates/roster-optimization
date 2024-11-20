setwd("D:/OneDrive - Pittsburgh Associates/Downloads/Roster_Optimization")
# Source position player files
source("Position_Players/00_Process_Data.R")
source("Position_Players/01_Simulate_Seasons.R")
source("Position_Players/Roster_Optimization.R")
# Source Pitching Files
source("Pitching/00_Load_Pitching_Data.R")
source("Pitching/01_Piching_Optimization.R")
source("Pitching/02_Simulate_Pitching.R")

### Add Player Parameters ###
# Position Players
addPositionPlayers <- c(
  # ""
  "1c40ff76-048b-4ff2-914a-70a0856f0f06", # Bryson Stott
  "72f94915-e477-4e2e-b2c8-b7febe2484a2" # Brandon Marsh
)
playerRemovals_pos <- c(
  #""
  "915c8fee-00aa-4f5f-9526-3f5d233f5603", # Jared Jones
  "86d4dd35-6065-4c58-b179-142b3eb1f261" # IKF
)
addPlayerPositions <- data.frame(
  PiratesID = c("2dac7862-21dd-4926-8452-075757d9cccb", # Reynolds
                "50ec0fe4-8960-4188-a1a3-36aae400e1e3",# Yorke
                "48b0cda6-abea-40ec-8120-c7ee12ceb3e6"#Cruz
  ),
  PositionTypeID = c(3, 8, 6),
  FRAA = c(-2, -7, -99)
)
playerConstraints <- data.frame(
  PiratesID = "48b0cda6-abea-40ec-8120-c7ee12ceb3e6", # Cruz
  Position = '6',
  Operator = "<=",
  RHS = 0
)
# adjPlayerOffense <- data.frame(
#   PiratesID = c("72f94915-e477-4e2e-b2c8-b7febe2484a2","72f94915-e477-4e2e-b2c8-b7febe2484a2"),
#   split = c('vL', 'vR'),
#   adj = c(7, -3)
# )
addPitchers <- c(
  ""
)

# Optimize Position Players
base_position_players <- optimize_rosters_agg_all(df_repeated, Org = 'PIT'
                                                  #, playerRemovals = playerRemovals_pos
                                                  , addPlayerPositions = addPlayerPositions
                                                  , playerConstraints = NULL
                                                  # , playerConstraints = playerConstraints
                                                  # , adjPlayerOffense = adjPlayerOffense
)

updated_position_players <- optimize_rosters_agg_all(df_repeated, Org = 'PIT'
                                                     , playerAdditions = addPositionPlayers
                                                     , playerRemovals = playerRemovals_pos
                                                     , addPlayerPositions = addPlayerPositions
                                                     , playerConstraints = NULL
                                                     # , playerConstraints = playerConstraints
                                                     # , adjPlayerOffense = adjPlayerOffense
)
runUpdate_position_players_runs <- updated_position_players$obj - base_position_players$obj

# Optimize Pitchers (Heavy on the beta here! Lot of bad assumptions)
base_pitching <- optimize_pitching_agg(df_repeated_pitching, Org = "PIT")
# addPitcher <- "44c8dce1-bb49-4d23-91c8-a4194afc7b19" # Camilo Doval
addPitcher <- "fcbbe69a-e6ea-464d-ade7-76606878be83" # Reid Detmers
updated_pitching <- optimize_pitching_agg(df_repeated_pitching, Org = "PIT", playerAdditions = addPitcher)

marginalGain_pitching_runs <- -(updated_pitching$obj - base_pitching$obj)/9

### Total Imapct#### 
marginalGain_runs <- marginalGain_pitching_runs + runUpdate_position_players_runs

### Impact on Utility ### 
PP_Wins <- read.csv('Org_Win_Probs.csv')
Orgs_Pitching <- read.csv("Playoff_Odds/PitchingWins.csv")
playoff_cond_probs <- read.csv('Playoff_Odds/playoff_probs.csv')
playoffFit <- readRDS("playoffFit_PIT_2025.RDS")

findWinDist <- function(Wins, playoff_cond_probs, playoffFit, sampleVar = 6.25, talentVar = 3){
  totalVar <- sqrt(sampleVar^2 + talentVar^2)
  winVals <-65:110
  winFrame <- data.frame(
    W = winVals,
    prob = pnorm(winVals +.5, mean = Wins, sd = totalVar) - pnorm(winVals -.5, mean = Wins, sd = totalVar)
  ) 
  winFrame$Playoff = predict(playoffFit, newdata = winFrame, type = 'response')
  
  winFrame <- bind_cols(winFrame, apply(winFrame, 1, function(x) x['Playoff']*playoff_cond_probs) %>% bind_rows()) 
  winFrame %>% summarise(
    Wins = Wins,
    Playoff = sum(prob*Playoff)/sum(prob),
    WCWin   = sum(prob*WCWin)/sum(prob),
    DSWin = sum(prob*DSWin)/sum(prob),
    LCSWin = sum(prob*LCSWin)/sum(prob),
    WSWin = sum(prob*WSWin)/sum(prob),
  ) %>% return()
}
win_projections <- PP_Wins %>% inner_join(Orgs_Pitching, by = 'Org') %>% 
  mutate(Win_Total = Wins + WAA.y) %>% 
  select(
    Org, Wins, WAA.y, Win_Total
  )
base_PIT_win <- win_projections %>% filter(Org == 'PIT') %>% pull(Win_Total)
winDist_base <- findWinDist(Wins = win_projections %>% filter(Org == 'PIT') %>% pull(Win_Total), 
                        playoff_cond_probs = playoff_cond_probs,
                        playoffFit = playoffFit) %>% mutate(Wins = round(Wins,1))

winDist_udpated <- findWinDist(Wins = win_projections %>% filter(Org == 'PIT') %>% pull(Win_Total) + marginalGain_runs/10, 
                               playoff_cond_probs = playoff_cond_probs,
                               playoffFit = playoffFit) %>% mutate(Wins = round(Wins,1))


