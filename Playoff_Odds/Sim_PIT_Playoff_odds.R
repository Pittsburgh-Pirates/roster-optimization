PP_Wins <- read.csv('Org_Win_Probs.csv')
Orgs_Pitching <- read.csv("PitchingWins.csv")
playoffFit <- readRDS("playoffFit_PIT_2025.RDS")


win_projections <- PP_Wins %>% inner_join(Orgs_Pitching, by = 'Org') %>% 
  mutate(Win_Total = Wins + WAA.y) %>% 
  select(
    Org, Wins, WAA.y, Win_Total
  )

findWinDist(Wins = win_projections %>% filter(Org == 'PIT') %>% pull(Win_Total), 
            playoff_cond_probs = playoff_cond_probs,
            playoffFit = playoffFit)
