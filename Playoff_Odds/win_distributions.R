library(stringr)

seasonSims <- read.csv("SeasonSimResults.csv") %>% filter(Year == 2025 
                                                          & TeamID ==374
                                                          )



seasonSims <- seasonSims %>% mutate(
  Playoff = PostSeasonResult != 'NULL',
  WCWin = str_detect(PostSeasonResult, 'F'),
  DSWin = str_detect(PostSeasonResult, '1'),
  LCSWin = str_detect(PostSeasonResult, '2'),
  WSWin = str_detect(PostSeasonResult, 'S')
) 


plotDat <- seasonSims %>% group_by(W) %>% summarise(
  Playoff = mean(Playoff),
  WCWin = mean(WCWin),
  DSWin = mean(DSWin),
  LCSWin = mean(LCSWin),
  WSWin = mean(WSWin)
)
### Find conditional playoff probs
playoff_cond_probs <- seasonSims %>% filter(Playoff == 1) %>% 
  summarise(WCWin = mean(WCWin),
            DSWin = mean(DSWin),
            LCSWin = mean(LCSWin),
            WSWin = mean(WSWin))

write.csv(playoff_cond_probs, "playoff_probs.csv")

# ggplot(data = plotDat %>% mutate(projPlayoff = predict(playoffFit, newdata = plotDat, type = 'response'))) + geom_point(aes(x= W, y = Playoff)) + 
#   geom_point(aes(x = W, y = projPlayoff), colour = 'red')

playoffFit <- glm(Playoff ~ W, data = seasonSims, family = binomial(link = 'logit'))
summary(playoffFit)
saveRDS(playoffFit, "playoffFit_PIT_2025.RDS")


# seasonSims %>% group_by(PostSeasonResult) %>% summarise(mean(W)) %>% View()
# 
# 
# seasonSims %>% group_by(TeamID) %>% 
#   summarise(std = sd(W)) %>%View()
# 6.25 std error by sampling
# assume 3 for talent level


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
