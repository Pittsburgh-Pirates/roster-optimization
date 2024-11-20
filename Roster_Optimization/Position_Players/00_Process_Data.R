library(dplyr)
library(lpSolve)
library(tidyr)
library(purrr)
library(ggplot2)



projRaw <- read.csv("EPS.csv")
offenseOnly <- projRaw %>% group_by(PiratesID, PlayerName, OrgCode, SplitType) %>% summarise(RAA_635 = mean(RAA_635))

posFrame <- expand_grid(PiratesID = unique(projRaw$PiratesID), SplitType = unique(projRaw$SplitType), PositionTypeID = 2:10)

defProjExp <- posFrame  %>% left_join(projRaw, by = c('PiratesID', 'PositionTypeID', 'SplitType')) %>% #filter(PiratesID =='86D4DD35-6065-4C58-B179-142B3EB1F261') %>% arrange(PositionTypeID) %>% 
  select(PiratesID, PlayerName, SplitType, PositionTypeID, FRAA_150) %>% 
  mutate(FRAA = ifelse(is.na(FRAA_150), -1000, FRAA_150)) %>% select(PiratesID, SplitType, PositionTypeID, FRAA)


fullProj <- defProjExp %>% inner_join(offenseOnly, by = c('PiratesID', 'SplitType')) %>% 
  mutate(Value = RAA_635 + FRAA)


positions <- c('C','1B','2B','3B','SS','LF','CF','RF','DH')

