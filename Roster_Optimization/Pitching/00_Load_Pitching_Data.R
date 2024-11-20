library(dplyr)
library(lpSolve)
library(tidyr)
library(purrr)
library(ggplot2)

# Do we need to do this on a per inning basis rather than BF? Or RA9 rather than ERA?
projRaw <- read.csv("EPS 2025 Pitchers.csv") %>% filter(ERA > 0 & SplitTypeCode == 'EPS' & 
                                                          PrefPos %in% c('RHS','RHR','LHS','LHR','RHP','LHP','P') &
                                                          !(OrgCode %in% c('NULL','NPB','KBO','BOC')))

projRaw <- projRaw %>% mutate(
  SP = as.numeric(PrefPos %in% c('LHS','RHS')),
  RA_adj = ERA # come back to this value and make it better.
) %>% select(PiratesID, PlayerName, OrgCode, SP, RA9_raw = ERA, RA_adj)




