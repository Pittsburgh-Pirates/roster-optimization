
# fullProj <- pitcherFrame
talent_simulation <- function(fullProj, sampleVar=5, numSamples = 50){
  df_repeated <- replicate(numSamples, fullProj, simplify = FALSE) %>% bind_rows()
  
  df_repeated$RA_adj <- df_repeated$RA_adj + rnorm(nrow(df_repeated), mean = 0, sd = sampleVar) 
  df_repeated$iteration <- sort(rep(1:numSamples, nrow((fullProj))))
  
  return(df_repeated)
}

df_repeated <- talent_simulation(projRaw, sampleVar = .5)

pitchingOpt <- lapply(unique(projRaw$OrgCode), function(org){
  cat(org,'\n')
  pitching <- optimize_pitching_agg(df_repeated, Org = org)
  return(pitching)
})

Orgs_Pitching <- data.frame(
  Org = unique(projRaw$OrgCode)
)

Orgs_Pitching$obj <- map(pitchingOpt, "obj") %>% unlist()


Orgs_Pitching <- Orgs %>% mutate(
  RAA = obj - mean(Orgs$obj),
  WAA = -RAA/10/9
  )


