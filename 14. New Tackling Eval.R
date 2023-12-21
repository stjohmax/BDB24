tacklingEvaluationTibble = pursuitQuality %>%
  left_join(tackles) %>%
  mutate(tackleResult = case_when(
    pff_missedTackle == 1 ~ "Missed Tackle",
    forcedFumble == 1 ~ "Tackle",
    tackle == 1 ~ "Tackle",
    assist == 1 ~ "Assist",
    .default = NA
  )) %>%
  filter(!is.na(tackleResult)) %>%
  left_join(plays %>% select(gameId, playId, defensiveTeam)) %>%
  group_by(gameId, playId, nflId, displayName, defensiveTeam) %>%
  filter(frameId == frameId[cumulativePursuitQuality == max(cumulativePursuitQuality)] |
           frameId == max(frameId)) %>%
  mutate(peakPQ = max(cumulativePursuitQuality),
         finalPQ = cumulativePursuitQuality[frameId == max(frameId)],
         valueLost = peakPQ - finalPQ) %>%
  select(gameId, playId, nflId, displayName, defensiveTeam, distToBallCarrier, peakPQ, finalPQ, valueLost, tackleResult) %>%
  unique()

tacklingPlayerSummary = tacklingEvaluationTibble %>%
  group_by(nflId, displayName, defensiveTeam) %>%
  summarise(tackleOpps = n(), valueLost = abs(sum(valueLost)),
            tackleValueLostPerOpp = valueLost/tackleOpps) %>%
  filter(tackleOpps >= 20) %>%
  left_join(players %>% select(nflId, displayName, position)) %>%
  left_join(colorsLogos, by = c('defensiveTeam' = 'team_abbr')) %>%
  left_join(totalTackles) %>%
  mutate(position = case_when(
    position == "DB" ~ "CB",
    position == "MLB" ~ "ILB",
    position == "NT" ~ "IDL",
    position == "DT" ~ "IDL",
    position == "FS" ~ "S",
    position == "SS" ~ "S",
    .default = position
  ))

x = cumPQvsTacklingResultTibble %>%
  group_by(tackleResult) %>%
  summarise(n = n())

ggplot(tacklingEvaluationTibble, aes(valueLost, tackleResult, fill = tackleResult)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none")

