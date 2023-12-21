pursuitQualityByPlay = pursuitQuality %>%
  left_join(tackles) %>%
  mutate(tackleResult = case_when(
    pff_missedTackle == 1 ~ "Missed Tackle",
    forcedFumble == 1 ~ "Tackle Contribution",
    tackle == 1 ~ "Tackle Contribution",
    assist == 1 ~ "Tackle Contribution",
    .default = "No Event"
  )) %>%
  group_by(gameId, playId, nflId) %>%
  summarise(maxFrameId = max(frameId), 
            maxSO = max(percentOwnership),
            pqMaxSO = cumulativePursuitQuality[percentOwnership == max(percentOwnership)][1],
            pqFinal = cumulativePursuitQuality[frameId == maxFrameId],
            tackleResult = tackleResult[frameId == maxFrameId],
            startEvent = startEvent[frameId == maxFrameId],
            pQ = case_when(
              tackleResult == "No Event" ~ pqFinal,
              .default = pqMaxSO
            ))


pursuitQualityAddDetails= pursuitQualityByPlay %>%
  # filter(maxSO >= .05) %>%
  left_join(players %>% select(nflId, displayName, position)) %>%
  mutate(position = case_when(
    position == "DB" ~ "CB",
    position == "MLB" ~ "ILB",
    position == "NT" ~ "IDL",
    position == "DT" ~ "IDL",
    position == "FS" ~ "S",
    position == "SS" ~ "S",
    .default = position
  )) %>%
  filter(startEvent %in% c("ball_snap", "run", "pass_outcome_caught"),
         position %in% c("CB", "DE", "IDL", "ILB", "OLB", "S")) %>%
  mutate(startEvent = case_when(
    startEvent == "ball_snap" ~ "Run",
    startEvent == "pass_outcome_caught" ~ "Pass",
    startEvent == "run" ~ "Scramble"
  )) %>%
  rename(playType = "startEvent")

pursuitQualitySnaps = pursuitQualityAddDetails %>%
  group_by(nflId) %>%
  summarise(snaps = n())

pursuitQualitySummary = pursuitQualityAddDetails %>%
  group_by(nflId, displayName, position, playType) %>%
  summarise(plays = n(), totalPQ = sum(pQ), pqPerAttempt = totalPQ/plays) %>%
  pivot_wider(id_cols = c("nflId", "displayName", "position"), names_from = playType, values_from = pqPerAttempt) %>%
  left_join(pursuitQualitySnaps)


