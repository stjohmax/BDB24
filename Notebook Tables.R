colorsLogos = nflfastR::teams_colors_logos %>%
  select(team_abbr, team_color, team_color2, team_logo_wikipedia)

###########

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
  filter(maxSO >= .05) %>%
  left_join(players %>% select(nflId, displayName, position)) %>%
  left_join(plays %>% select(gameId, playId, passResult, defensiveTeam)) %>%
  mutate(position = case_when(
    position == "DB" ~ "CB",
    position == "MLB" ~ "ILB",
    position == "NT" ~ "IDL",
    position == "DT" ~ "IDL",
    position == "FS" ~ "S",
    position == "SS" ~ "S",
    .default = position
  ),
  playType = case_when(
    is.na(passResult) ~ "Run",
    passResult == "C" ~ "Pass",
    .default = "Scramble"
  ))

snapCount = trackingData %>%
  select(gameId, playId, nflId) %>%
  unique() %>%
  group_by(nflId) %>%
  summarise(snapCount = n())

pursuitOpps = pursuitQualityAddDetails %>%
  group_by(nflId) %>%
  summarise(pursuitOpps = n())

pursuitQualitySummary = pursuitQualityAddDetails %>%
  group_by(nflId, displayName, position, defensiveTeam, playType) %>%
  summarise(plays = n(), totalPQ = sum(pQ), pqPerAttempt = totalPQ/plays) %>%
  pivot_wider(id_cols = c("nflId", "displayName", "position", "defensiveTeam"), names_from = playType, values_from = pqPerAttempt) %>%
  left_join(snapCount) %>%
  left_join(pursuitOpps) %>%
  filter(snapCount >= 100)

positionMidpoints <- aggregate(cbind(Run, Pass) ~ position, pursuitQualitySummary, function(x) mean(x))


ggplot(pursuitQualitySummary, aes(Run, Pass, fill = position, color = position)) +
  geom_point() +
  geom_label(data = positionMidpoints, aes(color = position, label = position), fill = "white") +
  theme_bw() +
  xlab("Run PQ") +
  ylab("Pass PQ") +
  ggtitle("Pass vs Run PQ by Position") +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = .5),
        axis.title.y = element_text(hjust = .5),
        plot.title = element_text(face = "bold", hjust = 0.5))

##############

positionTacklePercentSummary = tackles %>%
  filter(tackle == 1) %>%
  left_join(players %>% select(nflId, position)) %>%
  left_join(plays %>% select(gameId, playId, passResult)) %>%
  mutate(position = case_when(
    position == "DB" ~ "CB",
    position == "MLB" ~ "ILB",
    position == "NT" ~ "IDL",
    position == "DT" ~ "IDL",
    position == "FS" ~ "S",
    position == "SS" ~ "S",
    .default = position
  ),
  playType = case_when(
    is.na(passResult) ~ "Run",
    passResult == "C" ~ "Pass",
    .default = "Scramble"
  )) %>%
  #filter(playType != "Scramble") %>%
  group_by(position, playType) %>%
  summarise(totalTackles = sum(tackle)) %>%
  pivot_wider(id_cols = position, names_from = playType, values_from = totalTackles) %>%
  mutate(totalTackles = sum(Pass, Run, Scramble),
         passTacklePercent = Pass/totalTackles,
         runTacklePercent = Run/totalTackles,
         scrambleTacklePercent = Scramble/totalTackles) %>%
  select(-c(totalTackles, Pass, Run, Scramble))


pursuitComposite = pursuitQualitySummary %>%
  left_join(positionTacklePercentSummary) %>%
  mutate(pursuitComposite = Pass*passTacklePercent + Run*runTacklePercent + Scramble*scrambleTacklePercent) %>%
  left_join(colorsLogos, by = c('defensiveTeam' = 'team_abbr')) %>%
  select(nflId, displayName, position, defensiveTeam, team_logo_wikipedia, team_color, team_color2, snapCount, pursuitOpps, pursuitComposite)


### Tackling ###

tacklePlaySummary = pursuitQuality %>%
  group_by(gameId, playId, nflId) %>%
  summarise(maxSO = max(percentOwnership),
            finalSO = percentOwnership[frameId == max(frameId)]) %>%
  left_join(tackles) %>%
  mutate(tackleResult = case_when(
    pff_missedTackle == 1 ~ "Missed Tackle",
    forcedFumble == 1 ~ "Tackle Contribution",
    tackle == 1 ~ "Tackle Contribution",
    assist == 1 ~ "Tackle Contribution",
    .default = "No Event"
  )) %>%
  filter(tackleResult != "No Event") %>%
  mutate(tackleValue = maxSO - finalSO)
  # mutate(tackleValue = case_when(
  #   tackleResult == "Missed Tackle" ~ finalSO - maxSO,
  #   .default = finalSO
  # ))

tackleCount = tackles %>%
  group_by(nflId) %>%
  summarise(tackles = sum(tackle))

playerTackleSummary = tacklePlaySummary %>%
  group_by(nflId) %>%
  summarise(tackleAttempts = n(),
            tackleValue = sum(tackleValue),
            tackleValuePerAttempt = tackleValue/tackleAttempts) %>%
  left_join(players %>% select(nflId, displayName, position)) %>%
  filter(tackleAttempts >= 15) %>%
  mutate(position = case_when(
    position == "DB" ~ "CB",
    position == "MLB" ~ "ILB",
    position == "NT" ~ "IDL",
    position == "DT" ~ "IDL",
    position == "FS" ~ "S",
    position == "SS" ~ "S",
    .default = position
  )) %>%
  left_join(tackleCount)

pursuitQualityTacklesSummary = pursuitComposite %>%
  left_join(playerTackleSummary) %>%
  na.omit()

x = tacklePlaySummary %>%
  ungroup() %>%
  dplyr::group_by(gameId, playId) %>%
  summarise(tackle = sum(tackle), assist = sum(assist), forcedFumble = sum(forcedFumble), missedTackle = sum(pff_missedTackle), lv = max(tackleValue))
