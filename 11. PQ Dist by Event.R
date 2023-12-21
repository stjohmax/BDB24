finalPursuitQuality = pursuitQuality %>%
  group_by(gameId, playId) %>%
  filter(frameId == max(frameId)) %>%
  left_join(plays %>% select(gameId, playId, quarter, gameClock, defensiveTeam)) %>%
  mutate(timeElapsed = (15 - (hour(gameClock) + minute(gameClock)/60)) + 15*(quarter - 1)) %>%
  left_join(games %>% select(gameId, week)) %>%
  mutate(timeOrder = week*120 + timeElapsed) %>%
  ungroup() %>%
  group_by(nflId) %>%
  arrange(timeOrder) %>%
  mutate(snapNumber = 1:n(),
         seasonLongCumPQ = cumsum(cumulativePursuitQuality)) %>%
  left_join(players %>% select(nflId, displayName, position)) %>%
  filter(max(snapNumber) >= 100) %>%
  left_join(tackles) %>%
  mutate(tackleResult = case_when(
    pff_missedTackle == 1 ~ "Missed Tackle",
    forcedFumble == 1 ~ "Tackle Contribution",
    tackle == 1 ~ "Tackle Contribution",
    assist == 1 ~ "Tackle Contribution",
    .default = "No Event"
  )) %>%
  left_join(colorsLogos,
            by = c("defensiveTeam" = "team_abbr"))

ggplot(finalPursuitQuality, aes(cumulativePursuitQuality, tackleResult, fill = tackleResult)) +
  geom_density_ridges() +
  theme_bw() +
  theme(legend.position = "none")

