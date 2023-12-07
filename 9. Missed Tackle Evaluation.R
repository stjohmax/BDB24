compareMissedTackle = pursuitQuality %>%
  group_by(gameId, playId, nflId) %>%
  filter(frameId >= min(frameId[cumulativePursuitQuality == max(cumulativePursuitQuality)])) %>%
  summarise(peakPursuitQuality = cumulativePursuitQuality[frameId == min(frameId)],
            minPursuitQuality = cumulativePursuitQuality[cumulativePursuitQuality == min(cumulativePursuitQuality)],
            peakFrameId = min(frameId),
            minFrameId = frameId[cumulativePursuitQuality == min(cumulativePursuitQuality)],
            lostPursuitQuality = peakPursuitQuality - minPursuitQuality) %>%
  left_join(plays %>% select(gameId, playId, defensiveTeam)) %>%
  left_join(players %>% select(nflId, displayName, position)) %>%
  left_join(tackles) %>%
  #filter(!is.na(pff_missedTackle)) %>%
  rowwise() %>%
  # mutate(n = sum(tackle, assist, forcedFumble, pff_missedTackle, na.rm = T),
  #        result = ifelse(tackle == 1, "Tackle",
  #                        ifelse(pff_missedTackle == 1, "Missed Tackle", "Other"))) %>%
  left_join(calculateInfluence %>% select(gameId, playId, nflId, peakFrameId = frameId, distToBallCarrier)) %>%
  filter(distToBallCarrier <= 1) %>%
  #mutate(timeElapsed = minFrameId - peakFrameId) %>%
  group_by(nflId, displayName, defensiveTeam, position) %>%
  summarise(
    #totalTime = sum(timeElapsed)/10, 
            valueLost = sum(lostPursuitQuality))
            #pqLostPerSecond = valueLost/totalTime)

missedTackles = tackles %>%
  filter(pff_missedTackle == 1) %>%
  group_by(nflId) %>%
  summarise(missedTackles = sum(pff_missedTackle)) %>%
  left_join(snapCount) %>%
  mutate(missedTacklesPerPlay = missedTackles/snapCount) %>%
  filter(snapCount >= 100) %>%
  left_join(compareMissedTackle %>%
              select(nflId, displayName, defensiveTeam, position, valueLost)) %>%
  na.omit() %>%
  mutate(valueLostPerPlay = valueLost/snapCount) %>%
  left_join(colorsLogos, by = c('defensiveTeam' = 'team_abbr')) %>%
  rename(Name=displayName, Team=defensiveTeam)

x = ggplot(missedTackles, aes(valueLostPerPlay, missedTacklesPerPlay)) +
  geom_smooth(method = "lm", se = F, linetype = 'dashed', color = 'black') +
  geom_point(aes(fill = team_color, color = team_color2, label = Name, group = Team), size = 2.5) +
  scale_color_identity() +
  scale_fill_identity() +
  theme_bw() +
  theme(legend.position = "none")

ggplotly(x, tooltip = c('label', 'group'))


