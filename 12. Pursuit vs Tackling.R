pursuitAttemptIds = pursuitQuality %>%
  group_by(gameId, playId, nflId) %>%
  summarise(pursuitAttempt = case_when(
    max(percentOwnership) >= .05 ~ TRUE,
    .default = FALSE
  ))

pursuitAttemptsFilter = pursuitQuality %>%
  left_join(pursuitAttemptIds) %>%
  filter(pursuitAttempt == T) %>%
  group_by(gameId, playId, nflId) %>%
  summarise(peakPQ = max(cumulativePursuitQuality))

pursuitPlayerSummary = pursuitEvaluationTibble %>%
  dplyr::group_by(nflId, displayName, defensiveTeam) %>%
  summarise(pursuits = n(), pqGained = sum(peakPursuitQuality), timeElapsed = sum(timeToPeak),
            pqGainedPerPursuit= pqGained/pursuits) %>%
  filter(pursuits >= 100)


tacklingAttemptStartFrame = pursuitAttemptsFilter %>%
  left_join(calculateInfluence %>% select(gameId, playId, nflId, frameId, distToBallCarrier)) %>%
  group_by(gameId, playId, nflId) %>%
  summarise(tackleStartFrame = min(frameId[(distToBallCarrier <= 1.5 & percentOwnership >= .05 & cumulativePursuitQuality == max(cumulativePursuitQuality))])) %>%
  filter(tackleStartFrame != Inf)

tacklingEvaluationTibble = pursuitQuality %>%
  left_join(tacklingAttemptStartFrame) %>%
  filter(!is.na(tackleStartFrame)) %>%
  group_by(gameId, playId, nflId) %>%
  filter(frameId >= tackleStartFrame) %>%
  left_join(plays %>% select(gameId, playId, defensiveTeam))
  
tacklingPlayerSummary = tacklingEvaluationTibble %>%
  left_join(tackles) %>%
  mutate(tackleResult = case_when(
    pff_missedTackle == 1 ~ "Missed Tackle",
    forcedFumble == 1 ~ "Tackle",
    tackle == 1 ~ "Tackle",
    assist == 1 ~ "Assist",
    .default = "No Event"
  )) %>%
  filter(tackleResult != "No Event") %>%
  group_by(nflId, displayName, defensiveTeam) %>%
  summarise(tackleOpps = n(), valueLost = abs(sum(pursuitQuality)),
            tackleValueLostPerOpp = valueLost/tackleOpps)
%>%
  filter(tackleOpps >= 50) %>%
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

playerPursuitTacklingSummary = pursuitPlayerSummary %>%
  inner_join(tacklingPlayerSummary)


playerPursuitTacklingSummary


pursuitVsTacklingGG = ggplot(playerPursuitTacklingSummary, aes(pqGainedPerPursuit, tackleValueLostPerOpp, label = displayName,
                                         group = defensiveTeam)) +
  geom_vline(xintercept = mean(playerPursuitTacklingSummary$pqGainedPerPursuit)) +
  geom_hline(yintercept = mean(playerPursuitTacklingSummary$tackleValueLostPerOpp)) +
  geom_point(aes(fill = team_color, color = team_color2), size = 2.5) +
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(~ position) +
  theme_bw() +
  theme(legend.position = "none")

ggplotly(pursuitVsTacklingGG, tooltip = c('label', 'group'))


cumPQvsTacklingResultTibble = tacklingEvaluationTibble %>%
  group_by(gameId, playId, nflId) %>%
  summarise(peakPQ = max(cumulativePursuitQuality),
            finalPQ = cumulativePursuitQuality[frameId == max(frameId)],
            valueLost = peakPQ - finalPQ) %>%
  left_join(tackles) %>%
  mutate(tackleResult = case_when(
    pff_missedTackle == 1 ~ "Missed Tackle",
    forcedFumble == 1 ~ "Tackle",
    tackle == 1 ~ "Tackle",
    assist == 1 ~ "Assist",
    .default = "No Event"
  ))

x = cumPQvsTacklingResultTibble %>%
  group_by(tackleResult) %>%
  summarise(n = n())

ggplot(cumPQvsTacklingResultTibble, aes(valueLost, tackleResult, fill = tackleResult)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none")





ggplot(tackleEvaluationTibble, aes(valueLost, tackleResult, fill = tackleResult)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none")

peakPQ = pursuitQuality %>%
  group_by(gameId, playId, nflId) %>%
  summarise(peakPQ = max(percentOwnership))

x = peakPQ %>%
  left_join(tackles) %>%
  mutate(tackleAttempt = case_when(
    pff_missedTackle == 1 ~ TRUE,
    forcedFumble == 1 ~ TRUE,
    tackle == 1 ~ TRUE,
    assist == 1 ~ TRUE,
    .default = FALSE
  ))

cor(x$peakPQ, x$tackleAttempt)

ggplot(x, aes(peakPQ, tackleAttempt, fill = tackleAttempt)) +
  geom_boxplot() +
  theme_bw()


