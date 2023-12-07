library(ggridges)

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

totalTackles = tackles %>%
  group_by(nflId) %>%
  summarise(totalTackles = sum(tackle)) %>%
  filter(totalTackles > 0)

snapCount = trackingData %>%
  select(gameId, playId, nflId) %>%
  unique() %>%
  left_join(plays %>% select(gameId, playId, passResult)) %>%
  mutate(playType = case_when(
    is.na(passResult) ~ "Run",
    passResult == "C" ~ "Pass",
    .default = "Scramble"
  )) %>%
  #filter(playType != "Scramble") %>%
  group_by(nflId) %>%
  summarise(snapCount = n())

totalTacklesSummary = totalTackles %>%
  left_join(snapCount) %>%
  mutate(tacklesPerPlay = totalTackles/snapCount)
  

playerTacklingComposite = pursuitQualitySummaryWider %>%
  left_join(positionTacklePercentSummary,
            by = c("Position" = "position")) %>%
  mutate(tacklingComposite = `Run PQ per Second`*runTacklePercent + `Pass PQ per Second`*passTacklePercent) %>%
  left_join(totalTacklesSummary)

tacklingCompositePlot = ggplot(playerTacklingComposite, aes(tacklesPerPlay, tacklingComposite)) +
  geom_vline(xintercept = mean(playerTacklingComposite$tacklesPerPlay), linetype = 'dashed') +
  geom_hline(yintercept = mean(playerTacklingComposite$tacklingComposite), linetype = 'dashed') +
  #geom_smooth(method = "lm", se = F, linetype = 'dashed', color = 'black') +
  geom_point(aes(label = Name, group = Team, fill = team_color, color = team_color2), size = 2.5) +
  facet_wrap(~ Position) +
  scale_color_identity() +
  scale_fill_identity() +
  theme_bw() +
  theme(legend.position = "none")

ggplotly(tacklingCompositePlot, tooltip = c('label', 'group'))

positionTacklePercentSummaryLonger = positionTacklePercentSummary %>%
  rename(Run="runTacklePercent", Pass="passTacklePercent", Scramble="scrambleTacklePercent") %>%
  pivot_longer(cols = c("Pass", "Run", "Scramble"), names_to = 'playType')

plot_order <- c("IDL", "DE", "OLB", "ILB", "S", "CB")

# Reorder the factor levels of the Category variable
positionTacklePercentSummaryLonger$position <- factor(positionTacklePercentSummaryLonger$position, levels = plot_order)
positionTacklePercentSummaryLonger$playType = factor(positionTacklePercentSummaryLonger$playType, 
                                                     levels = c("Scramble", "Run", "Pass"))

ggplot(positionTacklePercentSummaryLonger, aes(value, position, fill = playType)) +
  geom_bar(stat = "identity") +
  theme_bw()




playerTacklingComposite$Position = factor(playerTacklingComposite$Position,
                                          levels = c('CB', 'IDL', 'OLB', 'DE', 'S', 'ILB'))

ggplot(playerTacklingComposite, aes(tacklingComposite, Position, height = after_stat(density), fill = Position)) +
  geom_density_ridges() +
  theme_bw() +
  ggtitle('Tackling Composite Distribution by Position') +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = .5),
        axis.title.y = element_text(hjust = .5),
        plot.title = element_text(face = "bold", hjust = 0.5))

playerTacklingComposite %>%
  group_by(Position) %>%
  summarise(meanTC = mean(tacklingComposite)) %>%
  arrange(-meanTC)

  
