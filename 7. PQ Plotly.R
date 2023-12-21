library(plotly)
library(ggimage)

pursuitQualitySummary = pursuitQuality %>%
  left_join(plays %>% select(gameId, playId, defensiveTeam)) %>%
  group_by(nflId, displayName, defensiveTeam, startEvent) %>%
  summarise(n = n(), plays = length(unique(playId)), pursuitQualityPerSecond = sum(pursuitQuality)*10/n,
            movement = sum(dis)) %>%
  # filter(plays >= 100) %>%
  left_join(players %>% select(nflId, position)) %>%
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
  rename(playType = "startEvent") %>%
  filter(plays >= 75)

pursuitQualitySummaryWider = pursuitQualitySummary %>%
  pivot_wider(id_cols = c('nflId', 'displayName', 'defensiveTeam', 'position'), names_from = 'playType', values_from = 'pursuitQualityPerSecond') %>%
  na.omit() %>%
  left_join(colorsLogos, by = c('defensiveTeam' = 'team_abbr'))

custom_order <- c("CB", "DE", "OLB", "S", "IDL", "ILB")

# Reorder the factor levels of the Category variable
pursuitQualitySummaryWider$position <- factor(pursuitQualitySummaryWider$position, levels = custom_order)

colnames(pursuitQualitySummaryWider) = c('nflId', 'Name', 'Team', 'Position', 'Run PQ per Second', 'Pass PQ per Second',
                                         'team_color', 'team_color2', 'team_logo_wikipedia')

pqFacet = ggplot(pursuitQualitySummaryWider, aes(`Run PQ per Second`, `Pass PQ per Second`, 
                                                 label = Name, group = Team)) +
  geom_point(aes(fill = team_color, color = team_color2), size = 2.5) +
  scale_color_identity() +
  scale_fill_identity() +
  # geom_image(aes(image = team_logo_wikipedia), size = 0.04, by = "width", asp = 1.618) +
  geom_vline(xintercept = mean(pursuitQualitySummaryWider$`Run PQ per Second`)) +
  geom_hline(yintercept = mean(pursuitQualitySummaryWider$`Pass PQ per Second`)) +
  # xlim(0, .1) +
  # ylim(0, .1) +
  facet_wrap(~ Position) +
  theme_bw() +
  theme(legend.position = "none")


# +
#   ggtitle('Pass vs Run PQ by Position') +
#   theme(plot.title = element_text(hjust = 0.5)) 

p = ggplotly(pqFacet, tooltip = c('label', 'group'))

htmlwidgets::saveWidget(as_widget(p), "Pass vs Run PQ.html")


