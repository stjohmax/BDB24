source("1. Data Load.R")
source("2. Calculate Influence.R")

loadCSVFilesFromFolder <- function(folder_path) {
  # Get a list of CSV files in the specified folder
  csv_files <- list.files(path = folder_path, pattern = ".csv", full.names = TRUE)
  
  # Initialize an empty list to store data frames
  data_frames <- list()
  
  # Loop through the list of CSV files and read them into data frames
  for (file in csv_files) {
    data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)  # Modify read function as needed
    data_frames[[file]] <- data
  }
  
  # Combine all data frames into a single list or data frame
  # You can choose to return a list or combine them into a single data frame
  # In this example, we return a list of data frames
  return(data_frames)
}

# Usage: Provide the folder path as an argument
folder_path <- "/Users/maxstjohn/Desktop/NFL_BDB_24/Data/Next Location Value"
csv_data <- loadCSVFilesFromFolder(folder_path)

# Access individual data frames from the list
# For example, to access the first data frame, use csv_data[[1]]

next_location_values = bind_rows(csv_data)

ownership_location_values = calculateInfluence %>%
  filter(isOffense == F) %>%
  left_join(next_location_values,
            by = c("gameId" = "gameId",
                   "playId" = "playId",
                   "frameId" = "frameId",
                   "nflId" = "nflId"))

ownership_location_values = ownership_location_values %>%
  select(gameId, playId, frameId, nflId, displayName, x:spaceGainedValue)

pursuitQuality = ownership_location_values %>%
  select(gameId:event, dis, influence, opponentOwnershipPercentNextLoc) %>%
  arrange(frameId) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(lagInfluence = lag(influence)) %>%
  mutate(influenceGained = influence - lagInfluence,
         pursuitQuality = opponentOwnershipPercentNextLoc*influenceGained,
         cumulativePursuitQuality = cumsum(replace_na(pursuitQuality, 0)),
         startEvent = event[frameId == 6]) %>%
  filter(!is.na(pursuitQuality))

pursuitQualitySummary = pursuitQuality %>%
  group_by(nflId, displayName, startEvent) %>%
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

custom_order <- c("CB", "DE", "OLB", "S", "IDL", "ILB")

# Reorder the factor levels of the Category variable
pursuitQualitySummary$position <- factor(pursuitQualitySummary$position, levels = custom_order)

ggplot(pursuitQualitySummary, aes(movement, pursuitQualityPerSecond, color = playType)) +
  geom_point() +
  facet_wrap(~ position) +
  theme_bw()

trackPursuitPlot = function(game, play){
  selectPlayTackles = tackles %>%
    filter(gameId == game, playId == play) %>%
    left_join(players %>% select(nflId, displayName)) %>%
    pivot_longer(cols = tackle:pff_missedTackle) %>%
    filter(value != 0) %>%
    select(-value) %>%
    rename(playerResult = "name")
  
  selectPlay = pursuitQuality %>%
    filter(gameId == game, playId == play) %>%
    left_join(selectPlayTackles)
  
  ggplot(selectPlay, aes(frameId, cumulativePursuitQuality, color = displayName, shape = playerResult)) +
    geom_line() +
    geom_point() +
    theme_bw()
}

trackPursuitPlot(2022091500, 3350)

selectPlay = pursuitQuality %>%
  filter(gameId == 2022091111, playId == 1790)

#2022091111, 3051

selectPlayTackles = tackles %>%
  filter(gameId == 2022100913, playId == 459) %>%
  left_join(players %>% select(nflId, displayName)) %>%
  pivot_longer(cols = tackle:pff_missedTackle) %>%
  filter(value != 0) %>%
  rename(playerResult = "name")

ggplot(selectPlay, aes(frameId, cumulativePursuitQuality, color = displayName)) +
  geom_line() +
  geom_point() +
  theme_bw()

pursuitQuality = pursuitQuality %>%
  left_join(tackles)

compareMissedTackle = pursuitQuality %>%
  group_by(gameId, playId, nflId) %>%
  filter(frameId >= min(frameId[cumulativePursuitQuality == max(cumulativePursuitQuality)])) %>%
  summarise(peakPursuitQuality = cumulativePursuitQuality[frameId == min(frameId)],
            minPursuitQuality = cumulativePursuitQuality[cumulativePursuitQuality == min(cumulativePursuitQuality)],
            peakFrameId = min(frameId),
            minFrameId = frameId[cumulativePursuitQuality == min(cumulativePursuitQuality)],
            lostPursuitQuality = peakPursuitQuality - minPursuitQuality) %>%
  left_join(tackles) %>%
  filter(!is.na(pff_missedTackle)) %>%
  rowwise() %>%
  mutate(n = sum(tackle, assist, forcedFumble, pff_missedTackle, na.rm = T),
         result = ifelse(tackle == 1, "Tackle",
                  ifelse(pff_missedTackle == 1, "Missed Tackle", "Other")))

ggplot(compareMissedTackle %>% filter(result != "Other"), aes(lostPursuitQuality, result)) +
  geom_boxplot() +
  theme_bw()

colorsLogos = nflfastR::teams_colors_logos %>%
  select(team_abbr, team_color, team_color2, team_logo_wikipedia)

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
    forcedFumble == 1 ~ "Forced Fumble",
    tackle == 1 ~ "Tackle",
    assist == 1 ~ "Assist",
    .default = NA
  )) %>%
  left_join(colorsLogos,
            by = c("defensiveTeam" = "team_abbr"))
  

x = data %>%
  filter(position == "ILB") %>%
  group_by(nflId) %>%
  summarise(n = n())



cumulativePlotPQ = function(position, id){
  data = finalPursuitQuality %>%
    filter(position == position,
           nflId == id)
  
  color1 = data$team_color[1]
  color2 = data$team_color2[1]
  # %>%
  #   mutate(lineColor = case_when(
  #     nflId == id ~ team_color,
  #     .default = "black"),
  #     alphaCol = case_when(
  #       nflId == id ~ 1,
  #       .default = .95),
  #     pointShape = case_when(
  #       nflId == id ~ tackleResult,
  #       .default = NA
  #     ))
  ggplot(data, aes(snapNumber, seasonLongCumPQ, fill = color2)) +
    geom_point() +
    # geom_line(color = color1) +
    # geom_point() +
    theme_bw()
  
  
  ggplot(data %>% filter(is.na(pointShape)), aes(snapNumber, seasonLongCumPQ, group = nflId)) +
    geom_line(aes(alpha = alphaCol)) +
    # geom_point(size = 2, aes(color = lineColor, fill = "#B3995D", shape = pointShape)) +
    scale_color_identity() +
    # scale_fill_identity() + 
    theme_bw()
  # +
  #   theme(legend.position = "none")
}

"#9E7c0C"

cumulativePlotPQ("ILB", 46139)


# pursuitQualityScatterPlot = function(position, type, id){
#   data = pursuitQualitySummary %>%
#     filter(position == position, type == playType) %>%
#     mutate(pointColor = case_when(
#       nflId == id ~ "#FF0000",
#       .default = "black"),
#       pointSize = case_when(
#         nflId == id ~ 3,
#         .default = 1))
#   
#   ggplot(data, aes(plays, pursuitQualityPerSecond, color = pointColor, size = pointSize)) +
#     geom_point() +
#     scale_color_identity(aesthetics = c("fill", "color")) +
#     theme_bw() +
#     theme(legend.position = "none")
#   
# }
# 
# pursuitQualityScatterPlot("ILB", "Pass", 46139)
