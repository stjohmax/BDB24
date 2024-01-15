library(tidyverse)

# Load all data
games = read_csv("Data/games.csv")
tackles = read_csv("Data/tackles.csv")
players = read_csv("Data/players.csv") 
plays = read_csv("Data/plays.csv")

# Iterate over 9 weeks to load tracking data into 1 tibble
trackingData = tibble()
for(i in 1:9){
  data = read_csv(paste0("Data/tracking_week_", i, ".csv"))
  trackingData = bind_rows(trackingData, data)
}

# Distance formula
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Cleaning tracking data

# Reorienting plays so data has them moving left to right
trackingData <- trackingData %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y),
         dir = ifelse(playDirection == "left" & dir < 90, dir + 360,
                      ifelse(playDirection != "left" & dir > 270, dir - 360, dir)),
         dir = ifelse(playDirection == "left", dir - 180, dir),
         dir = dir*(pi/180),
         o = ifelse(playDirection == "left" & o < 90, o + 360,
                    ifelse(playDirection != "left" & o > 270, o - 360, o)),
         o = ifelse(playDirection == "left", o - 180, o),
         o = o*(pi/180))

# Location of just football
footballLocations = trackingData %>%
  filter(is.na(nflId)) %>%
  select(gameId, playId, frameId, footballX = x, footballY = y) %>%
  arrange(frameId) %>%
  group_by(gameId, playId) %>%
  mutate(lagFootballX = lag(footballX),
         lagFootballY = lag(footballY))

# Joining plays and trackingData tibbles
trackingData = plays %>%
  select(gameId:preSnapVisitorScore, expectedPointsAdded) %>%
  full_join(trackingData,
            by = c("gameId" = "gameId",
                   "playId" = "playId")) %>%
  # Add football locations to trackingData and calculate distance from player to ball
  left_join(footballLocations,
            by = c("gameId" = "gameId",
                   "playId" = "playId",
                   "frameId" = "frameId")) %>%
  rowwise() %>%
  mutate(distToFootball = euclidean_distance(x, y, footballX, footballY)) %>%
  # Add variable to identify offense, defense, and ball carrier
  mutate(isOffense = possessionTeam == club,
         isBallCarrier = ballCarrierId == nflId)

# Find ball carrier location for each frame
ballCarrierLocations = trackingData %>%
  filter(isBallCarrier == T) %>%
  select(gameId, playId, frameId, ballCarrierX = x, ballCarrierY = y) %>%
  arrange(frameId) %>%
  group_by(gameId, playId) %>%
  mutate(lagBallCarrierX = lag(ballCarrierX),
         lagBallCarrierY = lag(ballCarrierY))

trackingData = trackingData %>%
  left_join(ballCarrierLocations,
            by = c("gameId" = "gameId",
                   "playId" = "playId",
                   "frameId" = "frameId")) %>%
  group_by(gameId, playId) %>%
  filter(frameId >= 6, frameId <= max(frameId) - 5) %>%
  ungroup() %>%
  mutate(distToBallCarrier = euclidean_distance(x, y, ballCarrierX, ballCarrierY)) %>%
  filter(!is.na(nflId))

remove(footballLocations, ballCarrierLocations)




radius_calc <- function(dist_to_ball) {
  return(4 + 6 * (dist_to_ball >= 15) + (dist_to_ball ** 3) / 560 * (dist_to_ball < 15))
}

compute_influence = function(x_point, y_point, 
                             player_x, player_y, player_s, player_theta, football_x, football_y){
  
  # Convert angles from degrees to radians
  theta <- player_theta
  speed <- player_s
  player_coords <- c(player_x, player_y)
  ball_coords <- c(football_x, football_y)
  
  # Calculate Euclidean distance to the ball
  dist_to_ball <- euclidean_distance(player_coords[1], player_coords[2], 
                                     ball_coords[1], ball_coords[2])
  
  # Calculate S_ratio
  S_ratio <- (speed / 13)^2
  
  # Calculate RADIUS using the radius_calc function
  RADIUS <- radius_calc(dist_to_ball)
  
  # Calculate S_matrix
  S_matrix <- matrix(c(RADIUS * (1 + S_ratio), 0, 0, RADIUS * (1 - S_ratio)), nrow = 2)
  
  # Calculate R_matrix
  R_matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), byrow = TRUE, nrow = 2)
  
  COV_matrix <- R_matrix %*% S_matrix %*% S_matrix %*% solve(R_matrix)
  
  norm_fact <- (1 / (2 * pi)) * (1 / sqrt(det(COV_matrix)))
  
  mu_play <- player_coords + (speed) * c(cos(theta), sin(theta)) / 2
  
  intermed_scalar_player <- (player_coords - mu_play) %*% solve(COV_matrix) %*% t(matrix(player_coords - mu_play, nrow = 1))
  
  player_influence <- norm_fact * exp(-0.5 * intermed_scalar_player)
  
  intermed_scalar_point <- (c(x_point, y_point) - mu_play) %*% solve(COV_matrix) %*% t(matrix(c(x_point, y_point) - mu_play, nrow = 1))
  
  point_influence <- norm_fact * exp(-0.5 * intermed_scalar_point)
  
  return(as.numeric(point_influence / player_influence))
}


# Test calc influence for a play
calculateInfluence = trackingData %>%
  filter(isBallCarrier == F) %>%
  rowwise() %>%
  mutate(influence = compute_influence(ballCarrierX, ballCarrierY, 
                                       x, y, s, dir, ballCarrierX, ballCarrierY)) %>%
  group_by(gameId, playId, frameId) %>%
  mutate(overallOwnership = sum(influence)) %>%
  ungroup() %>%
  group_by(gameId, playId, isOffense, frameId) %>%
  mutate(teamOverallOwnership = sum(influence)) %>%
  ungroup() %>%
  mutate(percentOwnership = influence/overallOwnership,
         percentTeamOwnership = influence/teamOverallOwnership) %>%
  arrange(frameId) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(lagOwnership = lag(percentOwnership),
         lagTeamOwnership = lag(percentTeamOwnership)) %>%
  group_by(gameId, playId, nflId, frameId) %>%
  mutate(xMovement = footballX - lagFootballX,
         deltaSO = percentOwnership - lagOwnership,
         deltaTeamSO = percentTeamOwnership - lagTeamOwnership) %>%
  ungroup()

playerSO = calculateInfluence %>%
  filter(isOffense == F) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(startEvent = event[frameId == 6]) %>%
  ungroup() %>%
  group_by(nflId, displayName, startEvent) %>%
  summarise(n = n(), plays = length(unique(playId)), movement = sum(dis),
            spaceGain = sum(deltaSO, na.rm = T), spaceGainPerSecond = spaceGain*10/n) %>%
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
  ))


# 
# # Define the custom order of facets
# custom_order <- c("CB", "DE", "OLB", "S", "IDL", "ILB")
# 
# # Reorder the factor levels of the Category variable
# playerSO$position <- factor(playerSO$position, levels = custom_order)
# 
# ggplot(playerSO %>% filter(startEvent %in% c("ball_snap", "pass_outcome_caught"), plays >= 100) %>%
#          mutate(startEvent = case_when(startEvent == "ball_snap" ~ "run", startEvent == "pass_outcome_caught" ~ "pass")), 
#        aes(movement, spaceGainPerSecond, color = startEvent)) +
#   geom_point() +
#   facet_wrap(~ position) +
#   theme_bw()
# 
# # x = playerSO %>%
# #   filter(position == "IDL", startEvent == "ball_snap", plays >= 80)
# # 2022091900
# spaceGainedValueCalc = function(game_Id){
#   data = trackingData %>%
#     filter(gameId == game_Id) %>%
#     arrange(frameId) %>%
#     group_by(gameId, playId, frameId) %>%
#     mutate(leadPlayerX = lead(x),
#            leadPlayerY = lead(y)) %>%
#     arrange(nflId) %>%
#     group_by(gameId, playId, frameId, isOffense) %>%
#     ungroup()
# 
#   defenseData = data %>%
#     filter(isOffense == F) %>%
#     select(gameId, playId, frameId, defId = nflId, leadDefX = leadPlayerX, leadDefY = leadPlayerY)
# 
#   spaceGained = data %>%
#     select(gameId, playId, frameId, nflId, isOffense, x, y, s, dir, footballX, footballY) %>%
#     right_join(defenseData,
#                by = c("gameId" = "gameId",
#                       "playId" = "playId",
#                       "frameId" = "frameId")) %>%
#     rowwise() %>%
#     mutate(influenceNextLocation = compute_influence(leadDefX, leadDefY, x, y, s, dir, footballX, footballY)) %>%
#     group_by(gameId, playId, frameId, defId) %>%
#     mutate(overallOwnershipNextLoc = sum(influenceNextLocation)) %>%
#     ungroup() %>%
#     group_by(gameId, playId, frameId, defId, isOffense) %>%
#     mutate(teamOwnershipNextLoc = sum(influenceNextLocation)) %>%
#     ungroup() %>%
#     filter(nflId == defId) %>%
#     select(gameId, playId, frameId, nflId, influenceNextLocation:teamOwnershipNextLoc) %>%
#     mutate(opponentOwnershipNextLoc = overallOwnershipNextLoc - teamOwnershipNextLoc,
#            opponentOwnershipPercentNextLoc = opponentOwnershipNextLoc/overallOwnershipNextLoc,
#            spaceGainedValue = influenceNextLocation*opponentOwnershipPercentNextLoc)
# 
#   return(spaceGained)
# 
# }
# # 
# # testFunc = spaceGainedValueCalc(2022091900)
# # 
# # 
# # 
# gameIdList = trackingData %>% select(gameId) %>% unique() %>% as.list() %>% unlist()
# 
# for(i in 70:length(gameIdList)){
#   data = spaceGainedValueCalc(gameIdList[i])
#   write_csv(data, paste0("Data/Next Location Value/", i, "_", gameIdList[i], ".csv"))
#   print(i)
# }
# 
# 
#   # group_by(nflId) %>%
#   # summarise(n = n(), spaceGained = sum(spaceGainedValue),
#   #           spaceGainedPerSecond = spaceGained*10/n) %>%
#   # left_join(players %>% select(nflId, displayName, position))
#   
#   # NEED TO INCLUDE BALL CARRIER (?)
# 
# # 
# # x = calculateInfluence %>%
# #   filter(frameId == 6) %>%
# #   select(gameId, playId, event) %>%
# #   unique()
# # 
# # y = calculateInfluence %>%
# #   filter(gameId == 2022100207, playId == 2933)
# # 
# # # a = y %>%
# # #   group_by(playId, frameId, isOffense) %>%
# # #   summarise(so = sum(percentTeamOwnership))
# # 
# # b = y %>%
# #   select(nflId, displayName, frameId, isOffense, influence:deltaTeamSO) %>%
# #   mutate(increaseSO = deltaSO > 0,
# #          increaseTeamSO = deltaTeamSO > 0) %>%
# #   filter(isOffense == F) %>%
# #   group_by(increaseSO, increaseTeamSO) %>%
# #   summarise(n = n(), percent = n/26873)
# # 
# # # a = z %>%
# # #   filter(isOffense == F) %>%
# # #   group_by(nflId, displayName) %>%
# # #   summarise(maxSO = max(percentOwnership), finalSO = percentOwnership[frameId == max(frameId)],
# # #             diff = maxSO - finalSO)
# # 
# # # a = z %>%
# # #   filter(isOffense == F) %>%
# # #   mutate(sg = deltaSO*(1 - max(xMovement, 0))) %>%
# # #   group_by(nflId, displayName) %>%
# # #   summarise(n = n(), framesInPosition = sum(distToBallCarrier <= 5), totalSG = sum(sg), sgPerFrame = sum(sg)/n)
# # 
# # ggplot(z %>% filter(isOffense == F),
# #                     # displayName %in% c("Bobby Wagner", "Jalen Ramsey", "Ernest Jones")), 
# #        aes(frameId, percentOwnership, color = displayName)) + geom_line() 
# # 
# # 
# # # Aggregate 

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
  select(gameId:event, dis, influence, percentOwnership, opponentOwnershipPercentNextLoc) %>%
  arrange(frameId) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(lagInfluence = lag(percentOwnership)) %>%
  mutate(influenceGained = percentOwnership - lagInfluence,
         pursuitQuality = ifelse(influenceGained < 0, influenceGained*(1-opponentOwnershipPercentNextLoc), opponentOwnershipPercentNextLoc*influenceGained),
         #opponentOwnershipPercentNextLoc*influenceGained,
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
            peakFrameId = frameId[percentOwnership == maxSO],
            maxInfluence = max(influence),
            pqMaxSO = cumulativePursuitQuality[influence == maxInfluence][1],
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
  mutate(tackleValueLost = maxSO - finalSO)
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
            tackleValueLost = sum(tackleValueLost),
            tackleValueLostPerAttempt = tackleValueLost/tackleAttempts) %>%
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

custom_order <- c("CB", "DE", "OLB", "S", "IDL", "ILB")

# Reorder the factor levels of the Category variable
pursuitQualityTacklesSummary$position <- factor(pursuitQualityTacklesSummary$position, levels = custom_order)

x = tacklePlaySummary %>%
  ungroup() %>%
  dplyr::group_by(gameId, playId) %>%
  summarise(tackle = sum(tackle), assist = sum(assist), forcedFumble = sum(forcedFumble), missedTackle = sum(pff_missedTackle), lv = max(tackleValueLost))


jerseyNumbers = trackingData %>%
  select(nflId, displayName, jerseyNumber) %>%
  unique()

playerDistance = trackingData %>%
  select(gameId, playId, nflId, frameId, dis) %>%
  right_join(tackles) %>%
  group_by(gameId, playId, nflId) %>%
  summarise(distance = sum(dis), tackle = max(tackle), assist = max(assist), forcedFumble = max(forcedFumble),
            pff_missedTackle = max(pff_missedTackle)) %>%
  left_join(games %>% select(gameId, homeTeamAbbr, visitorTeamAbbr)) %>%
  left_join(pursuitQualityByPlay %>% select(gameId, playId, nflId, maxSO, maxFrameId, peakFrameId)) %>%
  mutate(tacklePhaseLength = maxFrameId - peakFrameId) %>%
  right_join(tacklePlaySummary %>% select(gameId, playId, nflId, tackleValueLost)) %>%
  group_by(gameId, playId) %>%
  mutate(totalMissedTackles = sum(pff_missedTackle))


pursuitQualityBoxplotData = pursuitQualityByPlay %>%
  mutate(pursuitOutcome = case_when(
    tackleResult == "No Event" ~ "No Event",
    .default = "Tackle Attempt"
  ))


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

positionTacklePercentSummaryLonger = positionTacklePercentSummary %>%
  rename(Run="runTacklePercent", Pass="passTacklePercent", Scramble="scrambleTacklePercent") %>%
  pivot_longer(cols = c("Pass", "Run", "Scramble"), names_to = 'playType') %>%
  mutate(percentLabel = case_when(
    value < .07 ~ NA,
    .default = paste0(round(value, 2)*100, "%")
  ))


plot_order <- c("IDL", "DE", "OLB", "ILB", "S", "CB")

# Reorder the factor levels of the Category variable
positionTacklePercentSummaryLonger$position <- factor(positionTacklePercentSummaryLonger$position, levels = plot_order)
positionTacklePercentSummaryLonger$playType = factor(positionTacklePercentSummaryLonger$playType, 
                                                     levels = c("Scramble", "Run", "Pass"))

positionTacklePercentSummaryLonger$labelPos = c(.365, .85, NA, .055, .5355, NA, .0415, .526, NA, .2305, .7, NA, .17, .64, NA, .28, .765, NA)

pursuitQualityFrameByFrameAvg = pursuitQuality %>%
  left_join(tackles) %>%
  mutate(tackleResult = case_when(
    pff_missedTackle == 1 ~ "Missed Tackle",
    forcedFumble == 1 ~ "Tackle Contribution",
    tackle == 1 ~ "Tackle Contribution",
    assist == 1 ~ "Tackle Contribution",
    .default = "No Event"
  )) %>%
  select(gameId, playId, nflId, frameId, tackleResult, percentOwnership, cumulativePursuitQuality) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(maxFrameId = max(frameId),
         maxSO = max(percentOwnership),
         peakFrameId = min(frameId[percentOwnership == maxSO]),
         finalFrame = case_when(
           tackleResult == "No Event" ~ maxFrameId,
           .default = peakFrameId
         )) %>%
  filter(maxSO >= .05) %>%
  ungroup() %>%
  group_by(frameId) %>%
  summarise(nflAvgPQ = mean(cumulativePursuitQuality))

spaceOwnershipAvgByFrame = pursuitQuality %>%
  ungroup() %>%
  select(frameId, percentOwnership) %>%
  group_by(frameId) %>%
  summarise(nflAvgSO = mean(percentOwnership)) %>%
  mutate(frameId = frameId - 5)

tvlAvgByFrame = pursuitQuality %>%
  left_join(tackles) %>%
  mutate(tackleResult = case_when(
    pff_missedTackle == 1 ~ "Missed Tackle",
    forcedFumble == 1 ~ "Tackle Contribution",
    tackle == 1 ~ "Tackle Contribution",
    assist == 1 ~ "Tackle Contribution",
    .default = "No Event"
  )) %>%
  filter(tackleResult != "No Event") %>%
  group_by(gameId, playId, nflId) %>%
  mutate(maxSO = max(percentOwnership),
         peakFrameId = min(frameId[percentOwnership == maxSO])) %>%
  filter(frameId >= peakFrameId) %>%
  mutate(tvl = maxSO - percentOwnership) %>%
  ungroup() %>%
  group_by(frameId) %>%
  summarise(nflAvgTVL = mean(tvl))

