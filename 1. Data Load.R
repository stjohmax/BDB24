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



  