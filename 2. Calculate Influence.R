library(matrixStats)
library(geometry)

source("1. Data Load.R")

radius_calc <- function(dist_to_ball) {
  return(4 + 6 * (dist_to_ball >= 15) + (dist_to_ball ** 3) / 560 * (dist_to_ball < 15))
}

# euclidean_distance <- function(x1, y1, x2, y2) {
#   sqrt((x1 - x2)^2 + (y1 - y2)^2)
# }

# compute_influence(2022090800, 56, 53532, 6, 41.78, 15.8, 43.29, 19.97,
#                   1.34, 4.56, 41.78, 15.8)

compute_influence = function(x_point, y_point, 
                             player_x, player_y, player_s, player_theta, football_x, football_y){
  
  # Load necessary libraries
  
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
                     
                     # Calculate COV_matrix
                     COV_matrix <- R_matrix %*% S_matrix %*% S_matrix %*% solve(R_matrix)
                     
                     # Calculate norm_fact
                     norm_fact <- (1 / (2 * pi)) * (1 / sqrt(det(COV_matrix)))
                     
                     # Calculate mu_play
                     mu_play <- player_coords + (speed) * c(cos(theta), sin(theta)) / 2
                     
                     # Calculate intermed_scalar_player
                     intermed_scalar_player <- (player_coords - mu_play) %*% solve(COV_matrix) %*% t(matrix(player_coords - mu_play, nrow = 1))
                     
                     # Calculate player_influence
                     player_influence <- norm_fact * exp(-0.5 * intermed_scalar_player)
                     
                     # Calculate intermed_scalar_point
                     intermed_scalar_point <- (c(x_point, y_point) - mu_play) %*% solve(COV_matrix) %*% t(matrix(c(x_point, y_point) - mu_play, nrow = 1))
                     
                     # Calculate point_influence
                     point_influence <- norm_fact * exp(-0.5 * intermed_scalar_point)
                     
                     # Return the final result
                     return(as.numeric(point_influence / player_influence))
}

# compute_influence(2022090800, 56, 53532, 6, 41.78, 15.8, 43.29, 19.97,
#                   1.34, 4.56, 41.78, 15.8)


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