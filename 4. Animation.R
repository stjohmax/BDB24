# https://michael-morris.us/2020/10/nfl-big-data-bowl-2021-exploration.en-us/
library(gganimate)
library(transformr)
library(magick)


# gameId == 2022101601, playId == 528, frameId == 15

colorsLogos = nflfastR::teams_colors_logos %>%
  select(team_abbr, team_color, team_color2, team_logo_wikipedia)

plot_frame = function(game, play, frame){
  pull_influence = calculateInfluence %>%
    filter(gameId == game, playId == play, frameId == frame) %>%
    select(gameId, playId, frameId, nflId, percentOwnership)
  
  pull_difficulty = pursuitQuality %>%
    filter(gameId == game, playId == play, frameId == frame) %>%
    select(gameId, playId, frameId, nflId, opponentOwnershipPercentNextLoc)
  
  # medianPercentOwnership = median(pull_influence$percentOwnership)
  
  single_play = trackingData %>%
    filter(gameId == game, playId == play, frameId == frame) %>%
    left_join(colorsLogos,
              by = c("club" = "team_abbr")) %>%
    mutate(team_color = ifelse(ballCarrierId == nflId, "orange", team_color),
           team_color2 = ifelse(ballCarrierId == nflId, "black", team_color2)) %>%
    left_join(pull_influence) %>%
    left_join(pull_difficulty) %>%
    group_by(frameId, nflId) %>%
    mutate(s_x = s*cos(dir - pi/2), s_y = s*sin(dir - pi/2),
           cone_x1 = 2.5*cos(dir - pi/2 + pi/4), cone_y1 = 2.5*sin(dir - pi/2 + pi/4),
           cone_x2 = 2.5*cos(dir - pi/2 - pi/4), cone_y2 = 2.5*sin(dir - pi/2 - pi/4))
  
  triangle_data = single_play %>%
    select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y) %>% mutate(newX = x, newY = y) %>%
    bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y, cone_x1, cone_y1) %>% 
                mutate(newX = x + cone_x1, newY = y + cone_y1) %>% select(-c(cone_x1, cone_y1))) %>%
    bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y, cone_x2, cone_y2) %>% 
                mutate(newX = x + cone_x2, newY = y + cone_y2) %>% select(-c(cone_x2, cone_y2))) %>%
    bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y) %>% mutate(newX = x, newY = y)) %>%
    mutate(deltaX = newX - x, deltaY = newY - y)
           # "#654321"
           # s_x = s*cos(dir),
           # s_y = s*sin(dir))

  # %>%
  #   mutate(text_color = case_when(
  #     percentOwnership > medianPercentOwnership ~ "white",
  #     ballCarrierId == nflId ~ "white",
  #     .default = "black"
  #   ))
  
  
  ## General field boundaries
  xmin <- 0
  xmax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(single_play$playDescription[1])))
  
  ## Specific boundaries for a given play
  ymin <- max(round(min(single_play$x, na.rm = TRUE) - 10, -1), 0)
  ymax <- min(round(max(single_play$x, na.rm = TRUE) + 10, -1), 120)
  df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
  df_hash <- df_hash %>% filter(y < ymax, y > ymin)
  
  ggplot() +
    scale_size_manual(values = c(6, 6), guide = FALSE) +
    scale_shape_manual(values = c(21, 21), guide = FALSE) +
    scale_fill_identity() +
    # scale_colour_identity() +
    scale_alpha_identity() +
    # scale_alpha_continuous(range = c(.4, .8), name = "opponentOwnershipPercentNextLoc") +
    # scale_fill_manual(values = c("#e31837", "#002244"), guide = FALSE) +
    # scale_colour_manual(values = c("black", "#c60c30"), guide = FALSE) +
    annotate("text", x = df_hash$x[df_hash$x < 55/2], 
             y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    annotate("text", x = df_hash$x[df_hash$x > 55/2], 
             y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4) + 
    annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
             angle = 90, size = 4) + 
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    geom_polygon(data = triangle_data %>% filter(isOffense == F), aes(x = (xmax - y) + deltaY, y = x + deltaX, group = nflId,
                                           alpha = percentOwnership), fill = "red") +
    # geom_path(data = triangle_data %>% filter(isOffense == F), aes(x = (xmax - y) + deltaY, y = x + deltaX, group = nflId), 
    #           fill = "black", size = 1) +
    geom_segment(data = single_play, aes(x = (xmax - y), y = x, xend = (xmax - y) + s_y, yend = x + s_x),
                 size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
    geom_point(data = single_play %>% filter(isOffense == F), aes(x = (xmax-y), y = x, shape = club,
                                                                      group = nflId, alpha = opponentOwnershipPercentNextLoc), fill = "red", size = 9) +
    geom_point(data = single_play %>% filter(isOffense == F), aes(x = (xmax-y), y = x, shape = club,
                                                                      group = nflId, fill = NA), colour = "black", size = 9) +
    geom_point(data = single_play, aes(x = (xmax-y), y = x, shape = club,
                                       fill = team_color, group = nflId, size = club)) +
    geom_text(data = single_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white",
              vjust = 0.36, size = 3.5) +
    # geom_segment(data = single_play, aes(x = y, y = x, xend = y + s_y, yend = x + x+y),
    #              size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
    ylim(ymin, ymax) +
    coord_fixed() +
    cowplot::theme_nothing() +
    theme(plot.title = element_text()) +
    labs(title = plot_title)
  
}

plot_frame(2022091500, 3350, 7)

animate_play = function(game, play){
  pull_influence = calculateInfluence %>%
    filter(gameId == game, playId == play) %>%
    select(gameId, playId, frameId, nflId, percentOwnership)
  
  pull_difficulty = pursuitQuality %>%
    filter(gameId == game, playId == play) %>%
    select(gameId, playId, frameId, nflId, opponentOwnershipPercentNextLoc)
  
  # medianPercentOwnership = median(pull_influence$percentOwnership)
  
  single_play = trackingData %>%
    filter(gameId == game, playId == play) %>%
    left_join(colorsLogos,
              by = c("club" = "team_abbr")) %>%
    mutate(team_color = ifelse(ballCarrierId == nflId, "orange", team_color),
           team_color2 = ifelse(ballCarrierId == nflId, "black", team_color2)) %>%
    left_join(pull_influence) %>%
    left_join(pull_difficulty) %>%
    group_by(frameId, nflId) %>%
    mutate(s_x = s*cos(dir - pi/2), s_y = s*sin(dir - pi/2),
           cone_x1 = 3*cos(dir - pi/2 + pi/4), cone_y1 = 3*sin(dir - pi/2 + pi/4),
           cone_x2 = 3*cos(dir - pi/2 - pi/4), cone_y2 = 3*sin(dir - pi/2 - pi/4))
  
  triangle_data = single_play %>%
    select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y) %>% mutate(newX = x, newY = y) %>%
    bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y, cone_x1, cone_y1) %>% 
                mutate(newX = x + cone_x1, newY = y + cone_y1) %>% select(-c(cone_x1, cone_y1))) %>%
    bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y, cone_x2, cone_y2) %>% 
                mutate(newX = x + cone_x2, newY = y + cone_y2) %>% select(-c(cone_x2, cone_y2))) %>%
    bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y) %>% mutate(newX = x, newY = y)) %>%
    mutate(deltaX = newX - x, deltaY = newY - y)
  # "#654321"
  # s_x = s*cos(dir),
  # s_y = s*sin(dir))
  
  # %>%
  #   mutate(text_color = case_when(
  #     percentOwnership > medianPercentOwnership ~ "white",
  #     ballCarrierId == nflId ~ "white",
  #     .default = "black"
  #   ))
  
  
  ## General field boundaries
  xmin <- 0
  xmax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(single_play$playDescription[1])))
  
  ## Specific boundaries for a given play
  ymin <- max(round(min(single_play$x, na.rm = TRUE) - 10, -1), 0)
  ymax <- min(round(max(single_play$x, na.rm = TRUE) + 10, -1), 120)
  df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
  df_hash <- df_hash %>% filter(y < ymax, y > ymin)
  
  animate_play = ggplot() +
    scale_size_manual(values = c(6, 6), guide = FALSE) +
    scale_shape_manual(values = c(21, 21), guide = FALSE) +
    scale_fill_identity() +
    # scale_colour_identity() +
    scale_alpha_identity() +
    # scale_alpha_continuous(range = c(.4, .8), name = "opponentOwnershipPercentNextLoc") +
    # scale_fill_manual(values = c("#e31837", "#002244"), guide = FALSE) +
    # scale_colour_manual(values = c("black", "#c60c30"), guide = FALSE) +
    annotate("text", x = df_hash$x[df_hash$x < 55/2], 
             y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    annotate("text", x = df_hash$x[df_hash$x > 55/2], 
             y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4) + 
    annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
             angle = 90, size = 4) + 
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    geom_polygon(data = triangle_data %>% filter(isOffense == F), aes(x = (xmax - y) + deltaY, y = x + deltaX, group = nflId,
                                                                      alpha = percentOwnership), fill = "red") +
    geom_segment(data = single_play, aes(x = (xmax - y), y = x, xend = (xmax - y) + s_y, yend = x + s_x),
                 size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
    geom_point(data = single_play %>% filter(isOffense == F), aes(x = (xmax-y), y = x, shape = club,
                                                                  group = nflId, alpha = opponentOwnershipPercentNextLoc), fill = "red", size = 9) +
    geom_point(data = single_play %>% filter(isOffense == F), aes(x = (xmax-y), y = x, shape = club,
                                                                  group = nflId, fill = NA), colour = "black", size = 9) +
    geom_point(data = single_play, aes(x = (xmax-y), y = x, shape = club,
                                       fill = team_color, group = nflId, size = club)) +
    geom_text(data = single_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white",
              vjust = 0.36, size = 3.5) +
    # geom_segment(data = single_play, aes(x = y, y = x, xend = y + s_y, yend = x + x+y),
    #              size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
    ylim(ymin, ymax) +
    coord_fixed() +
    cowplot::theme_nothing() +
    theme(plot.title = element_text()) +
    labs(title = plot_title) +
    transition_states(frameId)  +
    ease_aes('linear') +
    NULL
  
  
  # animate_play
  
  # 
  # ## Ensure timing of play matches 10 frames-per-second
  play.length.ex <- length(unique(single_play$frameId))
  animate(animate_play, fps = 10, nframe = play.length.ex, res = 150, renderer = magick_renderer())
}

play = animate_play(2022091500, 3350)

x = plays %>% filter(defensiveTeam == "LAC") %>%
  left_join(tackles %>% select(gameId, playId, tackle:pff_missedTackle)) %>%
  group_by(playId) %>%
  filter(sum(pff_missedTackle) >= 1, !is.na(passResult))

# magick::image_write(
#   animate(gif, width = 1000, height = 1000), 
#   "DEN_LAC_Play.gif"
# )

track_SO = function(game, play, ids){
  pull_influence = calculateInfluence %>%
    filter(gameId == game, playId == play,
           nflId %in% ids) %>%
    select(gameId, playId, frameId, nflId, displayName, percentOwnership)
  
  anim = ggplot(pull_influence, aes(frameId, percentOwnership, color = displayName)) +
    geom_line() +
    theme_bw() +
    transition_reveal(frameId) +
    theme(legend.position = "none")
  
  
  play.length.ex <- max(pull_influence$frameId) - min(pull_influence$frameId)
  animate(anim, fps = 10, nframe = play.length.ex, height = 2, width = 3, units = "in", res = 150, renderer = magick_renderer())
  
}

track_diff = function(game, play, ids){
  pull_influence = calculateInfluence %>%
    filter(gameId == game, playId == play,
           nflId %in% ids) %>%
    select(gameId, playId, frameId, nflId, displayName, percentOwnership) %>%
    left_join(pursuitQuality %>% select(gameId, playId, frameId, nflId, displayName, opponentOwnershipPercentNextLoc,
                                        cumulativePursuitQuality))
  
  anim = ggplot(pull_influence, aes(frameId, opponentOwnershipPercentNextLoc, color = displayName)) +
    geom_line() +
    theme_bw() +
    transition_reveal(frameId) +
    theme(legend.position = "none")
  
  
  play.length.ex <- max(pull_influence$frameId) - min(pull_influence$frameId)
  animate(anim, fps = 10, nframe = play.length.ex, height = 2, width = 3, units = "in", res = 150, renderer = magick_renderer())
  
}

track_PQ = function(game, play, ids){
  pull_influence = calculateInfluence %>%
    filter(gameId == game, playId == play,
           nflId %in% ids) %>%
    select(gameId, playId, frameId, nflId, displayName, percentOwnership) %>%
    left_join(pursuitQuality %>% select(gameId, playId, frameId, nflId, displayName, opponentOwnershipPercentNextLoc,
                                        cumulativePursuitQuality))
  
  anim = ggplot(pull_influence, aes(frameId, cumulativePursuitQuality, color = displayName)) +
    geom_line() +
    theme_bw() +
    transition_reveal(frameId) +
    theme(legend.position = "none")
  
  
  play.length.ex <- max(pull_influence$frameId) - min(pull_influence$frameId)
  animate(anim, fps = 10, nframe = play.length.ex, height = 2, width = 3, units = "in", res = 150, renderer = magick_renderer())
  
}



p1 = track_SO(2022091500, 3350, c(46775, 46086))
p2 = track_diff(2022091500, 3350, c(46775, 46086))
p3 = track_PQ(2022091500, 3350, c(46775, 46086))

p1Gif = image_read(p1)
p2Gif = image_read(p2)
p3Gif = image_read(p3)


newGif = image_append(c(p1Gif[1], p2Gif[1], p3Gif[1]))

for(i in 2:length(p2Gif)){
  combined <- image_append(c(p1Gif[i], p2Gif[i], p3Gif[i]))
  newGif <- c(newGif, combined)
}

newGif

# pull_influence = calculateInfluence %>%
#   filter(gameId == 2022091500, playId == 3350,
#          nflId %in% c(46775, 46086), frameId > min(frameId)) %>%
#   select(gameId, playId, frameId, nflId, displayName, percentOwnership) %>%
#   left_join(pursuitQuality %>% select(gameId, playId, frameId, nflId, displayName, opponentOwnershipPercentNextLoc,
#                                       cumulativePursuitQuality))
#   
# p1 = ggplot(pull_influence, aes(frameId, percentOwnership, color = displayName)) +
#   geom_line() +
#   theme_bw()
# 
# p2 = ggplot(pull_influence, aes(frameId, opponentOwnershipPercentNextLoc, color = displayName)) +
#   geom_line() +
#   theme_bw()
# 
# patchwork::
# 
# x = calculateInfluence %>%
#   filter(gameId == 2022091500, playId == 3350) %>%
#   select(gameId, playId, frameId, nflId, isOffense, percentOwnership)
