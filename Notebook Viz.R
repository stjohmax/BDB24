library(gganimate)
library(transformr)
library(magick)
#2022092510, 3356
start_frames_function = function(game, play, tacklerId, pauseFrame1, pauseFrame2){
  
  pull_influence = calculateInfluence %>%
    filter(gameId == game, playId == play) %>%
    select(gameId, playId, frameId, club, nflId, percentOwnership)
  
  pull_difficulty = pursuitQuality %>%
    filter(gameId == game, playId == play) %>%
    select(gameId, playId, frameId, nflId, opponentOwnershipPercentNextLoc, cumulativePursuitQuality)
  
  maxSOByPlayer = pull_influence %>%
    group_by(nflId) %>%
    filter(percentOwnership == max(percentOwnership)) %>%
    select(percentOwnership, frameId)
  
  
  maxSOFrame = maxSOByPlayer$frameId[maxSOByPlayer$nflId == tacklerId]
  # medianPercentOwnership = median(pull_influence$percentOwnership)
  
  single_play = trackingData %>%
    filter(gameId == game, playId == play,
           frameId <= maxSOFrame) %>%
    left_join(colorsLogos,
              by = c("club" = "team_abbr")) %>%
    mutate(team_color = ifelse(ballCarrierId == nflId, "orange", team_color),
           team_color2 = ifelse(ballCarrierId == nflId, "black", team_color2)) %>%
    left_join(pull_influence) %>%
    left_join(pull_difficulty) %>%
    group_by(frameId, nflId) %>%
    mutate(s_x = s*sin(dir), s_y = s*cos(dir),
           cone_x1 = 2.5*sin(dir + pi/4), cone_y1 = 2.5*cos(dir + pi/4),
           cone_x2 = 2.5*sin(dir - pi/4), cone_y2 = 2.5*cos(dir - pi/4),
           cumulativePursuitQuality = ifelse(is.na(cumulativePursuitQuality), 0, cumulativePursuitQuality)) %>%
    mutate(annotate_x_start = case_when(
      frameId == 7 | frameId == 8 ~ 90,
      .default = NA
    ),
    annotate_y_start = case_when(
      frameId == 7 | frameId == 8 ~ 7.5,
      .default = NA
    ),
      annotate_x_middle1 = case_when(
      frameId == pauseFrame1 | frameId == pauseFrame1 - 1 ~ 90,
      .default = NA
    ),
    annotate_y_middle1 = case_when(
      frameId == pauseFrame1 | frameId == pauseFrame1 - 1 ~ 7.5,
      .default = NA
    ),
    annotate_x_middle2 = case_when(
      frameId == pauseFrame2 | frameId == pauseFrame2 - 1 ~ 90,
      .default = NA
    ),
    annotate_y_middle2 = case_when(
      frameId == pauseFrame2 | frameId == pauseFrame2 - 1 ~ 7.5,
      .default = NA
    ),
    annotate_x_final = case_when(
      frameId == maxSOFrame | frameId == maxSOFrame - 1 ~ 90,
      .default = NA
    ),
    annotate_y_final = case_when(
      frameId == maxSOFrame | frameId == maxSOFrame - 1 ~ 7.5,
      .default = NA
    ))
  
  tackler_location = trackingData %>%
    filter(gameId == game, playId == play, nflId == tacklerId) %>%
    left_join(pull_difficulty) %>%
    filter(frameId > min(frameId),
           frameId <= maxSOFrame) %>%
    select(x, y, opponentOwnershipPercentNextLoc)
  
  offense_color = single_play$team_color[single_play$isOffense == T][1]
  offense_color2 = single_play$team_color2[single_play$isOffense == T][1]
  defense_color = single_play$team_color[single_play$isOffense == F][1]
  defense_color2 = single_play$team_color2[single_play$isOffense == F][1]
  
  offense_club = single_play$club[single_play$isOffense == T][1]
  defense_club = single_play$club[single_play$isOffense == F][1]
  
  maxFrame = max(pull_influence$frameId)
  minFrame = min(pull_influence$frameId)
  
  transition_len = c(1, 120, rep(1, pauseFrame1 - minFrame - 3), 120, rep(1, pauseFrame2 - pauseFrame1 - 1), 120, rep(1, maxSOFrame - pauseFrame2 - 1), 120)
  
  startLabel = "The fill of the circle around #34 tracks his Space \n Ownership of the ball carrier, while the line tracks \n his Path Difficulty throughout the play."
  middleLabel1 = "As #34 gets further away from offensive players \n that could possibly hinder his path to the \n ball carrier, his path becomes easier."
  middleLabel2 = "As #34 approaches the ball carrier, his Space Ownership \n increases, as he has stronger control of the space the \n ball carrier occupies."
  finalLabel = "At this point, #34 has reached his peak Space \n Ownership. We will evaluate his pursuit of \n the ball carrier up to this point."
  ## General field boundaries
  ymin <- 0
  ymax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(single_play$playDescription[1])))
  
  ## Specific boundaries for a given play
  xmin <- 0
  xmax <- 120
  df_hash <- expand.grid(y = c(0, 23.36667, 29.96667, ymax), x = (10:110))
  df_hash <- df_hash %>% filter(!(floor(x %% 5) == 0))
  df_hash <- df_hash %>% filter(x < xmax, x > xmin)
  
  anim = ggplot() +
    scale_size_manual(values = c(6, 6), guide = FALSE) +
    scale_shape_manual(values = c(21, 21), guide = FALSE) +
    scale_fill_identity() +
    scale_alpha_identity() +
    scale_colour_gradient2(low = "green", mid = "yellow" , high = "red", 
                           midpoint=.5) +
    # scale_colour_identity() +
    annotate("text", y = 23.36667, 
             x = df_hash$x[df_hash$y < 55/2], label = "_", hjust = .5, vjust = -0.2, color = "grey", size = .9) + 
    annotate("text", y = 29.96667, 
             x = df_hash$x[df_hash$y > 55/2], label = "_", hjust = .5, vjust = -0.2, color = "grey", size = .9) + 
    annotate("segment", y = ymin, 
             x = seq(max(10, xmin), min(xmax, 110), by = 5), 
             yend =  ymax, 
             xend = seq(max(10, xmin), min(xmax, 110), by = 5), color = "grey") + 
    annotate("text", y = rep(hash.left, 11), x = seq(10, 110, by = 10), 
             label = c("G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G"), 
             angle = 0, size = 4, color = "grey") + 
    annotate("text", y = rep((ymax - hash.left), 11), x = seq(10, 110, by = 10), 
             label = c("G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G"), 
             angle = 180, size = 4, color = "grey") + 
    annotate("segment", y = c(ymin, ymin, ymax, ymax), 
             x = c(xmin, xmax, xmax, xmin), 
             yend = c(ymin, ymax, ymax, ymin), 
             xend = c(xmax, xmax, xmin, xmin), color = "grey") +
    geom_path(data = tackler_location, aes(x=x, y=y, color = opponentOwnershipPercentNextLoc)) +
    # geom_polygon(data = triangle_data %>% filter(isOffense == F), aes(x = x + deltaX, y = y + deltaY, group = nflId,
    #                                                                   alpha = percentOwnership), fill = "red") +
    geom_segment(data = single_play, aes(x = x, y = y, xend = x + s_x, yend = y + s_y),
                 size = 1, alpha = .5, arrow = arrow(length = unit(0.01, "npc"))) +
    geom_point(data = single_play %>% filter(isOffense == F, nflId == tacklerId), aes(x = x, y = y, shape = club,
                                                                                  group = nflId, alpha = percentOwnership), fill = "red", size = 9) +
    geom_point(data = single_play %>% filter(isOffense == F, nflId == tacklerId), aes(x = x, y = y, shape = club,
                                                                                      group = nflId, ), fill = NA, color='black', size = 9) +
    # geom_point(data = single_play %>% filter(isOffense == F, nflId %in% ids), aes(x = x, y = y, shape = club,
    #                                                                               group = nflId, alpha = cumulativePursuitQuality*2), fill = "red", size = 9) +
    # geom_point(data = single_play %>% filter(isOffense == F), aes(x = x, y = y, shape = club,
    #                                                                               group = nflId, fill = NA), colour = "black", size = 9) +
    geom_point(data = single_play, aes(x = x, y = y, shape = club,
                                       group = nflId, size = club), color="black", fill = NA) +
    geom_point(data = single_play, aes(x = x, y = y, shape = club,
                                       fill = team_color, group = nflId, size = club)) +
    geom_text(data = single_play, aes(x = x, y = y, label = jerseyNumber), colour = "white",
              vjust = 0.36, size = 3.5) +
    geom_label(data = single_play %>% filter(nflId == tacklerId), aes(x = annotate_x_start, y = annotate_y_start), label = startLabel, fill = "lightblue", size = 3.5) +
    geom_label(data = single_play %>% filter(nflId == tacklerId), aes(x = annotate_x_middle1, y = annotate_y_middle1), label = middleLabel1, fill = "lightblue", size = 3.5) +
    geom_label(data = single_play %>% filter(nflId == tacklerId), aes(x = annotate_x_middle2, y = annotate_y_middle2), label = middleLabel2, fill = "lightblue", size = 3.5) +
    geom_label(data = single_play %>% filter(nflId == tacklerId), aes(x = annotate_x_final, y = annotate_y_final), label = finalLabel, fill = "lightblue", size = 3.5) +
    
    # geom_segment(data = single_play, aes(x = y, y = x, xend = y + s_y, yend = x + x+y),
    #              size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
    coord_fixed() +
    cowplot::theme_nothing() +
    transition_states(frameId, state_length = 0,
                      transition_length = transition_len)  +
    #enter_manual(frameId == pauseFrame, annotate('rect', xmin = 80, xmax = 90, ymin = 18, ymax = 23, color = 'black', fill = 'lightblue')) +
    labs(title = plot_title) +
    # guides(color = guide_legend(
    #   title = "Teams",
    #   override.aes = list(shape = c(offense_club = 16, defense_club = 16, "Ball Carrier" = 16),
    #                    color = c(offense_club = offense_color2, defense_club = defense_color2, "Ball Carrier" = "black"),
    #                    fill = c(offense_club = offense_color, defense_club = defense_color, "Ball Carrier" = "orange"))
    # )) +
    theme(plot.title = element_text()) 
  # +
  #   annotate('rect', xmin = 0, xmax = 120, ymin = 160/3, ymax = 70, color = 'black', fill = 'lightblue') +
  #   annotate('text', x = 60, y = 61.5, label = "Here is where the text will go!")
  
  
  play.length.ex <- maxSOFrame - min(pull_influence$frameId) + 476
  animate(anim, fps = 10, nframe = play.length.ex, height = 4, width = 9, units = "in", res = 150, renderer = magick_renderer())
  
}

start = start_frames_function(2022092510, 3356, 49410, 35, 40)
start

anim_save('Full Play Metric Description.gif', start)

