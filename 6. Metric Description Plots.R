library(knitr)
library(gridExtra)
library(grid)


custom_legend <- function() {
  grobTree(
    rectGrob(width = 0.5, height = 0.5, gp = gpar(fill = "red")),
    textGrob("Custom Legend", hjust = 0, x = 0.6, y = 0.25, gp = gpar(col = "black"))
  )
}

rotate_plot_frame_SO = function(game, play, alphaScale){
  
  pull_influence = calculateInfluence %>%
    filter(gameId == game, playId == play) %>%
    select(gameId, playId, frameId, club, nflId, percentOwnership)
  
  pull_difficulty = pursuitQuality %>%
    filter(gameId == game, playId == play) %>%
    select(gameId, playId, frameId, nflId, opponentOwnershipPercentNextLoc, cumulativePursuitQuality)
  
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
    mutate(s_x = s*sin(dir), s_y = s*cos(dir),
           cone_x1 = 2.5*sin(dir + pi/4), cone_y1 = 2.5*cos(dir + pi/4),
           cone_x2 = 2.5*sin(dir - pi/4), cone_y2 = 2.5*cos(dir - pi/4),
           cumulativePursuitQuality = ifelse(is.na(cumulativePursuitQuality), 0, cumulativePursuitQuality))
  
  triangle_data = single_play %>%
    select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y) %>% mutate(newX = x, newY = y) %>%
    bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y, cone_x1, cone_y1) %>% 
                mutate(newX = x + cone_x1, newY = y + cone_y1) %>% select(-c(cone_x1, cone_y1))) %>%
    bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y, cone_x2, cone_y2) %>% 
                mutate(newX = x + cone_x2, newY = y + cone_y2) %>% select(-c(cone_x2, cone_y2))) %>%
    bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y) %>% mutate(newX = x, newY = y)) %>%
    mutate(deltaX = newX - x, deltaY = newY - y)
  
  offense_color = single_play$team_color[single_play$isOffense == T][1]
  offense_color2 = single_play$team_color2[single_play$isOffense == T][1]
  defense_color = single_play$team_color[single_play$isOffense == F][1]
  defense_color2 = single_play$team_color2[single_play$isOffense == F][1]
  
  offense_club = single_play$club[single_play$isOffense == T][1]
  defense_club = single_play$club[single_play$isOffense == F][1]
  
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
    scale_color_identity() +
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
    # geom_polygon(data = triangle_data %>% filter(isOffense == F), aes(x = x + deltaX, y = y + deltaY, group = nflId,
    #                                                                   alpha = percentOwnership), fill = "red") +
    geom_segment(data = single_play, aes(x = x, y = y, xend = x + s_x, yend = y + s_y),
                 size = 1, alpha = .5, arrow = arrow(length = unit(0.01, "npc"))) +
    # geom_point(data = single_play %>% filter(isOffense == F, nflId %in% ids), aes(x = x, y = y, shape = club,
    #                                                                               group = nflId, alpha = cumulativePursuitQuality*2), fill = "red", size = 9) +
    # geom_point(data = single_play %>% filter(isOffense == F), aes(x = x, y = y, shape = club,
    #                                                                               group = nflId, fill = NA), colour = "black", size = 9) +
    geom_point(data = single_play, aes(x = x, y = y, shape = club,
                                       group = nflId, size = club, color = team_color2), fill = NA) +
    geom_point(data = single_play, aes(x = x, y = y, shape = club,
                                       fill = team_color, alpha = percentOwnership * alphaScale, group = nflId, size = club)) +
    geom_text(data = single_play, aes(x = x, y = y, label = jerseyNumber), colour = "black",
              vjust = 0.36, size = 3.5) +
    # geom_segment(data = single_play, aes(x = y, y = x, xend = y + s_y, yend = x + x+y),
    #              size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
    coord_fixed() +
    cowplot::theme_nothing() +
    transition_states(frameId)  +
    labs(title = plot_title) +
    # guides(color = guide_legend(
    #   title = "Teams",
    #   override.aes = list(shape = c(offense_club = 16, defense_club = 16, "Ball Carrier" = 16),
    #                    color = c(offense_club = offense_color2, defense_club = defense_color2, "Ball Carrier" = "black"),
    #                    fill = c(offense_club = offense_color, defense_club = defense_color, "Ball Carrier" = "orange"))
    # )) +
    theme(plot.title = element_text()) +
    annotate('rect', xmin = 0, xmax = 120, ymin = 160/3, ymax = 70, color = 'black', fill = 'lightblue') +
    annotate('text', x = 60, y = 61.5, label = "Here is where the text will go!")

  
  play.length.ex <- max(pull_influence$frameId) - min(pull_influence$frameId)
  animate(anim, fps = 10, nframe = play.length.ex, height = 4, width = 9, units = "in", res = 150, renderer = magick_renderer())
  
}


SO_GIF = rotate_plot_frame_SO(2022091101, 2832, 3)
anim_save('SO.gif', SO_GIF)


rotate_plot_frame_Path_Difficulty = function(game, play, tacklerId){
  
  pull_influence = calculateInfluence %>%
    filter(gameId == game, playId == play) %>%
    select(gameId, playId, frameId, club, nflId, percentOwnership)
  
  pull_difficulty = pursuitQuality %>%
    filter(gameId == game, playId == play) %>%
    select(gameId, playId, frameId, nflId, opponentOwnershipPercentNextLoc, cumulativePursuitQuality)
  
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
    mutate(s_x = s*sin(dir), s_y = s*cos(dir),
           cone_x1 = 2.5*sin(dir + pi/4), cone_y1 = 2.5*cos(dir + pi/4),
           cone_x2 = 2.5*sin(dir - pi/4), cone_y2 = 2.5*cos(dir - pi/4),
           cumulativePursuitQuality = ifelse(is.na(cumulativePursuitQuality), 0, cumulativePursuitQuality))
  
  triangle_data = single_play %>%
    select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y) %>% mutate(newX = x, newY = y) %>%
    bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y, cone_x1, cone_y1) %>% 
                mutate(newX = x + cone_x1, newY = y + cone_y1) %>% select(-c(cone_x1, cone_y1))) %>%
    bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y, cone_x2, cone_y2) %>% 
                mutate(newX = x + cone_x2, newY = y + cone_y2) %>% select(-c(cone_x2, cone_y2))) %>%
    bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y) %>% mutate(newX = x, newY = y)) %>%
    mutate(deltaX = newX - x, deltaY = newY - y)
  
  tackler_location =  trackingData %>%
    filter(gameId == game, playId == play, nflId == tacklerId) %>%
    left_join(pull_difficulty) %>%
    filter(frameId > min(frameId)) %>%
    select(x, y, opponentOwnershipPercentNextLoc)
  
  offense_color = single_play$team_color[single_play$isOffense == T][1]
  offense_color2 = single_play$team_color2[single_play$isOffense == T][1]
  defense_color = single_play$team_color[single_play$isOffense == F][1]
  defense_color2 = single_play$team_color2[single_play$isOffense == F][1]
  
  offense_club = single_play$club[single_play$isOffense == T][1]
  defense_club = single_play$club[single_play$isOffense == F][1]
  
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
    # scale_color_gradientn(low = "green", high = "red", name = "opponentOwnershipPercentNextLoc") +
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
    geom_segment(data = single_play %>% filter(nflId != tacklerId), aes(x = x, y = y, xend = x + s_x, yend = y + s_y),
                 size = 1, alpha = .5, arrow = arrow(length = unit(0.01, "npc"))) +
    # geom_point(data = single_play %>% filter(isOffense == F, nflId %in% ids), aes(x = x, y = y, shape = club,
    #                                                                               group = nflId, alpha = cumulativePursuitQuality*2), fill = "red", size = 9) +
    # geom_point(data = single_play %>% filter(isOffense == F), aes(x = x, y = y, shape = club,
    #                                                                               group = nflId, fill = NA), colour = "black", size = 9) +
    geom_point(data = single_play, aes(x = x, y = y, shape = club,
                                       group = nflId, size = club), color='black', fill = NA) +
    geom_point(data = single_play, aes(x = x, y = y, shape = club,
                                       fill = team_color, group = nflId, size = club)) +
    geom_text(data = single_play, aes(x = x, y = y, label = jerseyNumber), colour = "white",
              vjust = 0.36, size = 3.5) +
    # geom_segment(data = single_play, aes(x = y, y = x, xend = y + s_y, yend = x + x+y),
    #              size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
    coord_fixed() +
    cowplot::theme_nothing() +
    transition_states(frameId)  +
    labs(title = plot_title) +
    # guides(color = guide_legend(
    #   title = "Teams",
    #   override.aes = list(shape = c(offense_club = 16, defense_club = 16, "Ball Carrier" = 16),
    #                    color = c(offense_club = offense_color2, defense_club = defense_color2, "Ball Carrier" = "black"),
    #                    fill = c(offense_club = offense_color, defense_club = defense_color, "Ball Carrier" = "orange"))
    # )) +
    theme(plot.title = element_text()) +
    annotate('rect', xmin = 0, xmax = 120, ymin = 160/3, ymax = 70, color = 'black', fill = 'lightblue') +
    annotate('text', x = 60, y = 61.5, label = "Here is where the text will go!")
  
  play.length.ex <- max(pull_influence$frameId) - min(pull_influence$frameId)
  animate(anim, fps = 10, nframe = play.length.ex, height = 4, width = 9, units = "in", res = 150, renderer = magick_renderer())

}

path_gif = rotate_plot_frame_Path_Difficulty(2022101300, 54, 47956)

anim_save('Path.gif', path_gif)

anim_save('')