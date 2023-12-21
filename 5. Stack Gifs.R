rotate_animate_play = function(game, play, ids){

pull_influence = calculateInfluence %>%
  filter(gameId == game, playId == play) %>%
  select(gameId, playId, frameId, nflId, percentOwnership)

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

animate_play = ggplot() +
  scale_size_manual(values = c(6, 6), guide = FALSE) +
  scale_shape_manual(values = c(21, 21), guide = FALSE) +
  scale_fill_identity() +
  scale_alpha_identity() +
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
               size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = single_play %>% filter(isOffense == F, nflId %in% ids), aes(x = x, y = y, shape = club,
                                                                group = nflId, alpha = cumulativePursuitQuality*2), fill = "red", size = 9) +
  geom_point(data = single_play %>% filter(isOffense == F, nflId %in% ids), aes(x = x, y = y, shape = club,
                                                                group = nflId, fill = NA), colour = "black", size = 9) +
  geom_point(data = single_play, aes(x = x, y = y, shape = club,
                                     fill = team_color, group = nflId, size = club)) +
  geom_text(data = single_play, aes(x = x, y = y, label = jerseyNumber), colour = "white",
            vjust = 0.36, size = 3.5) +
  # geom_segment(data = single_play, aes(x = y, y = x, xend = y + s_y, yend = x + x+y),
  #              size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
  coord_fixed() +
  cowplot::theme_nothing() +
  transition_states(frameId)  +
  theme(plot.title = element_text()) +
  labs(title = plot_title)

play.length.ex <- length(unique(single_play$frameId))
animate(animate_play, fps = 10, nframe = play.length.ex, height = 4, width = 9, units = "in", res = 150, renderer = magick_renderer())

}

track_SO = function(game, play, ids){
  pull_influence = calculateInfluence %>%
    filter(gameId == game, playId == play,
           nflId %in% ids) %>%
    mutate(Player = ifelse(nchar(jerseyNumber) == 1, paste0(jerseyNumber, "  ", displayName), 
                           paste0(jerseyNumber, " ", displayName))) %>%
    select(gameId, playId, frameId, nflId, Player, percentOwnership)
  
  anim = ggplot(pull_influence, aes(frameId, percentOwnership, color = Player)) +
    geom_line() +
    theme_bw() +
    transition_reveal(frameId) +
    theme(
      legend.position = "none",
      axis.title = element_text(size = rel(.8)),
          axis.text = element_text(size = rel(.8)),
          panel.grid.minor = element_blank()) +
    ylab("Space Ownership") +
    ylim(-.1, 1)
  
  
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
    theme(legend.position = "none", axis.title = element_text(size = rel(.8)),
          axis.text = element_text(size = rel(.8)),
          panel.grid.minor = element_blank()) +
    ylab("Path Difficulty") +
    ylim(-.1, 1)
  
  
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
    theme(legend.position = c(.3, .75),
          legend.box.background = element_rect(colour = "black"),
          legend.key.height = unit(.1, 'in'), axis.title = element_text(size = rel(.8)),
          axis.text = element_text(size = rel(.8)),
          panel.grid.minor = element_blank()) +
    ylab("Cumulative Pursuit Quality") +
    ylim(-.1, 1)
  
  
  play.length.ex <- max(pull_influence$frameId) - min(pull_influence$frameId)
  animate(anim, fps = 10, nframe = play.length.ex, height = 2, width = 3, units = "in", res = 150, renderer = magick_renderer())
  
}

#blank_image = image_blank(width = 50, height = 200, color = "white")


play_gif = rotate_animate_play(2022092510, 3356, c(49410, 52945))

p1 = track_SO(2022092510, 3356, c(49410, 52945))
p2 = track_diff(2022092510, 3356, c(49410, 52945))
p3 = track_PQ(2022092510, 3356, c(49410, 52945))


plots_combined = image_append(c(p1[1], p2[1], p3[1]))

new_gif = image_append(c(play_gif[1], image_flatten(plots_combined)), stack = T)

for(i in 2:length(p2)){
  plots_combined = image_append(c(p1[i], p2[i], p3[i]))
  
  full_combined = image_append(c(play_gif[i], image_flatten(plots_combined)), stack = T)
  new_gif = c(new_gif, full_combined)
}

new_gif

anim_save('Full Play.gif', new_gif)
