library(gganimate)
library(transformr)
library(magick)
library(kableExtra)
library(DT)
library(webshot2)
library(htmltools)
library(gt)
library(gtExtras)
library(grid)
library(gridExtra)
library(png)
library(patchwork)
library(cowplot)
library(gtsummary)
library(plotly)
library(ggimage)
library(RColorBrewer)



### SO + Path Difficulty GIF ###

# Draw Legend
sampleLegendData = tibble(x = c(0, 1), y = 0)

SOLegendDraw = ggplot(sampleLegendData, aes(x, y, color = x)) +
  geom_point() +
  scale_colour_gradient2(low = alpha("red", 0), mid = alpha("red", .5) , high = "red", 
                         midpoint=.5, name = "Space\nOwnership") +
  theme(legend.title = element_text(face = "bold", size = 7),
        legend.key.width = unit(.125, "in")) +
  guides(
    color = guide_colorsteps(
      title.position = "top",
      barwidth = unit(.1, "in"),
      # barheight = unit(.125, "in")
    )
  )

pathDifficultyLegendDraw = ggplot(sampleLegendData, aes(x, y, color = x)) +
  geom_point() +
  scale_colour_gradient2(low = "#E0ECF4", mid = "#9EBCDA" , high = "#8856A7", 
                         midpoint=.5, name = "Path\nDifficulty") +
  theme(legend.title = element_text(face = "bold", size = 7),
        legend.key.width = unit(.125, "in")) +
  guides(
    color = guide_colorsteps(
      title.position = "top",
      barwidth = unit(.1, "in"),
      # barheight = unit(.125, "in")
    )
  )

SOLegend = as.ggplot(get_legend(SOLegendDraw))
SOLegend
pathDifficultyLegend = as.ggplot(get_legend(pathDifficultyLegendDraw))
pathDifficultyLegend

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
      frameId == 7 | frameId == 8 ~ 35,
      .default = NA
    ),
    annotate_y_start = case_when(
      frameId == 7 | frameId == 8 ~ 7.5,
      .default = NA
    ),
      annotate_x_middle1 = case_when(
      frameId == pauseFrame1 | frameId == pauseFrame1 - 1 ~ 35,
      .default = NA
    ),
    annotate_y_middle1 = case_when(
      frameId == pauseFrame1 | frameId == pauseFrame1 - 1 ~ 7.5,
      .default = NA
    ),
    annotate_x_middle2 = case_when(
      frameId == pauseFrame2 | frameId == pauseFrame2 - 1 ~ 35,
      .default = NA
    ),
    annotate_y_middle2 = case_when(
      frameId == pauseFrame2 | frameId == pauseFrame2 - 1 ~ 7.5,
      .default = NA
    ),
    annotate_x_final = case_when(
      frameId == maxSOFrame | frameId == maxSOFrame - 1 ~ 35,
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
  
  transition_len = c(1, 100, rep(1, pauseFrame1 - minFrame - 3), 100, rep(1, pauseFrame2 - pauseFrame1 - 1), 100, rep(1, maxSOFrame - pauseFrame2 - 1), 100)
  #transition_len = c(1, 1, rep(1, pauseFrame1 - minFrame - 3), 1, rep(1, pauseFrame2 - pauseFrame1 - 1), 1, rep(1, maxSOFrame - pauseFrame2 - 1), 1)
  
  startLabel = "The fill of the circle around #43 tracks his Space \n Ownership of the ball carrier, while the line tracks \n his Path Difficulty throughout the play."
  middleLabel1 = "As #43 gets further away from offensive players \n that could possibly hinder his path to the \n ball carrier, his path becomes easier."
  middleLabel2 = "As #43 approaches the ball carrier, his Space Ownership \n increases, as he has stronger control of the space the \n ball carrier occupies."
  finalLabel = "At this point, #43 has reached his peak Space \n Ownership. We will evaluate his pursuit of \n the ball carrier up to this point."
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
    scale_colour_gradient2(low = "#E0ECF4", mid = "#9EBCDA" , high = "#8856A7", 
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
    geom_path(data = tackler_location, aes(x=x, y=y, color = opponentOwnershipPercentNextLoc), size = 2) +
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
    annotation_custom(ggplotGrob(SOLegend),
                      xmin = 122, xmax = 129.5,
                      ymin = 33, ymax = 48) +
    annotation_custom(ggplotGrob(pathDifficultyLegend),
                      xmin = 120.5, xmax = 129.5,
                      ymin = 6, ymax = 21) +
    
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
  
  
  play.length.ex <- maxSOFrame - min(pull_influence$frameId) + 396
  #play.length.ex <- maxSOFrame - min(pull_influence$frameId) + 1
  animate(anim, fps = 10, nframe = play.length.ex, height = 4, width = 9, units = "in", res = 125, renderer = magick_renderer())
  
}

fullPlayMetricDescription = start_frames_function(2022091101, 1744, 44903, 25, 35)
#fullPlayMetricDescription
anim_save('Full Play Metric Description.gif', fullPlayMetricDescription)

### Pass vs Rush PQ by Position Scatterplot ###

vline = mean(pursuitQualitySummary %>% ungroup() %>% filter(pursuitOpps >= 100) %>% 
               select(Run) %>% as.list() %>% unlist(), na.rm = TRUE)
hline = mean(pursuitQualitySummary %>% ungroup() %>% filter(pursuitOpps >= 100) %>% 
               select(Pass) %>% as.list() %>% unlist(), na.rm = TRUE)

passVsRushPQScatter = ggplot(pursuitQualitySummary %>% filter(pursuitOpps >= 100), aes(Run, Pass, fill = position, color = position)) +
  geom_abline(intercept = seq(-0.04, .22, by = .02), slope = -1, color = "lightgrey") +
  geom_vline(xintercept = vline, color = "black") +
  geom_hline(yintercept = hline, color = "black") +
  geom_point() +
  geom_label(data = positionMidpoints, aes(color = position, label = position), fill = "white", fontface = "bold",
             alpha = .8) +
  scale_color_manual(values = c("#000000", "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7")) +
  theme_bw() +
  xlab("Run PQ per Pursuit Attempt") +
  ylab("Pass PQ per Pursuit Attempt") +
  xlim(-.01, .05) +
  ylim(-.01, .05) +
  ggtitle("Pass vs Run PQ by Position") +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = .5),
        axis.title.y = element_text(hjust = .5),
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

passVsRushPQScatter

ggsave("Pass vs Rush PQ Scatter.png", passVsRushPQScatter, dpi = 420, units = "in")

### PQ Boxplot by Event

pqBoxplot = ggplot(pursuitQualityBoxplotData %>% filter(pQ >= -.25), aes(y = pQ, x = pursuitOutcome, fill = pursuitOutcome)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#2297E6", "#CD0BBC")) +
  ggtitle("Cumulative Pursuit Quality vs Pursuit Outcome") +
  ylab("Cumulative Pursuit Quality") +
  xlab("Pursuit Outcome") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = .5),
        axis.title.y = element_text(hjust = .5),
        plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("Pursuit Quality vs Pursuit Outcome Boxplot.png", pqBoxplot, dpi = 420)


### Top 5 PQ by Position Table ###

pursuitCompositePosTable = function(pos){
  data = pursuitQualityTacklesSummary %>%
    ungroup() %>%
    filter(position == pos) %>%
    arrange(-pursuitComposite) %>%
    select(Name = displayName, Team = team_logo_wikipedia, 'PQ Composite' = pursuitComposite, Snaps = snapCount,
           "Pursuits" = pursuitOpps) %>%
    mutate(`PQ Composite` = round(`PQ Composite`, 3)) %>%
    head(5)
  
  titleString = paste0("**Top 5 ", pos, "**")
  
  data %>%
    gt() %>%
    tab_header(title = md(titleString)) %>%
    data_color(
      columns = vars('PQ Composite'),
      colors = scales::col_numeric(
        palette = "Blues",
        #palette = c('#d0e6ea', '#c6e0e5', '#bddbe1', '#b3d6dd', '#aad1d8', '#a0ccd4', '#90b8bf', '#80a3aa', '#708f94', '#607a7f'),
        domain = c(0.01, .05)
      )
    ) %>%
    tab_options(
      table.border.top.width = 0,
      table.border.bottom.color = "black",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width= px(2.5),
      column_labels.border.top.width = px(2.5),
      column_labels.font.size = px(18),
      table.font.size = px(15.5),
      heading.title.font.size = px(24)
    ) %>%
    cols_align(align = 'center',
               columns = everything()) %>%
    gt_img_rows(Team) %>%
    cols_width(Name ~ px(175),
               `PQ Composite` ~ px(50))
}



table1 = pursuitCompositePosTable('CB')
table2 = pursuitCompositePosTable('DE')
table3 = pursuitCompositePosTable('OLB')
table4 = pursuitCompositePosTable("S")
table5 = pursuitCompositePosTable("IDL")
table6 = pursuitCompositePosTable("ILB")

combined_pursuit_html <- tagList(
  tags$style(
    HTML("
      .table-container {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        grid-template-rows: repeat(2, 1fr);
        gap: 10px;
      }
      .table {
        grid-column: span 1;
        grid-row: span 1;
      }
    ")
  ),
  div(class = "table-container",
      div(class = "table", table1),
      div(class = "table", table2),
      div(class = "table", table3),
      div(class = "table", table4),
      div(class = "table", table5),
      div(class = "table", table6)
  )
)

save_html(combined_pursuit_html, "PQ Composite Table.html")



webshot("Pursuit Composite Table.html", "Pursuit Composite Table.png")

### Tackling Table ###
tackleValueLostPosTable = function(pos){
  data = pursuitQualityTacklesSummary %>%
    ungroup() %>%
    filter(position == pos) %>%
    arrange(tackleValueLostPerAttempt) %>%
    select(Name = displayName, Team = team_logo_wikipedia, 'TVL per Attempt' = tackleValueLostPerAttempt, Snaps = snapCount,
           Tackles = tackles) %>%
    mutate(`TVL per Attempt` = round(`TVL per Attempt`, 3)) %>%
    head(5)
  
  titleString = paste0("**Top 5 ", pos, "**")
  
  data %>%
    gt() %>%
    tab_header(title = md(titleString)) %>%
    data_color(
      columns = vars('TVL per Attempt'),
      colors = scales::col_numeric(
        palette = "Blues",
        domain = c(.0004, .03),
        reverse = TRUE
      )
    ) %>%
    tab_options(
      table.border.top.width = 0,
      table.border.bottom.color = "black",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width= px(2.5),
      column_labels.border.top.width = px(2.5),
      column_labels.font.size = px(18),
      table.font.size = px(15.5),
      heading.title.font.size = px(24)
    ) %>%
    cols_align(align = 'center',
               columns = everything()) %>%
    gt_img_rows(Team) %>%
    cols_width(Name ~ px(175),
               `TVL per Attempt` ~ px(50))
}



table1 = tackleValueLostPosTable('CB')
table2 = tackleValueLostPosTable('DE')
table3 = tackleValueLostPosTable('OLB')
table4 = tackleValueLostPosTable("S")
table5 = tackleValueLostPosTable("IDL")
table6 = tackleValueLostPosTable("ILB")

combined_tackling_html <- tagList(
  tags$style(
    HTML("
      .table-container {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        grid-template-rows: repeat(2, 1fr);
        gap: 10px;
      }
      .table {
        grid-column: span 1;
        grid-row: span 1;
      }
    ")
  ),
  div(class = "table-container",
      div(class = "table", table1),
      div(class = "table", table2),
      div(class = "table", table3),
      div(class = "table", table4),
      div(class = "table", table5),
      div(class = "table", table6)
  )
)

save_html(combined_tackling_html, "Tackle Composite Table.html")

### Tackle Event Boxplot ###
tvlBoxplot = ggplot(tacklePlaySummary %>% filter(tackleValueLost <= .6), aes(y = tackleValueLost, x = tackleResult, fill = tackleResult)) +
  geom_boxplot() +
  scale_fill_manual(values = c("violet", "lightblue")) + 
  ylab("Tackle Value Lost") +
  xlab("Tackle Result") +
  ggtitle("Tackle Value Lost vs Tackle Result") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = .5),
        axis.title.y = element_text(hjust = .5),
        plot.title = element_text(face = "bold", hjust = 0.5))

tvlBoxplot

ggsave("Tackle Result vs Tackle Value Lost Boxplot.png", tvlBoxplot, dpi = 420)

### Full Play GIF ###
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
    
    # mutate(team_color = ifelse(ballCarrierId == nflId, "orange", team_color),
    #        team_color2 = ifelse(ballCarrierId == nflId, "black", team_color2)) %>%
    left_join(pull_influence) %>%
    left_join(pull_difficulty) %>%
    group_by(frameId, nflId) %>%
    mutate(s_x = s*sin(dir), s_y = s*cos(dir),
           cone_x1 = 2.5*sin(dir + pi/4), cone_y1 = 2.5*cos(dir + pi/4),
           cone_x2 = 2.5*sin(dir - pi/4), cone_y2 = 2.5*cos(dir - pi/4),
           cumulativePursuitQuality = ifelse(is.na(cumulativePursuitQuality), 0, cumulativePursuitQuality),
           frameId = frameId - 5) %>%
    mutate(team_color = case_when(
      ballCarrierId == nflId ~ "orange",
      isOffense == F ~ "#CC79A7",
      .default = "turquoise"
    ))
  
  # triangle_data = single_play %>%
  #   select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y) %>% mutate(newX = x, newY = y) %>%
  #   bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y, cone_x1, cone_y1) %>% 
  #               mutate(newX = x + cone_x1, newY = y + cone_y1) %>% select(-c(cone_x1, cone_y1))) %>%
  #   bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y, cone_x2, cone_y2) %>% 
  #               mutate(newX = x + cone_x2, newY = y + cone_y2) %>% select(-c(cone_x2, cone_y2))) %>%
  #   bind_rows(single_play %>% select(gameId, playId, frameId, nflId, percentOwnership, isOffense, x, y) %>% mutate(newX = x, newY = y)) %>%
  #   mutate(deltaX = newX - x, deltaY = newY - y)
  
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
                                                                                  group = nflId, alpha = percentOwnership), fill = "red", size = 10) +
    geom_point(data = single_play %>% filter(isOffense == F, nflId %in% ids), aes(x = x, y = y, shape = club,
                                                                                  group = nflId, fill = NA), colour = "black", size = 10) +
    geom_point(data = single_play, aes(x = x, y = y, shape = club,
                                       fill = team_color, group = nflId, size = club)) +
    geom_text(data = single_play, aes(x = x, y = y, label = jerseyNumber), colour = "white",
              vjust = 0.36, size = 3.5) +
    # annotation_custom(ggplotGrob(SOLegend),
    #                   xmin = 122.5, xmax = 130,
    #                   ymin = 33, ymax = 48) +
    # annotation_custom(ggplotGrob(pathDifficultyLegend),
    #                   xmin = 121, xmax = 130,
    #                   ymin = 6, ymax = 21) +
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
  # player1 = returnPlayerName(ids[1])
  # player2 = returnPlayerName(ids[2])
  # colors = c(player1 = "darkblue", player2 = "lightblue")
  pull_influence = calculateInfluence %>%
    # filter(gameId == 2022102301, playId == 2441,
    #        nflId %in% c(54622)) %>%
    filter(gameId == game, playId == play,
           nflId %in% ids) %>%
    select(gameId, playId, frameId, nflId, displayName, percentOwnership) %>%
    left_join(pursuitQuality %>% select(gameId, playId, frameId, nflId, displayName, opponentOwnershipPercentNextLoc,
                                        cumulativePursuitQuality)) %>%
    mutate(frameId = frameId - 5)
  
  lastFrames = pull_influence %>%
    group_by(nflId) %>%
    summarise(finalFrame = frameId[percentOwnership == max(percentOwnership)],
              maxOwnership = max(percentOwnership))
  
  influenceBeforePeak = pull_influence %>%
    left_join(lastFrames) %>%
    left_join(jerseyNumbers) %>%
    mutate(Player = paste0(jerseyNumber, ". ", displayName),
           Phase = case_when(
             frameId > finalFrame ~ "Tackle",
             .default = "Pursuit"),
           dotX = case_when(
             frameId >= finalFrame ~ finalFrame,
             .default = NA
           ),
           dotY = case_when(
             frameId >= finalFrame ~ maxOwnership,
             .default = NA
           )) %>%
    arrange(nflId)
  
  ltList = c("Pursuit" = "dashed", "Tackle" = "solid")
  
  anim = ggplot(influenceBeforePeak, aes(frameId, percentOwnership, linetype = Phase, color = Player)) +
    annotate("segment", x =0, xend = 42, y = .05, yend = .05, color = "black", linewidth = .25,
             linetype = "dotted") +
    annotate("text", x = 41.5, y = -.025, label = "NFL\nAvg", size = 1.5, fontface = "bold") +
    geom_line() +
    geom_point(aes(dotX, dotY), size=2) +
    theme_bw() +
    scale_linetype_manual(values = ltList, guide = "legend") +
    #scale_color_manual(values = c("blue", "lightblue", "purple")) + 
    scale_color_manual(values = c("blue", "lightblue")) + 
    transition_reveal(frameId) +
    xlab("Time Elapsed (ms)") +
    guides(color = FALSE) +
    theme(
      legend.position = c(.21, .75),
      legend.box.background = element_rect(colour = "black"),
      legend.key.height = unit(.1, 'in'),
      axis.title = element_text(size = rel(.8)),
      axis.text = element_text(size = rel(.8)),
      panel.grid.minor = element_blank()) +
    ylab("Space Ownership") +
    ylim(-.1, 1)
  
  
  play.length.ex <- max(pull_influence$frameId) - min(pull_influence$frameId) + 1
  animate(anim, fps = 10, nframe = play.length.ex, height = 2, width = 3, units = "in", res = 150, renderer = magick_renderer())
  
}

track_TV = function(game, play, ids){
  pull_influence = calculateInfluence %>%
    # filter(gameId == 2022091500, playId == 3350,
    #        nflId %in% c(46086, 46775)) %>%
    filter(gameId == game, playId == play,
           nflId %in% ids) %>%
    select(gameId, playId, frameId, nflId, displayName, percentOwnership) %>%
    left_join(pursuitQuality %>% select(gameId, playId, frameId, nflId, displayName, opponentOwnershipPercentNextLoc,
                                        cumulativePursuitQuality)) %>%
    mutate(frameId = frameId - 5)
  
  lastFrames = pull_influence %>%
    group_by(nflId) %>%
    summarise(finalFrame = frameId[percentOwnership == max(percentOwnership)],
              maxOwnership = max(percentOwnership))
  
  influenceBeforePeak = pull_influence %>%
    left_join(lastFrames) %>%
    left_join(jerseyNumbers) %>%
    mutate(Player = paste0(jerseyNumber, ". ", displayName),
           lt = case_when(
             frameId > finalFrame ~ "solid",
             .default = "dashed"),
           dotX = case_when(
             frameId >= finalFrame ~ finalFrame,
             .default = NA
           ),
           dotY = case_when(
             frameId >= finalFrame ~ 0,
             .default = NA
           ),
           tackleValueLost = case_when(
             frameId >= finalFrame ~ maxOwnership - percentOwnership,
             .default = NA
           )) %>%
    arrange(nflId)
  
  anim = ggplot(influenceBeforePeak, aes(frameId, tackleValueLost, color = Player, linetype = lt)) +
    annotate("segment", x =0, xend = 42, y = .028, yend = .028, color = "black", linewidth = .25,
             linetype = "dotted") +
    annotate("text", x = 41.5, y = -.047, label = "NFL\nAvg", size = 1.5, fontface = "bold") +
    geom_line() +
    geom_point(aes(dotX, dotY), size=2) +
    theme_bw() +
    scale_linetype_identity() +
    #scale_color_manual(values = c("blue", "lightblue", "purple")) + 
    scale_color_manual(values = c("blue", "lightblue")) + 
    transition_reveal(frameId) +
    xlab("Time Elapsed (ms)") +
    theme(
      legend.position = "none",
      axis.title = element_text(size = rel(.8)),
      axis.text = element_text(size = rel(.8)),
      panel.grid.minor = element_blank()) +
    ylab("Tackle Value Lost") +
    ylim(-.1, 1)
  
  
  play.length.ex <- max(pull_influence$frameId) - min(pull_influence$frameId) + 1
  animate(anim, fps = 10, nframe = play.length.ex, height = 2, width = 3, units = "in", res = 150, renderer = magick_renderer())
  
}

track_PQ = function(game, play, ids){
  pull_influence = calculateInfluence %>%
    filter(gameId == game, playId == play,
           nflId %in% ids) %>%
    # filter(gameId == 2022091500, playId == 3350,
    #        nflId %in% c(46086, 46775)) %>%
    select(gameId, playId, frameId, nflId, displayName, percentOwnership) %>%
    left_join(pursuitQuality %>% select(gameId, playId, frameId, nflId, displayName, opponentOwnershipPercentNextLoc,
                                        cumulativePursuitQuality)) %>%
    mutate(frameId = frameId - 5)
  
  lastFrames = pull_influence %>%
    group_by(nflId) %>%
    summarise(finalFrame = frameId[percentOwnership == max(percentOwnership)],
              pqPeakOwnership = cumulativePursuitQuality[frameId == finalFrame])
  
  influenceBeforePeak = pull_influence %>%
    left_join(lastFrames) %>%
    left_join(jerseyNumbers) %>%
    mutate(Player = paste0(jerseyNumber, ". ", displayName),
           cumulativePursuitQuality = case_when(
             frameId > finalFrame ~ NA,
             .default = cumulativePursuitQuality),
           dotX = case_when(
             frameId >= finalFrame ~ finalFrame,
             .default = NA
           ),
           dotY = case_when(
             frameId >= finalFrame ~ pqPeakOwnership,
             .default = NA
           )) %>%
    arrange(nflId)
  
  
  
  
  anim = ggplot(influenceBeforePeak, aes(frameId, cumulativePursuitQuality, color = Player)) +
    annotate("segment", x =0, xend = 42, y = 0.005, yend = .005, color = "black", linewidth = .25,
             linetype = "dotted") +
    annotate("text", x = 41.5, y = -.07, label = "NFL\nAvg", size = 1.5, fontface = "bold") +
    geom_line() +
    geom_line() +
    geom_point(aes(x = dotX, y = dotY), size = 2) +
    #scale_color_manual(values = c("blue", "lightblue", "purple")) + 
    scale_color_manual(values = c("blue", "lightblue")) + 
    
    #geom_text(aes(label = frameId, x = 42.5, y = .85)) +
    #geom_point(data = dotPoints, aes(frameId, cumulativePursuitQuality, color = displayName), size = 2)
    theme_bw() +
    transition_reveal(frameId) +
    xlab("Time Elapsed (ms)") +
    theme(legend.position = c(.35, .75),
          legend.box.background = element_rect(colour = "black"),
          legend.key.height = unit(.1, 'in'), axis.title = element_text(size = rel(.8)),
          axis.text = element_text(size = rel(.8)),
          panel.grid.minor = element_blank()) +
    ylab("Cumulative Pursuit Quality") +
    ylim(-.1, 1)
  
  
  
  play.length.ex <- max(pull_influence$frameId) - min(pull_influence$frameId) + 1
  animate(anim, fps = 10, nframe = play.length.ex, height = 2, width = 3, units = "in", res = 150, renderer = magick_renderer())
  
}
#blank_image = image_blank(width = 50, height = 200, color = "white")
# filter(gameId == 2022091500, playId == 3350,
#        nflId %in% c(46086, 46775)) %>%

#2022091500, 3350, c(46086, 46775)
#2022092510, 2822, c(49410, 52945)
#2022091500, 1908, c(47984, 52471)

play_gif = rotate_animate_play(2022103012, 504, c(44911, 47804))


p1 = track_PQ(2022103012, 504, c(44911, 47804))
p2 = track_SO(2022103012, 504, c(44911, 47804))
p3 = track_TV(2022103012, 504, c(44911, 47804))

plots_combined = image_append(c(p1[1], p2[1], p3[1]))

new_gif = image_append(c(play_gif[1], image_flatten(plots_combined)), stack = T)

for(i in 2:length(p2) - 1){
  if(i == length(p2) - 1){
    for(j in 1:50){
      plots_combined = image_append(c(p1[length(p2) - 1], p2[length(p2) - 1], p3[length(p2) - 1]))
      
      full_combined = image_append(c(play_gif[length(p2) - 1], image_flatten(plots_combined)), stack = T)
      new_gif = c(new_gif, full_combined)
    }
  }
  else{
    
    
    plots_combined = image_append(c(p1[i], p2[i], p3[i]))
    
    full_combined = image_append(c(play_gif[i], image_flatten(plots_combined)), stack = T)
    new_gif = c(new_gif, full_combined)
  }
}

new_gif

anim_save('Full Play.gif', new_gif)



### Tackling vs Pursuit ###
write_csv(pursuitQualityTacklesSummary, "PQ and TVL Summary Data.csv")
tackleVsPursuitFacet = ggplot(pursuitQualityTacklesSummary %>% rename("Name" = displayName, "Team" = defensiveTeam),
                              aes(pursuitComposite, tackleValueLostPerAttempt, 
                                                 label = Name, group = Team)) +
  geom_abline(intercept = seq(-.22, .22, by = .04), slope = 2, color = "lightgrey") +
  geom_vline(xintercept = mean(pursuitQualityTacklesSummary$pursuitComposite)) +
  geom_hline(yintercept = mean(pursuitQualityTacklesSummary$tackleValueLostPerAttempt)) +
  geom_point(aes(fill = team_color, color = team_color2), size = 2.5) +
  scale_color_identity() +
  scale_fill_identity() +
  xlim(-.01, .04) +
  ylim(-.01, .08) +
  annotate(geom = "text", x = .0366, y = -.005, size = 3, label = "Good Tackling\nGood Pursuit", color = "black",
           fontface = "bold") +
  annotate(geom = "text", x = .0366, y = .0775, size = 3, label = "Bad Tackling\nGood Pursuit", color = "black",
           fontface = "bold") +
  annotate(geom = "text", x = -.005, y = .0775, size = 3, label = "Bad Tackling\nBad Pursuit", color = "black",
           fontface = "bold") +
  annotate(geom = "text", x = -.005, y = -.005, size = 3, label = "Bad Tackling\nBad Pursuit", color = "black",
           fontface = "bold") +
  # geom_image(aes(image = team_logo_wikipedia), size = 0.04, by = "width", asp = 1.618) +
  # xlim(0, .1) +
  # ylim(0, .1) +
  facet_wrap(~ position) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggplotly(tackleVsPursuitFacet, tooltip = c('label', 'group'))

save_html(p, "Tackle vs Pursuit.html")

## Appendix

tackleFrequencyByPosition = ggplot(positionTacklePercentSummaryLonger, aes(value, position, fill = playType)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percentLabel, x = labelPos), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("#28E2E5", "#2297E6", "#CD0BBC")) +
  ggtitle("Tackle Frequency by Play Type and Position") +
  xlab("Tackle Frequency") +
  ylab("Position") +
  theme_bw() +
  theme(axis.title.x = element_text(hjust = .5),
        axis.title.y = element_text(hjust = .5),
        plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("Tackle Frequency by Play Type and Position.png", tackleFrequencyByPosition, dpi = 420, units = "in")



