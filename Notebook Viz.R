library(gganimate)
library(transformr)
library(magick)
library(kableExtra)
library(DT)
library(webshot2)
library(htmltools)
library(gt)
library(gtExtras)
library(gridExtra)
library(png)
library(patchwork)
library(cowplot)
library(gtsummary)
library(plotly)
library(ggimage)



### SO + Path Difficulty GIF ###
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

anim_save('Full Play Metric Description.gif', start)

### Pass vs Rush PQ by Position Scatterplot ###
ggplot(pursuitQualitySummary, aes(Run, Pass, fill = position, color = position)) +
  geom_point() +
  geom_label(data = positionMidpoints, aes(color = position, label = position), fill = "white") +
  theme_bw() +
  xlab("Run PQ") +
  ylab("Pass PQ") +
  ggtitle("Pass vs Run PQ by Position") +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = .5),
        axis.title.y = element_text(hjust = .5),
        plot.title = element_text(face = "bold", hjust = 0.5))

### Top 5 PQ by Position Table ###

pursuitCompositePosTable = function(pos){
  data = pursuitComposite %>%
    ungroup() %>%
    filter(position == pos) %>%
    arrange(-pursuitComposite) %>%
    select(Name = displayName, Team = team_logo_wikipedia, 'Pursuit Score' = pursuitComposite, Snaps = snapCount,
           "Pursuit Opportunities" = pursuitOpps) %>%
    mutate(`Pursuit Score` = round(`Pursuit Score`, 3)) %>%
    head(5)
  
  titleString = paste0("**Top 5 ", pos, "**")
  
  data %>%
    gt() %>%
    tab_header(title = md(titleString)) %>%
    data_color(
      columns = vars('Pursuit Score'),
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
      table.font.size = px(14),
      heading.title.font.size = px(24)
    ) %>%
    cols_align(align = 'center',
               columns = everything()) %>%
    gt_img_rows(Team) %>%
    cols_width(Name ~ px(175),
               `Pursuit Score` ~ px(50))
}



table1 = pursuitCompositePosTable('CB')
table2 = pursuitCompositePosTable('DE')
table3 = pursuitCompositePosTable('OLB')
table4 = pursuitCompositePosTable("S")
table5 = pursuitCompositePosTable("IDL")
table6 = pursuitCompositePosTable("ILB")

combined_html <- tagList(
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

save_html(combined_html, "combined_tables.html")


### Tackling Table ###
tackleValuePosTable = function(pos){
  data = pursuitQualityTacklesSummary %>%
    ungroup() %>%
    filter(position == pos) %>%
    arrange(tackleValuePerAttempt) %>%
    select(Name = displayName, Team = team_logo_wikipedia, 'Tackle Score' = tackleValuePerAttempt, Snaps = snapCount,
           Tackles = tackles) %>%
    mutate(`Tackle Score` = round(`Tackle Score`, 3)) %>%
    head(5)
  
  titleString = paste0("**Top 5 ", pos, "**")
  
  data %>%
    gt() %>%
    tab_header(title = md(titleString)) %>%
    data_color(
      columns = vars('Tackle Score'),
      colors = scales::col_numeric(
        palette = "Blues",
        #palette = c('#d0e6ea', '#c6e0e5', '#bddbe1', '#b3d6dd', '#aad1d8', '#a0ccd4', '#90b8bf', '#80a3aa', '#708f94', '#607a7f'),
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
      table.font.size = px(14),
      heading.title.font.size = px(24)
    ) %>%
    cols_align(align = 'center',
               columns = everything()) %>%
    gt_img_rows(Team) %>%
    cols_width(Name ~ px(175),
               `Tackle Score` ~ px(50))
}



table1 = tackleValuePosTable('CB')
table2 = tackleValuePosTable('DE')
table3 = tackleValuePosTable('OLB')
table4 = tackleValuePosTable("S")
table5 = tackleValuePosTable("IDL")
table6 = tackleValuePosTable("ILB")

combined_html <- tagList(
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

save_html(combined_html, "combined_tables.html")

### Tackle Event Boxplot ###
ggplot(tacklePlaySummary, aes(tackleValue, tackleResult, fill = tackleResult)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none")

### Tackling vs Pursuit ###
tackleVsPursuitFacet = ggplot(pursuitQualityTacklesSummary, aes(pursuitComposite, tackleValuePerAttempt, 
                                                 label = displayName, group = defensiveTeam)) +
  geom_point(aes(fill = team_color, color = team_color2), size = 2.5) +
  scale_color_identity() +
  scale_fill_identity() +
  # geom_image(aes(image = team_logo_wikipedia), size = 0.04, by = "width", asp = 1.618) +
  geom_vline(xintercept = mean(pursuitQualityTacklesSummary$pursuitComposite)) +
  geom_hline(yintercept = mean(pursuitQualityTacklesSummary$tackleValuePerAttempt)) +
  # xlim(0, .1) +
  # ylim(0, .1) +
  facet_wrap(~ position) +
  theme_bw() +
  theme(legend.position = "none")

p = ggplotly(tackleVsPursuitFacet, tooltip = c('label', 'group'))

