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


# Code from https://github.com/husarah/big-data-bowl-2022/blob/master/scripts/viz.R
tacklingCompositePosTable = function(position){
  data = playerTacklingComposite %>%
    ungroup() %>%
    filter(Position == position) %>%
    arrange(-tacklingComposite) %>%
    select(Name, Team = team_logo_wikipedia, 'Tackling Composite' = tacklingComposite, Tackles = totalTackles, Snaps = snapCount) %>%
    mutate(`Tackling Composite` = round(`Tackling Composite`, 3)) %>%
  head(5)
  
  titleString = paste0("**Top 5 ", position, "**")
  
  data %>%
    gt() %>%
    tab_header(title = md(titleString)) %>%
    data_color(
      columns = vars('Tackling Composite'),
      colors = scales::col_numeric(
        palette = c('#d0e6ea', '#c6e0e5', '#bddbe1', '#b3d6dd', '#aad1d8', '#a0ccd4', '#90b8bf', '#80a3aa', '#708f94', '#607a7f'),
        domain = c(.02, .082)
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
    gt_img_rows(Team)
  }



table1 = tacklingCompositePosTable('CB')
table2 = tacklingCompositePosTable('DE')
table3 = tacklingCompositePosTable('OLB')
table4 = tacklingCompositePosTable("S")
table5 = tacklingCompositePosTable("IDL")
table6 = tacklingCompositePosTable("ILB")

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

#webshot(combined_html, "test_html.png")


save_html(combined_html, "combined_tables.html")

# olbTable %>% gtsave("olbTable.png")
# ilbTable %>% gtsave('ilbTable.png')
# sTable %>% gtsave('sTable.png')
# 
# # Create a grid layout with three columns
# grid_layout <- matrix(c(1, 2, 3), ncol = 3, byrow = TRUE)
# 
# image_files = c("olbTable.png", "ilbTable.png", "sTable.png")






