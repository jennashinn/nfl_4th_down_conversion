library(tidyverse)
library(gt)
library(gtExtras)
library(nfl4th)

# get teams and logos
teams <- nflfastR::teams_colors_logos %>%
  filter(!team_abbr %in% c("LAR", "SD", "STL", "OAK"))

# team abbreviations for dropdown menus
ids <- teams %>%
  pull(team_abbr)

team_ids <- c(sort(unique(as.character(ids))))


# FOURTH DOWN FUCNTIONS ----

#table
make_table_4th <- function(df, current_situation) {
  df %>%
    arrange(-choice_prob) %>%
    gt() %>%
    gt_theme_538() %>%
    cols_label(
      choice = "",
      choice_prob = "Win Prob%",
      success_prob = "Success Prob%",
      success_wp = "Succeed",
      fail_wp = "Fail") %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything())
      )
    ) %>%
    tab_options(
      heading.title.font.size = "medium",
      heading.subtitle.font.size = "medium",
      row_group.border.top.width = px(3),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      table_body.hlines.color = "white",
      table.border.top.color = "black",
      table.border.top.width = px(1),
      table.border.bottom.color = "white",
      table.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = px(2)
    ) %>%
    fmt_number(
      columns = c(choice_prob, success_prob, success_wp, fail_wp), decimals = 0
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "lightcyan"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(choice_prob)
      )
    )  %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(choice)
      )
    )  %>%
    tab_spanner(label = "Prob of Win % if",
                columns = 4:5) %>%
    cols_align(
      columns = 2:5,
      align = "center"
    ) %>%
    tab_footnote(
      footnote = "Expected win % for a given decision",
      locations = cells_column_labels(2)
    ) %>%
    tab_footnote(
      footnote = "Likelihood of success for a given decision",
      locations = cells_column_labels(3)
    )  %>%
    tab_header(
      title = md(glue("**Situation:**")), 
      subtitle = md(glue("{case_when(current_situation$score_differential < 0 ~ 'Down by',
      current_situation$score_differential == 0 ~ 'Tied',
      current_situation$score_differential > 0 ~ 'Up by')}
      {ifelse(current_situation$score_differential == 0,
      'up', abs(current_situation$score_differential))} |
      4th & {current_situation$ydstogo} |
                         {current_situation$yardline_100} yards from opponent end zone|
                         Qtr: {current_situation$qtr},
                         {hms::hms(current_situation$quarter_seconds_remaining) %>%
                             substr(4, 8)}"))
    ) %>%
    opt_align_table_header(align = "center") %>%
    sub_missing(
      columns = everything(),
      rows = everything(), 
      missing_text = "-"
    )
        
}


 

# TWO POINT CONVERSION FUNCTIONS ----
# table
make_table_2pt <- function(df, current_situation) {
  
  df %>%
    arrange(-choice_prob) %>%
    gt() %>%
    cols_label(
      choice = "",
      choice_prob = "Win %",
      success_prob = "Success %",
      success_wp = "Succeed",
      fail_wp = "Fail")
    # ) %>%
    # tab_style(
    #   style = cell_text(color = "black", weight = "bold"),
    #   locations = list(
    #     cells_row_groups(),
    #     cells_column_labels(everything())
    #   )
    # ) %>%
    # tab_options(
    #   row_group.border.top.width = px(3),
    #   row_group.border.top.color = "black",
    #   row_group.border.bottom.color = "black",
    #   table_body.hlines.color = "white",
    #   table.border.top.color = "black",
    #   table.border.top.width = px(1),
    #   table.border.bottom.color = "white",
    #   table.border.bottom.width = px(1),
    #   column_labels.border.bottom.color = "black",
    #   column_labels.border.bottom.width = px(2)
    # ) %>%
    # fmt_number(
    #   columns = c(choice_prob, success_prob, success_wp, fail_wp), decimals = 0
    # ) %>%
    # tab_style(
    #   style = list(
    #     cell_text(color = "red", weight = "bold")
    #   ),
    #   locations = cells_body(
    #     columns = c(choice_prob)
    #   )
    # )  %>%
    # tab_style(
    #   style = list(
    #     cell_text(weight = "bold")
    #   ),
    #   locations = cells_body(
    #     columns = c(choice)
    #   )
    # )  %>%
    # tab_spanner(label = "Win % if",
    #             columns = 4:5) %>%
    # cols_align(
    #   columns = 2:5,
    #   align = "center"
    # ) %>%
    # tab_footnote(
    #   footnote = "Expected win % for a given decision",
    #   locations = cells_column_labels(2)
    # ) %>%
    # tab_footnote(
    #   footnote = "Likelihood of success for a given decision",
    #   locations = cells_column_labels(3)
    # )  %>%
    # tab_header(
    #   title = md(glue::glue("{case_when(current_situation$score_differential < 0 ~ 'Down', 
    #                         current_situation$score_differential == 0 ~ 'Tied', 
    #                         current_situation$score_differential > 0 ~ 'Up')} 
    #                         {ifelse(current_situation$score_differential == 0, 'up', 
    #                         abs(current_situation$score_differential))}, 
    #                         Qtr {current_situation$qtr}, 
    #                         {hms::hms(current_situation$quarter_seconds_remaining) %>% 
    #                         substr(4, 8)}"))
    # )
  
}