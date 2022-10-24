library(tidyverse)
library(gt)
library(gtExtras)
library(nfl4th)
library(glue)


# Team logos
teams <- nflfastR::teams_colors_logos %>%
  filter(!team_abbr %in% c("LAR", "SD", "STL", "OAK"))
ids <- teams %>%
  pull(team_abbr)
team_ids <- c(sort(unique(as.character(ids))))


# FOURTH DOWN FUCNTIONS ----

#table
make_table_4th <- function(df, current_situation) {
  df %>%
    arrange(-choice_prob) %>%
    table_theme() %>%
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
    )
        
}


 

# TWO POINT CONVERSION FUNCTIONS ----
# table
make_table_2pt <- function(df, current_situation) {
  
  df %>%
    arrange(-choice_prob) %>%
    table_theme() %>%
    tab_header(
        title = md(glue("**Situation:**")), 
        subtitle = md(glue("{case_when(current_situation$score_differential < 0 ~ 'Down by',
      current_situation$score_differential == 0 ~ 'Tied',
      current_situation$score_differential > 0 ~ 'Up by')}
      {ifelse(current_situation$score_differential == 0,
      'up', abs(current_situation$score_differential))} |
      Qtr: {current_situation$qtr},
      {hms::hms(current_situation$quarter_seconds_remaining) %>%
                             substr(4, 8)}"))
    )
}



## Table theme ----
table_theme <- function(df) {
  df %>%
    gt() %>%
    #gt_theme_espn() %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything())
      )
    ) %>%
    cols_label(
      choice = "",
      choice_prob = "Win Prob%",
      success_prob = "Success Prob%",
      success_wp = "Succeed",
      fail_wp = "Fail") %>%
    
    fmt_number(
      columns = c(choice_prob, success_prob, success_wp, fail_wp), decimals = 0) %>%
    
    tab_spanner(label = "Win Prob % if",
                columns = 4:5, 
    ) %>%
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
    ) %>%
    tab_source_note(source_note = "*Win probabilities calculated by nfl4th package.") %>%
    tab_options(
      heading.title.font.size = "medium",
      heading.subtitle.font.size = "medium",
      row_group.border.top.width = px(2),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      table_body.hlines.color = "white",
      table.border.top.color = "black",
      table.border.top.width = px(3),
      table.border.bottom.color = "white",
      table.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = px(2), 
      table.layout = "auto", 
      table.width = "90%"
    ) %>%
    opt_align_table_header(align = "center") %>%
    sub_missing(
      columns = everything(),
      rows = everything(), 
      missing_text = "-") %>%
    
    gt_highlight_rows(
      rows = 1,     
      bold_target_only = TRUE,
      target_col = 1) 
  
}
