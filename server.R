source('editable.R')
library(glue)
library(shiny)
library(bslib)
library(shinythemes)

# get teams and logos
teams <- nflfastR::teams_colors_logos %>%
  filter(!team_abbr %in% c("LAR", "SD", "STL", "OAK"))

# team abbreviations for dropdown menus
ids <- teams %>%
  pull(team_abbr)

team_ids <- c(sort(unique(as.character(ids))))

#####################################################################################
######## Define server app ###########################################
#####################################################################################

shinyServer(function(session, input, output) {
  # User input ---- 
  fullInput<- eventReactive(
    input$Update,
    {tibble(
        "type" = as.character(input$type),
        "qtr" = as.integer(input$qtr),
        "quarter_seconds_remaining" = 60 * as.integer(input$mins) + as.integer(input$secs),
        'posteam' = as.character(input$posteam),
        'defteam' = as.character(input$defteam),
        'away_team' = as.character(input$away),
        'home_team' = as.character(input$home),
        'yardline_100' = as.integer(input$yardline),
        'ydstogo' = as.integer(input$ydstogo),
        'posteam_timeouts_remaining' = as.integer(input$posteam_to),
        'defteam_timeouts_remaining' = as.integer(input$defteam_to),
        'home_opening_kickoff' = as.integer(input$home_ko),
        'score_differential' = as.integer(input$score_diff),
        #'runoff' = as.integer(input$runoff),
        'season' = as.integer(input$season))
      }, ignoreNULL = FALSE)
  
  #### FOURTH DOWN CONVERSION ####
  # Table for 4th down ----
    tableData4th <- eventReactive(
      input$Update,
      {fullInput() %>%
          add_4th_probs() %>%
          make_table_data()
      }, ignoreNULL = FALSE)
    
    # Make 4th down table ----
    output$view4th <- render_gt(
      expr = make_table_4th(tableData4th(), fullInput())
    )
    
    # Team logo ----
    output$picture_off <-
      renderText({
        c('<img width="150" src="',
          glue("{teams %>% 
        filter(team_abbr == input$posteam) %>% 
                   pull(team_logo_espn)}"),
          '">')
      })
    
    output$picture_def <-
      renderText({
        c('<img width="150" src="',
          glue("{teams %>% 
        filter(team_abbr == input$defteam) %>% 
                   pull(team_logo_espn)}"),
          '">')
      })
    
    output$picture4th <-
      renderText({
        c('<img width="100" src="',
          glue("{teams %>%
                   filter(team_abbr == input$posteam) %>%
                   pull(team_logo_espn)}"),
          '">')
      })

    # Print decisions
    output$some_text4th <- renderText({
    return(glue("<font size='+1'> The Offense Should : <br>
    <font size='+1'><span style='color:red'> <strong>
    {tableData4th() %>% 
    arrange(-choice_prob) %>% 
    slice(1) %>% 
    pull(choice)}</span> 
    (+ <span style='color:green'> <strong>
    {round(tableData4th() %>%
    arrange(-choice_prob) %>% 
    slice(1) %>%
    pull(choice_prob) - tableData4th() %>% arrange(-choice_prob) %>%
              slice(2) %>% pull(choice_prob), 1)}% WP</strong></span>)</font>"))
  })

    
    
    
    ### TWO POINT CONVERSIONS TAB ####
    # Table for 2pt conv ----
    tableData2pt <- eventReactive(
      input$Update,
      {fullInput() %>%
          add_2pt_probs() %>%
          make_2pt_table_data()
      }, ignoreNULL = FALSE)    
    
    # Make 2pt conv table ----
    output$view2pt <- render_gt(
      expr = make_table_2pt(tableData2pt(), fullInput())
    )
    
    output$picture2pt <-
      renderText({
        c('<img width="75" src="',
          glue::glue("{teams %>%
                   filter(team_abbr == input$posteam) %>%
                   pull(team_logo_espn)}"),
          '">')
      })
    
    output$some_text2pt <- renderText({
    return(glue("<font size='+2'><span style='color:red'>{tableData2pt() %>% 
    arrange(-choice_prob) %>% 
    slice(1) %>% pull(choice)}</span> (+
      <span style='color:green'> <strong> {round(tableData2pt() %>% 
                    arrange(-choice_prob) %>% 
                    slice(1) %>% 
                    pull(choice_prob) - tableData2pt() %>% 
                    arrange(-choice_prob) %>% 
                    slice(2) %>% 
                    pull(choice_prob), 1)}% WP</strong></span>)</font>"))
    })
    
    observe({
      query <- parseQueryString(session$clientData$url_search)
      query1 <- paste(names(query), query, sep = "=", collapse=", ")
      # print(query1)
      # print(substr(query1, (nchar(query1) - 4), (nchar(query1))))
      if(substr(query1, (nchar(query1) - 4), (nchar(query1))) == "2pt=1"){
        updateTabsetPanel(session, "inTabset", selected = "two_pt")
      }
    })
    
}
)
  
# Create Shiny app ----
#shinyApp(ui, server)
  