source('functions.R')
library(shiny)
library(bslib)
library(shinythemes)
library(shinyWidgets)


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
        'season' = as.integer(input$season))
      }, ignoreNULL = FALSE)
  
  #### FOURTH DOWN CONVERSION ####
  
  # Update inputs ----
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
    
    # Offensive logo ----
    output$picture4th <-
      renderText({
        c('<img width="100" src="',
          glue("{teams %>%
                   filter(team_abbr == input$posteam) %>%
                   pull(team_logo_espn)}"),
          '">')
      })

    # Show decisions
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
    # Update info ----
    tableData2pt <- eventReactive(
      input$Update,
      {fullInput() %>%
          add_2pt_probs() %>%
          make_2pt_table_data()
      }, ignoreNULL = FALSE)    
    
    # Make 2pt table ----
    output$view2pt <- render_gt(
      expr = make_table_2pt(tableData2pt(), fullInput())
    )
    
    # Offensive logo ----
    output$picture2pt <-
      renderText({
        c('<img width="100" src="',
          glue::glue("{teams %>%
                   filter(team_abbr == input$posteam) %>%
                   pull(team_logo_espn)}"),
          '">')
      })
    
    # Show decision ----
    output$some_text2pt <- renderText({
      return(glue("<font size='+1'> The Offense Should : <br>
      <font size='+1'><span style='color:red'> <strong>
    {tableData2pt() %>% 
    arrange(-choice_prob) %>% 
    slice(1) %>% 
    pull(choice)}</span> 
    (+ <span style='color:green'> <strong>
    {round(tableData2pt() %>%
    arrange(-choice_prob) %>% 
    slice(1) %>%
    pull(choice_prob) - tableData2pt() %>% arrange(-choice_prob) %>%
              slice(2) %>% pull(choice_prob), 1)}% WP</strong></span>)</font>"))
    })
    
    observe({
      query <- parseQueryString(session$clientData$url_search)
      query1 <- paste(names(query), query, sep = "=", collapse=", ")
      if(substr(query1, (nchar(query1) - 4), (nchar(query1))) == "2pt=1"){
        updateTabsetPanel(session, "inTabset", selected = "two_pt")
      }
    })
    
})
  

  