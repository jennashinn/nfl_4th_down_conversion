source('functions.R')
library(shiny)
library(shinythemes)
library(bslib)
library(shinyWidgets)

shinyUI(
  fluidPage(
    theme = shinytheme('flatly'),
    
    # App title ----
    titlePanel("Conversion Calculator"),
    tags$hr(),
    
    #### USER INPUTS ----
    dropdownButton(
      fluidRow(
        
        ## GAME INPUTS ----
        column(3, 
               strong(h4("Game Details:")),
               fluidRow(
                 column(6,
                        selectInput(inputId =  "season", label = "Season:", choices = 2014:2022, selected = 2021)),
                 column(6,
                        radioButtons("type", label = "Game Type:", choices = list("Regular" = "reg", "Postseason" = "post"),
                                     selected = "reg"))),
               
               fluidRow(
                 column(6, 
                        selectInput(inputId = "posteam", label= "Offense:", choices = team_ids, selected = "PIT")),
                 column(6,
                        selectInput(inputId = "defteam", label = "Defense:", choices = team_ids, selected = "BAL"))),
               
               fluidRow(
                 column(6,
                        selectInput(inputId = "away", label = "Away team:", choices = team_ids, selected = "PIT")),
                 column(6,
                        selectInput(inputId = "home", label = "Home team:", choices = team_ids, selected = "BAL"))),
               
               radioButtons(inputId= "home_ko", label = "Did home team get opening kickoff?:",
                          choices = list("0" = 0, "1" = 1),
                          inline = T,
                          selected = "0", 
                          width = "100%")),
        
        ## TIME INPUTS ----
        column(3, offset= 1,  
               strong(h4("Time Details:")),
               radioButtons("qtr",label = "Quarter:", choices = 1:4, inline = T, selected = 4,width = "100%"),
               
               fluidRow(
                 column(6, 
                        selectInput(inputId =  "mins", label = "Minutes:", choices = 0:15, selected = 0)),
                 column(6,
                        selectInput(inputId =  "secs", label = "Seconds:", choices = 0:59, selected = 44))), 
               
               fluidRow(
                 column(6,
                        radioButtons("posteam_to",label = "Offensive timeouts:", choices = 0:3, inline = T, selected = 1)),
                 column(6,
                        radioButtons("defteam_to", label = "Defensive timeouts:", choices = 0:3, inline = T, selected = 2)))
               ),
        
        ## SITUATIONAL INPUTS ----
        column(4, offset =1, 
               strong(h4("Situtional Details:")), 
               sliderInput(inputId =  "ydstogo", label = "Yards to go:", min = 1, max = 30, value = 1, width = "100%"), 
               sliderInput(inputId =  "yardline",label = "Yards from opponent End Zone:",min = 1, max = 90, value = 56, width = "100%"),
               sliderInput(inputId =  "score_diff", label = "Score differential:", min = -28, max = 28, value = 0, width = "100%")
               )
        ),
      
      tags$br(),
      
      ## UPDATE INPUTS ----
      fluidRow(
        column(12, align = "center",
               actionButton("Update", "Update", 
                            type="button", 
                            status = "primary")),
        tags$br()
        ), 
      
      label = "Click to adjust inputs", 
      circle = FALSE, 
      width = "1000px", 
      status = "primary"),
    
    tags$br(),
    
    #### RESULTS ####
    tabsetPanel(type = 'tabs', id ='inTabset',
      ### FOURTH DOWN CONVERSION ####
      tabPanel("4th Down Conversion",
               tags$br(),
        fluidRow(
          column(12, align = "center",
                 htmlOutput("picture4th"), 
                 htmlOutput("some_text4th"))
        ),
        
        tags$br(),
        fluidRow(
          column(12, align="center",
                 gt_output(outputId = "view4th") %>%
                   shinycssloaders::withSpinner(type = 6, color = "#414141", color.background = "#FFFFFF"))
          )
        ),
      
      #### TWO POINT CONVERSION####
      tabPanel("2-Point Conversion", value = "two_pt",
               tags$br(), 
               fluidRow(
                 column(12, align = "center",
                   htmlOutput("picture2pt"),
                   htmlOutput("some_text2pt"))
                 ),
               
               fluidRow(
                column(12, align="center",
                   gt_output(outputId = "view2pt") %>%
                     shinycssloaders::withSpinner(type = 6, color = "#414141", color.background = "#FFFFFF"))
               )
      )
    )
  )
)