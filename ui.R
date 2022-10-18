source('editable.R')
library(shiny)
library(shinythemes)
library(bslib)
library(glue)




#####################################################################################
######## Define UI for viewer app ###########################################
#####################################################################################
shinyUI(
  fluidPage(
    #theme = bs_theme(bootswatch = "lux"),
    theme = shinytheme('flatly'),
    # App title ----
    titlePanel("Conversion Calculator"),
    #navbarPage("To Go or Not to Go")
      tabPanel(
          "Conversions",
          #### USER INPUTS ----
          fluidRow(
            column(12, 
              fluidRow(
                ## GAME INPUTS ----
                column(4, strong(h4("Game Details:")),
                  fluidRow(
                    column(6,
                           selectInput(inputId =  "season", label = "Season:", choices = 2014:2022, selected = 2020)),
                    column(6,
                           radioButtons("type", label = "Game Type:", choices = list("Regular" = "reg", "Postseason" = "post"),
                          selected = "reg"))
                    ),
                  fluidRow(
                    column(6,
                           selectInput(inputId = "posteam", label= "Offense:", choices = team_ids, selected = "TB")),
                    column(6,
                           selectInput(inputId = "defteam", label = "Defense:", choices = team_ids, selected = "CHI"))
                    ),
                  fluidRow(
                    column(6,
                           selectInput(inputId = "away", label = "Away team:", choices = team_ids, selected = "TB")),
                    column(6,
                           selectInput(inputId = "home", label = "Home team:", choices = team_ids, selected = "CHI"))
                    ),
                  radioButtons(inputId= "home_ko", label = "Did home team get opening kickoff?:",
                          choices = list("0" = 0, "1" = 1),
                          inline = T,
                          selected = "0", 
                          width = "100%")),
                
                ## TIME INPUTS ----
                column(4, strong(h4("Time Details:")),
                  radioButtons("qtr",label = "Quarter:", choices = 1:4, 
                               inline = T, 
                               selected = 4, 
                               width = "100%"),
                  fluidRow(
                    column(6,
                           selectInput(inputId =  "mins", label = "Minutes:", choices = 0:15, selected = 4)),
                    column(6,
                           selectInput(inputId =  "secs", label = "Seconds:", choices = 0:59, selected = 52))
                    ),
                  
                  fluidRow(
                    column(6,
                           radioButtons("posteam_to",label = "Offensive timeouts:", choices = 0:3,
                                    inline = T,
                                    selected = 3)),
                    column(6,
                           radioButtons("defteam_to", label = "Defensive timeouts:", choices = 0:3,
                                    inline = T,
                                    selected = 3))
                    )
                ),
                
                ## SITUATIONAL INPUTS ----
                column(4, strong(h4("Situtional Details:")), 
                       sliderInput(inputId =  "ydstogo", label = "Yards to go:", min = 1, max = 30,
                           value = 1, 
                           width = "100%"),
                       sliderInput(inputId =  "yardline",label = "Yards from opponent End Zone:",min = 1, max = 90,
                           value = 7, 
                           width = "100%"),
                       sliderInput(inputId =  "score_diff", label = "Score differential:", min = -28, max = 28,
                           value = -1, 
                           width = "100%"),
                       # sliderInput(inputId =  "runoff", label = "Additional seconds to run off clock after successful conversion:",
                       #     min = 0, max = 40,
                       #     value = 0,
                       #     width = "100%"))
                )
              )
          )
          ),
          
          tags$br(),
          tags$hr(),
          
          fluidRow(
            column(12, align = "center",
                 actionButton("Update", "Update", 
                              type="button", class="btn btn-dark")),
            tags$br(),
            tags$br()
            ),
          
          tags$hr(),
          
          #### RESULTS ####

          
          #### RESULT TABS ####
          tabsetPanel(type = 'tabs', id ='inTabset', 
            #### FOURTH DOWN CONVERSION ####
            tabPanel(
              "4th Down", 
              fluidRow(
                column(12, align = "center",
                       tags$br(),
                       tags$br(),
                       htmlOutput("picture4th"), 
                       htmlOutput("some_text4th"))
                ),
              fluidRow(
                column(12, align="center",
                       gt_output(outputId = "view4th") %>%
                         shinycssloaders::withSpinner(
                              type = 6,
                              color = "#414141",
                              color.background = "#FFFFFF"))
              )
              ),
            #### TWO POINT CONVERSION####
            tabPanel("2pt Conv", value = "two_pt",
              fluidRow(
                column(12, align = "center",
                   tags$br(),
                   tags$br(),
                   htmlOutput("picture2pt"), 
                   htmlOutput("some_text2pt"))
              ),
              
              fluidRow(
                column(12, align="center",
                   gt_output(outputId = "view2pt") %>%
                     shinycssloaders::withSpinner(
                       type = 6,
                       color = "#414141",
                       color.background = "#FFFFFF"))
              )
            )
          )
      )
    )
)
