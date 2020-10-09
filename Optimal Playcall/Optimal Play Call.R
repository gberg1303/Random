# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(nflfastR)
library(readr)
library(nflfastR)
library(DBI)
library(RSQLite)
library(DT)
library(tidyverse)
library(future)

games <- purrr::map_df(2010:2020, function(x) {
    readr::read_csv(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.csv.gz")
    )
  })

# Define UI
ui <- fluidPage(#theme = shinytheme("lumen"),
                titlePanel("Find Optimal Playcalls"),
                  sidebarPanel(
                    
                    # Select Game ID
                    selectizeInput(inputId = "game_id", label = strong("NFL Game ID"),
                                   choices = unique(games %>% pull(game_id))),
                    
                    # Select Season
                    selectInput(inputId = "season", label = strong("Season"),
                                choices = unique(games %>% pull(season))),
                    
                    # Select Home Team
                    selectInput(inputId = "home_team", label = strong("Home Team"),
                                choices = unique(games %>% pull(home_team))),
                    
                    # Select Team with the Ball
                    selectInput(inputId = "posteam", label = strong("Posession Team"),
                                choices = unique(games %>% pull(posteam))),
                    
                    # Select Defensive Team
                    selectInput(inputId = "defteam", label = strong("Defensive Team"),
                                choices = unique(games %>% pull(posteam))),
                    
                    # Select Roof Type
                    selectInput(inputId = "roof", label = strong("Roof Type"),
                                choices = unique(games %>% pull(roof))),
                    
                    # Select Team with the Ball
                    numericInput(inputId = "half_seconds_remaining", label = strong("Second Remaining in the Half"), value = 1800),
                    
                    # Select Yards from Goaline
                    numericInput(inputId = "yardline_100", label = strong("How Many Yards from the Goaline Are You?"), value = 80),
                    
                    # Select Down
                    numericInput(inputId = "down", label = strong("Down"),
                                value = 4),
                    
                    # Select ydstogo
                    numericInput(inputId = "ydstogo", label = strong("Yards for First Down"), value = 10),
                    
                    # Select Posession Team Timeouts
                    numericInput(inputId = "posteam_timeouts_remaining", label = strong("Posession Team Timeouts Remaining"),
                                value = 3),
                    
                    # Select Defense Team Timeouts
                    numericInput(inputId = "defteam_timeouts_remaining", label = strong("Defensive Team Timeouts Remaining"),
                                value = 3),
                    
                    # Select receive
                    selectInput(inputId = "receive_2h_ko", label = strong("Will this team receive the second half kickoff?"),
                                choices = c(1, 0)),
                    
                    # Select Season
                    numericInput(inputId = "score_differential", label = strong("What is the team's current Score Differential?"), value = 1),
                    
                    # Select Season
                    numericInput(inputId = "spread_line", label = strong("What was the Vegas Pregame Spread?"), value = 1)
                    ),

        
                ### Main Panel
                mainPanel(
                  textOutput("situation"),
                  textOutput("input"),
                  textOutput("conversion"),
                  textOutput("fg"),
                  textOutput("punt"),
                  textOutput("conversion_fail"),
                  dataTableOutput("similar_plays")
                )
                ### Close Page
                )

### Server
server <- function(input, output) {
 
  input_play <- reactive({ tibble(
    play_id = "input",
    season = input$season,
    home_team = input$home_team,
    posteam = input$posteam,
    roof = input$roof,
    half_seconds_remaining = input$half_seconds_remaining,
    yardline_100 = input$yardline_100,
    down = input$down,
    ydstogo = input$ydstogo,
    posteam_timeouts_remaining = input$posteam_timeouts_remaining,
    defteam_timeouts_remaining = input$defteam_timeouts_remaining,
    receive_2h_ko = input$receive_2h_ko,
    score_differential = input$score_differential,
    spread_line = as.integer(input$spread_line),
    posteam_spread = as.integer(spread_line),
  ) %>%
      bind_rows( 
        tibble(
          play_id = "conversion",
          season = input$season,
          home_team = input$home_team,
          posteam = input$posteam,
          roof = input$roof,
          half_seconds_remaining = input$half_seconds_remaining - 5,
          yardline_100 = input$yardline_100 - input$ydstogo,
          down = 1,
          ydstogo = 10,
          posteam_timeouts_remaining = input$posteam_timeouts_remaining,
          defteam_timeouts_remaining = input$defteam_timeouts_remaining,
          receive_2h_ko = input$receive_2h_ko,
          score_differential = input$score_differential,
          spread_line = as.integer(input$spread_line),
          posteam_spread = as.integer(spread_line),
        ),
        tibble(
          play_id = "punt",
          season = input$season,
          home_team = input$home_team,
          posteam = input$defteam,
          roof = input$roof,
          half_seconds_remaining = input$half_seconds_remaining - 5,
          yardline_100 = 80,
          down = 1,
          ydstogo = 10,
          posteam_timeouts_remaining = input$posteam_timeouts_remaining,
          defteam_timeouts_remaining = input$defteam_timeouts_remaining,
          receive_2h_ko = input$receive_2h_ko,
          score_differential = input$score_differential,
          spread_line = as.integer(input$spread_line),
          posteam_spread = as.integer(spread_line),
        ),
        tibble(
          play_id = "conversion_fail",
          season = input$season,
          home_team = input$home_team,
          posteam = input$defteam,
          roof = input$roof,
          half_seconds_remaining = input$half_seconds_remaining - 5,
          yardline_100 = 100 - input$yardline_100,
          down = 1,
          ydstogo = 10,
          posteam_timeouts_remaining = input$posteam_timeouts_remaining,
          defteam_timeouts_remaining = input$defteam_timeouts_remaining,
          receive_2h_ko = input$receive_2h_ko,
          score_differential = input$score_differential,
          spread_line = as.integer(input$spread_line),
          posteam_spread = as.integer(spread_line),
        )
      ) %>%
      mutate(spread_line = as.integer(spread_line),
             posteam_spread = as.integer(posteam_spread)) %>%
      nflfastR::calculate_expected_points()
  })
  
  ### Output Plays
  output$situation <- renderText({
    paste(input$posteam ,input$down, "and", input$ydstogo, "on the", ifelse(input$yardline_100 <= 50, input$defteam, input$posteam),
          ifelse(input$yardline_100 <= 50, input$yardline_100, 100-input$yardline_100), "with", lubridate::seconds_to_period(input$half_seconds_remaining), "remaining in the half.")
  })
  
  output$input <- renderText({
    input_ep <- input_play() %>% filter(play_id == "input") %>% pull(ep)
    paste("Expected Points at the current situation is", round(input_ep, 2))
  })
  
  output$conversion <- renderText({
    conversion_epa <- input_play() %>% filter(play_id == "conversion") %>% pull(ep) - input_play() %>% filter(play_id == "input") %>% pull(ep)
    conversion_epa <- ifelse(input$ydstogo == input$yardline_100, 6.9-input_play() %>% filter(play_id == "input") %>% pull(ep), conversion_epa)
    conversion_rate <- games %>% filter(posteam == input$posteam, down == input$down, ydstogo >= input$ydstogo - 1 & ydstogo <= input$ydstogo + 1, qb_dropback == 1 | rush == 1) %>% pull(first_down) %>% mean(na.rm = TRUE)
    paste("Expected Points Added from Conversion is", round(conversion_epa, 2), "*", round(conversion_rate, 2), "for an Expected EPA of", round(conversion_epa, 2)*round(conversion_rate, 2))
  })
  
  output$punt <- renderText({
    punt_epa <- 0 - input_play() %>% filter(play_id == "input") %>% pull(ep) - input_play() %>% filter(play_id == "punt") %>% pull(ep)
    paste("Expected Points Added from Punting is", round(punt_epa, 2))
  })
  
  output$fg <- renderText({
    fg_epa <- 3 - input_play() %>% filter(play_id == "input") %>% pull(ep) 
    fg_conversion_rate <- games %>% filter(field_goal_attempt == 1, yardline_100 == input$yardline_100) %>% mutate(field_goal_result = ifelse(field_goal_result == "made", 1, 0)) %>% pull(field_goal_result) %>% mean(na.rm = TRUE)
    #fg_conversion_rate <- ifelse(is.na(fg_conversion_rate) == TRUE, 0, fg_conversion_rate)
    paste("Expected Points Added from Field Goal is", round(fg_epa, 2),"*", round(fg_conversion_rate, 2), "for an Expected EPA of", round(fg_epa, 2)*round(fg_conversion_rate, 2))
  })
  
  output$conversion_fail <- renderText({
    conversion_fail_epa <- input_play() %>% filter(play_id == "conversion_fail") %>% pull(ep) - input_play() %>% filter(play_id == "input") %>% pull(ep)
    conversion_rate <- games %>% filter(posteam == input$posteam, down == input$down, ydstogo >= input$ydstogo - 1 & ydstogo <= input$ydstogo + 1, qb_dropback == 1 | rush == 1) %>% pull(first_down) %>% mean(na.rm = TRUE)
    paste("Opponent Expected Points Added from failed to convert is", round(conversion_fail_epa, 2), "*", round(1-conversion_rate, 2), "for an Expected EPA of", round(conversion_fail_epa, 2)*round(1-conversion_rate, 2))
  })
  
  output$similar_plays <- renderDataTable({
    games %>% filter(game_id == input$game_id)
  })
  
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)

