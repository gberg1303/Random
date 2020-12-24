library(cfbscrapR)
library(tidyverse)
library(nflfastR)
library(gt)
library(rtweet)
library(teamcolors)

token <- create_token(
  app = "go.on.fourth",
  consumer_key = "iOULkU2dTnf7kMXKmT6itr5DZ",
  consumer_secret = "L7ZSY7LjTmbZGDXfxhkilboeApfiJzv97nPaPSB0bQGO6rB90o",
  access_token  = "1314984606505078786-OLAP2RVGdan6MHbPmLeiPVUcIJNE2g",
  access_secret = "13aRTaqrxM2SgTp1kAnA5wZw8rt0s15ZdMieWaHEDMA2K")


###################### TABLE
old_data <- as_tibble()
create_tables <- function(plays, table_type) {
  all <- plays %>% group_by(posteam, home_team) %>% summarize(
    epa = mean(epa, na.rm = TRUE), success=mean(success, na.rm = TRUE), p=mean(pass), play=n()) %>%
    mutate(rowname="All plays", type=1)
  
  early <- plays %>% filter(down == 1 | down ==2) %>% group_by(posteam, home_team) %>% summarize(
    epa = mean(epa, na.rm = TRUE), success=mean(success, na.rm = TRUE), p=mean(pass),play=n())%>%
    mutate(rowname="Early downs (1st & 2nd)", type=4)
  
  earlyr <- plays %>% filter((down == 1 | down ==2) & rush==1) %>% group_by(posteam, home_team) %>% summarize(
    epa = mean(epa, na.rm = TRUE), success=mean(success, na.rm = TRUE), p=mean(pass),play=n())%>%
    mutate(rowname="Early rush", type=5)
  
  earlyp <- plays %>% filter((down == 1 | down ==2) & pass==1) %>% group_by(posteam, home_team) %>% summarize(
    epa = mean(epa, na.rm = TRUE), success=mean(success, na.rm = TRUE), p=mean(pass),play=n())%>%
    mutate(rowname="Early pass", type=6)
  
  late <- plays %>% filter(down==3  | down == 4) %>% group_by(posteam, home_team) %>% summarize(
    epa = mean(epa, na.rm = TRUE), success=mean(success, na.rm = TRUE), p=mean(pass), play=n())%>%
    mutate(rowname="Late downs (3rd & 4th)", type=7)
  
  type <- plays %>% group_by(posteam, pass, home_team) %>% summarize(
    epa = mean(epa, na.rm = TRUE), success=mean(success, na.rm = TRUE), p=mean(pass), play=n()) %>%
    mutate(rowname=if_else(pass==1,"Pass","Rush"), type=2)
  
  bound <- bind_rows(all,early,earlyr, earlyp,late,type) %>%
    mutate(
      #home=ifelse(posteam==home_team,1,0), p=round(100*p), 
      epa=round(epa, digits=2), 
      success=round(success,digits=2)) %>%
    arrange(home_team,type) %>% select(-pass, -type, -home_team)
  
  #team summary table
  table <- bound %>%  select(posteam, rowname, epa, success, play) %>% group_by(posteam) %>% gt() %>%
    cols_label(
      epa = md("**EPA/<br>play**"), success = md("**Success<br>rate**"), play = md("**Plays**")) %>%
    cols_align(align = "center") %>%
    tab_source_note(
      source_note = "@gberg1303 | Table: @benbbaldwin | Data: ESPN") %>%
    tab_header(title = paste("Game Summary,", pbp[["boxscore"]][["teams"]][["team.abbreviation"]][1], "@", pbp[["boxscore"]][["teams"]][["team.abbreviation"]][2])) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")), locations = cells_group(groups=TRUE)) %>%
    tab_style(
      style = list(
        cell_text(style = "italic", align="center")), 
      locations = cells_stub(rows=c(3,4,5,6,9,10,11,12))) %>%
    gtsave("/Users/jonathangoldberg/Google Drive/Random/Sports/Data Twitter Account/College Football/Live NU/team_summary.png")
  
  ##### do stuff for player summary table
  
  rushers <- plays %>% dplyr::filter(rush==1) %>% group_by(Rusher,posteam,logo)%>%
    summarize(
      tot_epa = sum(epa), epa = mean(epa, na.rm = TRUE),  success=mean(success, na.rm = TRUE), play=n())%>%
    mutate(rowname="Rush attempts", type=1, p="Rush attempts", rowname=Rusher) %>%ungroup()
  
  receivers <- plays %>% filter(rush==0 & !is.na(Receiver)) %>% group_by(Receiver,posteam,logo)%>%
    summarize(
      tot_epa = sum(epa),  epa = mean(epa, na.rm = TRUE), success=mean(success, na.rm = TRUE), play=n())%>%
    mutate(rowname="Targets", type=2, p="Targets", rowname=Receiver)%>%ungroup()
  
  passers <- plays %>% filter(rush==0 & !is.na(Passer)) %>% group_by(Passer,posteam,logo)%>%
    summarize(
      tot_epa = sum(epa),  epa = mean(epa, na.rm = TRUE), success=mean(success, na.rm = TRUE), play=n())%>%
    mutate(rowname="Dropbacks", type=0, p="Dropbacks", rowname=Passer)%>%ungroup()
  
  
  rp <- bind_rows(passers,rushers,receivers) %>%
    mutate(epa=round(epa, digits=2), tot_epa=round(tot_epa,digits=1),success=round(success,digits=2)) %>%
    arrange(type,desc(play)) %>% select(-type, -Rusher, -Receiver, -Passer)
  
  #player summary as one big table
  t2 <- rp %>% select(logo,rowname,epa,tot_epa,success,play,p) %>% group_by(p) %>%gt() %>%
    cols_label(
      logo = md("**Team**"), success = md("**Success<br>rate**"), play = md("**Plays**"), epa=md("**EPA/<br>play**"),tot_epa=md("**Total<br>EPA**")) %>%
    text_transform(
      locations = cells_body(vars(logo)),
      fn = function(x) web_image(url = x, height = 20)
    ) %>%
    cols_align(align = "center") %>%
    tab_source_note(
      source_note = "@gberg1303 | Table: @benbbaldwin | Data: ESPN") %>%
    tab_header(title = paste("Game Summary,", pbp[["boxscore"]][["teams"]][["team.abbreviation"]][1], "@", pbp[["boxscore"]][["teams"]][["team.abbreviation"]][2])) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")), locations = cells_group(groups=TRUE)) %>%
    gtsave("/Users/jonathangoldberg/Google Drive/Random/Sports/Data Twitter Account/College Football/Live NU/player_summary.png")
  
  
  
  
  if(table_type == "player"){
    return(t2)
    message("done")}
  if(table_type == "team"){
    return(table)
    message("done")}
  
}


################ Get Live Data

#game_id <- cfbscrapR::cfb_metrics_wp_pregame(2020) %>% filter(home_team == "Northwestern" | away_team == "Northwestern") %>% filter(week == max(week)) %>% pull(game_id)

twitter_bot <- function(z){
  
    # Get Game ID
    game_id <- ifelse(!is_empty(cfbscrapR::cfb_game_media(2020, team = "Northwestern") %>% 
                                  mutate(date = lubridate::date(start_time),
                                         start = lubridate::hour(lubridate::ymd_hms(start_time, tz = "EST")),
                                         now = ifelse(Sys.Date() == date & lubridate::hour(Sys.time()) >= start & lubridate::hour(Sys.time()) <= start+5, 1, 0)) %>%
                                  filter(now == 1) %>%
                                  pull(game_id)), 
                      cfbscrapR::cfb_game_media(2020, team = "Northwestern") %>% 
                        mutate(date = lubridate::date(start_time),
                               start = lubridate::hour(lubridate::ymd_hms(start_time, tz = "EST")),
                               now = ifelse(Sys.Date() == date & lubridate::hour(Sys.time()) >= start & lubridate::hour(Sys.time()) <= start+5, 1, 0)) %>%
                        filter(now == 1) %>%
                        pull(game_id),
                      NA)
    
    
    pbp <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event={game_id}")) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE) 
    message("JSON Done")
    
    ### Pull Raw Plays from the pbp
    #Previous Only
    if(is.null(pbp[["drives"]][["current"]]) & !is.null(pbp[["drives"]][["previous"]])){plays_raw <- pbp[["drives"]][["previous"]] %>%
      dplyr::select(team.name, plays, id) %>%
      rename(drive = id) %>%
      unnest(plays, names_repair = "unique") %>%
      janitor::clean_names()
    message("Plays Done")
    }
    # Previous and Current
    if(!is.null(pbp[["drives"]][["current"]]) & !is.null(pbp[["drives"]][["previous"]])){plays_raw <- pbp[["drives"]][["previous"]] %>%
      dplyr::select(team.name, plays, id) %>%
      rename(drive = id) %>%
      unnest(plays, names_repair = "unique") %>%
      janitor::clean_names() 
    message("Plays Done")
    }
    if(is.null(pbp[["drives"]][["current"]]) & is.null(pbp[["drives"]][["previous"]])){plays_raw <- as_tibble()}
    
    if(nrow(plays_raw) > 0){plays <- plays_raw %>%
      mutate(
        home_team = pbp[["boxscore"]][["teams"]][["team.name"]][2],
        away_team = pbp[["boxscore"]][["teams"]][["team.name"]][1],
        home_timeout = ifelse(type_text == "Timeout" & str_detect(text, pattern = toupper(pbp[["boxscore"]][["teams"]][["team.name"]][2])) == TRUE, 1, 0),
        away_timeout = ifelse(type_text == "Timeout" & str_detect(text, pattern = toupper(pbp[["boxscore"]][["teams"]][["team.name"]][2])) == FALSE, 1, 0),
        roof = "outdoors",
        posteam = team_name, 
        defteam = ifelse(home_team == posteam, away_team, home_team),
        game_id = game_id,
        receive_first_half = pbp[["drives"]][["previous"]][["team.name"]][[1]],
        receive_2h_ko = ifelse(posteam == receive_first_half, 0, 1),
        score_differential = ifelse(posteam == home_team, home_score-away_score, away_score-home_score),
        game_seconds_remaining = ifelse(period_number > 2, lubridate::period_to_seconds(lubridate::ms(clock_display_value))+ifelse(period_number == 1| period_number == 3, 900, 0), 1800 + lubridate::period_to_seconds(lubridate::ms(clock_display_value))+ifelse(period_number == 1| period_number == 3, 900, 0)),
        half_seconds_remaining = lubridate::period_to_seconds(lubridate::ms(clock_display_value))+ifelse(period_number == 1| period_number == 3, 900, 0),
        season = 2020,
        timeout = ifelse(type_text == "Timeout", 1, 0),
        posteam_timeouts_remaining = 3,
        defteam_timeouts_remaining = 3,
      ) %>%
      dplyr::rename(
        period = period_number,
        yardline_100 = end_yards_to_endzone,
        down = end_down, 
        ydstogo = end_distance
      ) %>%
      mutate(
        pass = if_else(type_text == "Pass Reception" | type_text == "Passing Touchdown" |
                         type_text == "Sack" | type_text == "Pass Interception Return" |
                         type_text == "Pass Incompletion" | type_text == "Sack Touchdown" |
                         (type_text == "Safety" & str_detect(text, "sacked")) |
                         (type_text == "Fumble Recovery (Own)" & str_detect(text, "pass")) |
                         (type_text == "Fumble Recovery (Opponent)" & str_detect(text, "pass")), 1, 0),
        rush = ifelse(type_text == "Rush" | type_text == "Rushing Touchdown" | (type_text == "Safety" & str_detect(text, "run")) |
                        (type_text == "Fumble Recovery (Opponent)" & str_detect(text, "run")) | 
                        (type_text == "Fumble Recovery (Own)" & str_detect(text, "run")), 1, 0),
        Reception = ifelse(type_text == "Pass Reception" | type_text == "Passing Touchdown", 1, 0),
        Touchdown = ifelse(type_text == "Passing Touchdown" | type_text == "Rushing Touchdown", 1, 0),
        Pass_Attempt = ifelse(type_text == "Pass Reception" | type_text == "Pass Incompletion" | type_text == "Passing Touchdown" |
                                type_text == "Pass Interception Return", 1, 0),
        Completion = ifelse(type_text == "Pass Reception" | type_text == "Passing Touchdown", 1, 0),
        text = gsub("Team ", "Team QB ", text),
        text = gsub("N/A ", "N/ A ", text),
        text = gsub(" III", "", text),
        text = gsub(" II", "", text),
        text = gsub(" Jr.", "", text),
        text = gsub(" Sr.", "", text),
        text = gsub(" IV", "", text),
        text = gsub(" V ", "", text),
        Passer = NA,
        Receiver = NA,
        Rusher = NA,
        Passer = ifelse(pass == 1, 
                        gsub(",","", word(text, 1,2, sep=" ")), 
                        Passer),
        Passer = ifelse(pass == 1 & type_text == "Passing Touchdown" & str_detect(text, "from"), 
                        gsub(",","", word(text, 7,8, sep=" ")), 
                        Passer),
        Receiver = ifelse(pass == 1 & type_text == "Passing Touchdown", 
                          gsub(",","", word(text, 1,2, sep=" ")), 
                          Receiver),
        Receiver = ifelse(pass == 1 & str_detect(text, "complete") | pass == 1 & str_detect(text, "incomplete"), 
                          gsub(",","", word(text, 6,7, sep=" ")),
                          Receiver),
        Rusher = ifelse(rush == 1, 
                        gsub(",","", word(text, 1,2, sep=" ")), 
                        Rusher),
        Receiver = ifelse(str_detect(text, "incomplete") == TRUE & str_detect(text, fixed('penalty', ignore_case=TRUE)) == TRUE, NA, Receiver)) %>%
      nflfastR::calculate_expected_points() %>%
      mutate(epa = case_when(
        scoring_type_abbreviation == "TD" ~ 6.89-ep,
        is.na(scoring_type_abbreviation) & type_text != "Punt" & type_text != "End of Half" ~ lead(ep)-ep,
        type_text == "Punt" ~ 0-ep,
        scoring_type_abbreviation == "FG" ~ 3-ep,
        type_text == "End of Half" & scoring_type_abbreviation != "FG" | scoring_type_abbreviation != "TD"~ 0
      ),
      success = ifelse(epa > 0, 1, 0)) %>%
      ### Add WP
      left_join(
        pbp[["winprobability"]] %>% as_tibble(), 
        by = c("id" = "playId")
      ) %>%
      mutate(
        homeWinPercentage = lag(homeWinPercentage),
        home_wpa = round(lead(homeWinPercentage)-homeWinPercentage, 6),
        wp = ifelse(posteam == home_team, homeWinPercentage, 1-homeWinPercentage),
        
      ) %>%
      ## Add Colors
      left_join(
        teamcolors::teamcolors %>% filter(league == "ncaa", location == pbp[["boxscore"]][["teams"]][["team.location"]][1] | location == pbp[["boxscore"]][["teams"]][["team.location"]][2]) %>% select(mascot, logo),
        by = c("posteam" = "mascot")
      )
    
    
    ### Load Tables
    team_stats <- create_tables(plays = plays, table_type = "team")
    player_stats <- create_tables(plays = plays, table_type = "player")
    message("Graphs Done")
    
    ### Tweets
    tweet_material <- plays %>% mutate(old = ifelse(id %in% old_data$id, 1, 0)) %>%
      filter(old == 0 & 
               old == 0 & type_text == "End Period" | 
               old == 0 & type_text == "End of Half" | 
               old == 0 & type_text == "End of Game" |
               old == 0 &  home_wpa >= .04 | old == 0 & home_wpa <= -.04) %>%
      mutate(text = ifelse(str_detect(text, "PENALTY") == TRUE, gsub(".*PENALTY ","",text), text), 
             text = ifelse(str_detect(text, "fumbled") == TRUE, gsub(".*fumbled,","Fumble",text), text),
             home_wpa = ifelse(is.na(home_wpa), 0, home_wpa))
    
    
    if(nrow(tweet_material) > 0){
      
      
      purrr::map(1:nrow(tweet_material), function(x){
        
        # Tweet Quarter Recap
        if(tweet_material$type_text[x] == "End Period" | tweet_material$type_text[x] == "End of Half" | tweet_material$type_text[x] == "End of Game"){
          rtweet::post_tweet(
            status = paste(paste0("At the end of the ", tweet_material$period[x], case_when(tweet_material$period[x] == 1 ~ "st, ", 
                                                                                            tweet_material$period[x] == 2 ~ "nd, ",
                                                                                            tweet_material$period[x] == 3 ~ "rd, ",
                                                                                            tweet_material$period[x] == 4 ~ "th, "), "Northwestern is"), paste0(
                                                                                              ifelse("Wildcats" == tweet_material$home_team[x], 
                                                                                                     ifelse(
                                                                                                       tweet_material$home_score[x] == tweet_material$away_score[x], 
                                                                                                       paste("tied", tweet_material$home_score[x], "to", tweet_material$away_score[x]), 
                                                                                                       ifelse(
                                                                                                         tweet_material$home_score[x] > tweet_material$away_score[x],
                                                                                                         paste("up", tweet_material$home_score[x], "to", tweet_material$away_score[x]),
                                                                                                         paste("down", tweet_material$home_score[x], "to", tweet_material$away_score[x])
                                                                                                       )
                                                                                                     )
                                                                                                     ,
                                                                                                     ifelse(
                                                                                                       tweet_material$home_score[x] == tweet_material$away_score[x], 
                                                                                                       paste("tied", tweet_material$away_score[x], "to", tweet_material$home_score[x]), 
                                                                                                       ifelse(
                                                                                                         tweet_material$home_score[x] < tweet_material$away_score[x],
                                                                                                         paste("up", tweet_material$away_score[x], "to", tweet_material$home_score[x]),
                                                                                                         paste("down", tweet_material$away_score[x], "to", tweet_material$home_score[x]))
                                                                                                     ))), case_when(tweet_material$type_text[x] != "End of Game" ~ "| See the performance so far:", tweet_material$type_text[x] == "End of Game" ~ "| See the final stats:")),
            media = c("/Users/jonathangoldberg/Google Drive/Random/Sports/Data Twitter Account/College Football/Live NU/player_summary.png", "/Users/jonathangoldberg/Google Drive/Random/Sports/Data Twitter Account/College Football/Live NU/team_summary.png"))}
        
        # Key Play
        if(tweet_material$home_wpa[x] >= .04 | tweet_material$home_wpa[x] <= -.04){
          rtweet::post_tweet(
            status = paste(paste0("Key Play: ", tweet_material$home_wpa[x]*100, "% win probability shift ", ifelse(tweet_material$home_wpa[x] > 0, "for the", "against the"), " ",
                                  tweet_material$home_team[x],". ", tweet_material$text[x], ".", 
                                  paste(paste0(" Northwestern is"), paste0(
                                    ifelse("Wildcats" == tweet_material$home_team[x], 
                                           ifelse(
                                             tweet_material$home_score[x] == tweet_material$away_score[x], 
                                             paste("Tied", tweet_material$home_score[x], "to", tweet_material$away_score[x]), 
                                             ifelse(
                                               tweet_material$home_score[x] > tweet_material$away_score[x],
                                               paste("Up", tweet_material$home_score[x], "to", tweet_material$away_score[x]),
                                               paste("Down", tweet_material$home_score[x], "to", tweet_material$away_score[x])
                                             )
                                           )
                                           ,
                                           ifelse(
                                             tweet_material$home_score[x] == tweet_material$away_score[x], 
                                             paste("Tied", tweet_material$away_score[x], "to", tweet_material$home_score[x]), 
                                             ifelse(
                                               tweet_material$home_score[x] < tweet_material$away_score[x],
                                               paste("Up", tweet_material$away_score[x], "to", tweet_material$home_score[x]),
                                               paste("Down", tweet_material$away_score[x], "to", tweet_material$home_score[x]))
                                           ))), "with", tweet_material$clock_display_value[x], "left in the", 
                                    paste0(tweet_material$period[x], 
                                           case_when(tweet_material$period[x] == 1 ~ "st.", 
                                                     tweet_material$period[x] == 2 ~ "nd.",
                                                     tweet_material$period[x] == 3 ~ "rd.",
                                                     tweet_material$period[x] == 4 ~ "th."))) ))
          )}
        
        
      })
    }
    
    ### Set Old Plays
    old_data <- plays
    message("Plays Stored")
    }
    ###
    
    
  
}
