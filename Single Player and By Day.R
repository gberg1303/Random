games <- nflfastR::fast_scraper_schedules(2008:2020) %>% mutate(weekday = lubridate::wday(as.Date(gameday), label = TRUE)) %>% select(game_id, weekday)
NFL_PBP %>% 
  filter(!is.na(epa), qb_dropback == 1, receiver_player_name == "J.Jones", posteam == "ATL") %>% 
  collect() %>%
  left_join(games, by = c("game_id" = "game_id")) %>%
  filter(weekday == "Mon") %>%
  group_by(receiver_player_name) %>% 
  summarise(games = n_distinct(game_id), 
            yards = sum(yards_gained), 
            touchdowns = sum(touchdown), 
            targets = sum(pass_attempt), 
            receptions = sum(complete_pass)) %>% 
  mutate(yards = yards/games, 
         touchdowns = touchdowns/games, 
         targets = targets/games, 
         receptions = receptions/games,
         Fantasy_Points = yards*.1 + receptions*.5 + touchdowns*6)
