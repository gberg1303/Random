NFL_PBP %>%
  filter(season == 2020) %>%
  collect() %>%
  mutate(time_of_poss = lubridate::period_to_seconds(lubridate::ms(drive_time_of_possession))) %>%
  group_by(game_id, drive, posteam) %>%
  summarise(
    plays = n_distinct(play_id),
    time_of_poss = max(time_of_poss)
  ) %>%
  group_by(game_id, posteam) %>%
  summarise(
    time_of_poss = sum(time_of_poss, na.rm = TRUE),
    plays = sum(plays)
  )
