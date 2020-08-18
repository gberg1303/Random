rb_data <- NFL_PBP %>%
  filter(rush == 1) %>%
  group_by(rusher_player_name, rusher_player_id, season) %>%
  summarise(
    rush_attempts = n(),
    rush_yards = sum(yards_gained),
    rush_touchdowns = sum(touchdown)
  )%>% 
  filter(!is.na(rusher_player_id)) %>%
  collect()

wr_data <- NFL_PBP %>%
  filter(pass_attempt == 1) %>%
  group_by(receiver_player_id, receiver_player_name, season) %>%
  summarise(
    targets = n(),
    receptions = sum(complete_pass),
    passing_yards = sum(yards_gained),
    passing_touchdowns = sum(touchdown)
  )%>% 
  filter(!is.na(receiver_player_id)) %>%
  collect()


fantasy_data <- wr_data %>%
  left_join(rb_data,
            by = c("receiver_player_id" = "rusher_player_id", "receiver_player_name" = "rusher_player_name", "season"), all.x = TRUE, all.y = TRUE) %>%
  filter(receiver_player_id != 0) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(fantasy_points = .5*receptions + .1*passing_yards + .1*rush_yards + 6*rush_touchdowns + 6*passing_touchdowns) 

summary(
  lm(
    data = fantasy_data %>%
      filter(rush_attempts >= 75) %>%
      mutate(opportunities = rush_attempts + targets) %>%
      group_by(season) %>%
      mutate(rank = rank(-fantasy_points)),
    formula = rank ~ opportunities
  )
)$r.squared

summary(
  lm(
    data = fantasy_data %>%
      filter(targets >= 65) %>%
      mutate(opportunities = rush_attempts + targets) %>%
      group_by(season) %>%
      mutate(rank = rank(-fantasy_points)),
    formula = rank ~ opportunities
  )
)$r.squared


