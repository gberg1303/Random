touchdown_passes <- NFL_PBP %>%
  filter(pass_attempt == 1, season >= 2010) %>%
  group_by(posteam, game_id) %>%
  summarise(touchdown_passes = sum(touchdown),
            yards_gained = sum(yards_gained)) %>%
  collect()

games <- nflfastR::fast_scraper_schedules(2010:2019) %>%
  mutate(team = home_team)

dset <- games %>%
  rbind(games %>% mutate(team = away_team)) %>%
  left_join(
    touchdown_passes,
    by = c("team" = "posteam", "game_id")
  ) %>%
  filter(home_score != away_score) %>%
  mutate(win = ifelse(team == home_team & home_score > away_score | team==away_team & home_score < away_score, 1, 0))


sjPlot::tab_model(glm(data = dset, win ~ touchdown_passes, family = "binomial"))
cor(dset$win, dset$touchdown_passes)

dset %>% 
  mutate(cases = case_when(
    yards_gained <= 100 ~ "Less Than 100",
    yards_gained >= 100 & yards_gained < 150 ~ "100-150",
    yards_gained >= 150 & yards_gained < 200 ~ "150-200",
    yards_gained >= 200 & yards_gained < 250 ~ "200-250",
    yards_gained >= 250 & yards_gained < 300 ~ "250-300",
    yards_gained >= 300 & yards_gained < 350 ~ "300-350",
    yards_gained >= 350 & yards_gained < 400 ~ "350-400",
    yards_gained >= 400 ~ "Greater than 400",
  )) %>%
  group_by(cases) %>%
  summarise(frequency = n(), 
            win_average = mean(win)) %>%
  mutate(
    win_prob_model = predict(object = glm(data = dset, win ~ yards_gained, family = "binomial"), newdata = ., type = "response")
  )
