library(tidyverse)

### Load Data
game_predictions <- purrr::map_df(01:17, function(x){
  readr::read_csv(glue::glue("/Users/jonathangoldberg/Google Drive/Random/Sports/Data Twitter Account/Football/Game Predictions/2020-2021/Week {ifelse(x < 10, paste0(0, x), x)} Game Predictions.csv")) %>%
    mutate(model_home_wp = as.numeric(model_home_wp),
           espn_home_wp = as.numeric(espn_home_wp),
           Caesars = as.numeric(Caesars),
           numberfire = as.numeric(numberfire))
})

schedule <- nflfastR::fast_scraper_schedules(2020) %>%
  mutate(
    win = ifelse(home_score > away_score, 1, 0)
  )

game_predictions <- game_predictions %>%
  left_join(schedule %>% select(home_team, away_team, week, season, win),
            by = c("home_team", "away_team", "week", "season"))
### Mutate for Accuracy

game_predictions %>%
  # Create Prediciton and Check Accuracy
  mutate(
    correct_adjusted_epa = ifelse(is.na(model_home_wp), NA, ifelse(ifelse(model_home_wp > .5, 1, 0) == win, 1, 0)),
    correct_espn = ifelse(is.na(espn_home_wp), NA, ifelse(ifelse(espn_home_wp > .5, 1, 0) == win, 1, 0)),
    correct_numberfire = ifelse(is.na(numberfire), NA, ifelse(ifelse(numberfire > .5, 1, 0) == win, 1, 0)),
    correct_fivethirtyeight = ifelse(is.na(fivethirtyeight_home_wp), NA, ifelse(ifelse(fivethirtyeight_home_wp > .5, 1, 0) == win, 1, 0))
  ) %>%
  # Pivot Longer to allow group_by and summarize
  pivot_longer(
    cols = starts_with("correct_"),
    names_to = "predictor",
    names_prefix = "correct_",
    values_to = "correct"
  ) %>%
  filter(!is.na(correct)) %>%
  # Group and Summarize
  group_by(predictor) %>%
  summarise(
    games = n(),
    games_correct = sum(correct, na.rm = TRUE),
    percent_correct = round(mean(correct, na.rm = TRUE),3)
  )
