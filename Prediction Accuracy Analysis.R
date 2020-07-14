library(tidyverse)
library(DescTools)
library(reactable)

### Load Games from 2018-2019 for Analysis
games <- nflfastR::fast_scraper_schedules(2018:2019)

### Load Predictions from Personal Model (Model and Model_Dataset Created in Separate Document) 
Personal_Predictions <- Model_Dataset %>%
  filter(season >= 2018) %>%
  #mutate(adjusted_epa_home_wp = predict(newdata = Model_Dataset %>% filter(season >= 2018) %>% mutate_if(is.numeric, function(x) ((x - min(x)) / (max(x) - min(x)))), object = Goldberg_Model, type = "response")) %>%
  mutate(adjusted_epa_home_wp = caret::predict.train(newdata = Model_Dataset %>% mutate(win = as.factor(win)) %>% filter(season >= 2018), object = Goldberg_Model, type = "prob")[,2]) %>%
  select(game_id, win, adjusted_epa_home_wp) 

### Load FiveThirtyEight Predictions
FiveThirtyEight_Elo <- read_csv("https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv") %>%
  dplyr::mutate(team1 = gsub("WSH", "WAS", team1),
                team2 = gsub("WSH", "WAS", team2),
                team1 = gsub("LAR", "LA", team1),
                team2 = gsub("LAR", "LA", team2)) %>%
  dplyr::select(date, season, team1, team2, elo_prob1) %>%
  dplyr::rename(fivethirtyeight_home_wp = elo_prob1) %>%
  dplyr::filter(season >= 2018)

### Load ESPN And Vegas Predictions
library(espnNFLscraper)
espn_predictions <- espnNFLscraper::get_pregame_predictions(espn_nfl_ids %>% filter(season >= 2018 & season < 2020 & season_type > 1) %>% pull(espn_gameid))
espn_predictions %>% 
  filter(!is.na(espn_home_wp)) %>%
  select(home_team, away_team, season, week, espn_home_wp, numberfire) %>%
  mutate(week = ifelse(week == 22, week-1, week)) %>%
  rename(numberfire_home_wp = numberfire)

### Merge Predictions Together
Predictions_Dataset <- games %>%
  # Add Personal Model
  left_join(Personal_Predictions, by = "game_id", all.x = TRUE) %>% 
  # Add FiveThirtyEight
  mutate(gameday = lubridate::as_date(gameday)) %>%
  left_join(FiveThirtyEight_Elo, by = c("home_team" = "team1", "gameday" = "date", "season")) %>%
  # Add ESPN Predictions
  left_join(espn_predictions %>% 
              filter(!is.na(espn_home_wp)) %>%
              select(home_team, away_team, season, week, espn_home_wp, numberfire) %>%
              mutate(week = ifelse(week == 22, week-1, week)) %>%
              rename(numberfire_home_wp = numberfire),
            by = c("home_team", "away_team", "season", "week")) %>%
  # Add an Extremized Version of adjusted_epa
  mutate(
    extremized_adjusted_epa_home_wp = ifelse(adjusted_epa_home_wp > .5, adjusted_epa_home_wp+adjusted_epa_home_wp*.1, adjusted_epa_home_wp-adjusted_epa_home_wp*.1)
  )
    



### Ascertain Percent of Games Correct
Accuracy_Dataset <- Predictions_Dataset %>%
  # Create Prediciton and Check Accuracy
  mutate(
    correct_adjusted_epa = ifelse(ifelse(adjusted_epa_home_wp > .5, 1, 0) == win, 1, 0),
    correct_espn = ifelse(ifelse(espn_home_wp > .5, 1, 0) == win, 1, 0),
    correct_numberfire = ifelse(ifelse(numberfire_home_wp > .5, 1, 0) == win, 1, 0),
    correct_fivethirtyeight = ifelse(ifelse(fivethirtyeight_home_wp > .5, 1, 0) == win, 1, 0)
  ) %>%
  # Pivot Longer to allow group_by and summarize
  pivot_longer(
    cols = starts_with("correct_"),
    names_to = "predictor",
    names_prefix = "correct_",
    values_to = "correct"
  ) %>%
  # Group and Summarize
  group_by(predictor) %>%
  summarise(
    games = n(),
    games_correct = sum(correct),
    percent_correct = round(mean(correct),3)
  )

### Create Brier Score Data.Frame and Merge that in
Accuracy_Dataset <- Accuracy_Dataset %>%
  left_join(data.frame(
    predictor = c("espn", "fivethirtyeight", "numberfire", "adjusted_epa"),
    brier_score = c(
      DescTools::BrierScore(Predictions_Dataset$win, Predictions_Dataset$espn_home_wp),
      DescTools::BrierScore(Predictions_Dataset$win, Predictions_Dataset$fivethirtyeight_home_wp),
      DescTools::BrierScore(Predictions_Dataset$win, Predictions_Dataset$numberfire_home_wp),
      DescTools::BrierScore(Predictions_Dataset$win, Predictions_Dataset$adjusted_epa_home_wp)
      )),
    by = "predictor"
    ) %>%
  mutate(brier_score = round(brier_score, 3))

### Create a Reactable Table
Accuracy_Dataset %>%
  arrange(-percent_correct) %>%
  reactable(
    compact = TRUE,
    borderless = FALSE,
    striped = FALSE,
    fullWidth = FALSE,
    defaultColDef = colDef(
      align = "center",
      minWidth = 100),
    theme = reactableTheme(
      headerStyle = list(
        "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
        "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
        borderColor = "#555")
    ),
    columns = list(
      predictor = colDef(name = "Predictor",
                         align = "left",
                          minWidth = 110),
      games = colDef(name = "Predictions"),
      games_correct = colDef(name = "Correct Predictions"),
      percent_correct = colDef(name = "% Correct",
                               class = "border-left",
                               style = function(value) {
                                 normalized <- (value - min(Accuracy_Dataset$percent_correct)) / (max(Accuracy_Dataset$percent_correct) - min(Accuracy_Dataset$percent_correct))
                                 color <- rgb(colorRamp(c("#0089BA","#00B0A7"))(normalized), maxColorValue = 255)
                                 list(background = color)}
                               ),
      brier_score = colDef(name = "Brier Score", 
                           style = function(value) {
                             normalized <- (value - min(Accuracy_Dataset$brier_score)) / (max(Accuracy_Dataset$brier_score) - min(Accuracy_Dataset$brier_score))
                             color <- rgb(colorRamp(c("#0089BA", 	"#00B0A7"))(normalized), maxColorValue = 255)
                             list(background = color)
                             }
                           )
    )
  )

### Delve more into the differences between the predictions

# Test Average Prediction to See if Brier Score Difference are a result of generally more confident predictions
Predictions_Dataset %>%
  # Pivot Longer to allow group_by and summarize
  pivot_longer(
    cols = ends_with("_home_wp"),
    names_to = "predictor",
    values_to = "prediction"
  ) %>%
  mutate(predictor = str_remove(predictor, "_home_wp")) %>%
  # Group and Summarize
  group_by(predictor) %>%
  summarise(
    games = n(),
    average_prediction = round(mean(prediction),3)
  ) %>%
  arrange(-average_prediction) %>%
  #create reactable
  reactable(compact = TRUE,
            borderless = FALSE,
            striped = FALSE,
            fullWidth = FALSE,
            defaultColDef = colDef(
              align = "center",
              minWidth = 100),
            theme = reactableTheme(
              headerStyle = list(
                "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                borderColor = "#555")
            ),
            columns = list(
              predictor = colDef(name = "Predictor",
                                 align = "left",
                                 minWidth = 110),
              games = colDef(name = "Predictions"),
              games_excluded = colDef(name = "Games Excluded"),
              average_prediction = colDef(name = "Average Prediction")
            )
  )

# Okay, so the fivethirtyeight really bucks the trend of a pure sharpness associated with their average prediction. 
# Lets try one more time and see what happens when you filter out games where the prediction within 10% range
Predictions_Dataset %>%
  # Pivot Longer to allow group_by and summarize
  pivot_longer(
    cols = ends_with("_home_wp"),
    names_to = "predictor",
    values_to = "prediction"
  ) %>%
  mutate(predictor = str_remove(predictor, "_home_wp")) %>%
  # Filter Predictions
  filter(prediction >= .6) %>%
  # Group and Summarize
  group_by(predictor) %>%
  summarise(
    games = n(),
    games_excluded = 534 - games,
    average_prediction = round(mean(prediction),3)
  ) %>%
  arrange(-average_prediction) %>%
  #create reactable
  reactable(compact = TRUE,
            borderless = FALSE,
            striped = FALSE,
            fullWidth = FALSE,
            defaultColDef = colDef(
              align = "center",
              minWidth = 100),
            theme = reactableTheme(
              headerStyle = list(
                "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                borderColor = "#555")
            ),
            columns = list(
              predictor = colDef(name = "Predictor",
                                 align = "left",
                                 minWidth = 110),
              games = colDef(name = "Predictions", 
                             footer = "wp > .6"),
              games_excluded = colDef(name = "Games Excluded"),
              average_prediction = colDef(name = "Average Prediction")
            )
  )

Predictions_Dataset %>%
  # Pivot Longer to allow group_by and summarize
  pivot_longer(
    cols = ends_with("_home_wp"),
    names_to = "predictor",
    values_to = "prediction"
  ) %>%
  mutate(predictor = str_remove(predictor, "_home_wp")) %>%
  # Filter Predictions
  filter(prediction <= .4) %>%
  # Group and Summarize
  group_by(predictor) %>%
  summarise(
    games = n(),
    games_excluded = 534 - games,
    average_prediction = round(mean(prediction), 3)
  ) %>%
  arrange(-average_prediction) %>%
  #create reactable
  reactable(compact = TRUE,
            borderless = FALSE,
            striped = FALSE,
            fullWidth = FALSE,
            defaultColDef = colDef(
              align = "center",
              minWidth = 100),
            theme = reactableTheme(
              headerStyle = list(
                "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                borderColor = "#555")
            ),
            columns = list(
              predictor = colDef(name = "Predictor",
                                 align = "left",
                                 minWidth = 110),
              games = colDef(name = "Predictions",
                             footer = "wp < .4"),
              games_excluded = colDef(name = "Games Excluded"),
              average_prediction = colDef(name = "Average Prediction")
              )
            )

# The takeaway form these two tables, I think, can be explained by ESPN having a sharp prediction — one that is more confident — once you get out of toss-up territory.

### Now Lets Find Games Where there is a distinction
Predictions_Dataset %>%
  # Create Prediction and Check Accuracy
  mutate(
    correct_adjusted_epa = ifelse(ifelse(adjusted_epa_home_wp > .5, 1, 0) == win, 1, 0),
    correct_espn = ifelse(ifelse(espn_home_wp > .5, 1, 0) == win, 1, 0),
    correct_numberfire = ifelse(ifelse(numberfire_home_wp > .5, 1, 0) == win, 1, 0),
    correct_fivethirtyeight = ifelse(ifelse(fivethirtyeight_home_wp > .5, 1, 0) == win, 1, 0),
  ) %>% 
  # Check for Disagreement 
  mutate(
    disagreement = ifelse(correct_adjusted_epa == correct_espn & 
                            correct_adjusted_epa == correct_fivethirtyeight & 
                            correct_adjusted_epa == correct_numberfire, 0, 1)
  ) %>%
  filter(correct_adjusted_epa == 0 & correct_espn == 1 & correct_numberfire == 1 & correct_fivethirtyeight == 1) %>% view()
  

