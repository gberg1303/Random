### NFL PBP Data
library(tidyverse)
library(imputeTS)
library(nflfastR)
library(janitor)
library(caret)
library(DBI)
library(RSQLite)


### Load Teamcolors
nflfastR::teams_colors_logos

### Load NFL Data
update_db(
  dbdir = "/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Raw NFL Seasons Data",
  dbname = "pbp_db",
  tblname = "nflfastR_pbp",
  force_rebuild = FALSE
)
NFL_PBP <- tbl(dbConnect(SQLite(), "/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Raw NFL Seasons Data/pbp_db"),
               "nflfastR_pbp")

############################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################
# Team Performance Metrics
############################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################



### Get EPA difference per game
epa_difference <- NFL_PBP %>%
  filter(!is.na(epa), !is.na(ep), !is.na(posteam), play_type=="pass" | play_type=="run", season >= 2007) %>%
  group_by(game_id, season, week, posteam, home_team) %>%
  summarise(
    off_epa= mean(epa),
  ) %>%
  left_join(NFL_PBP %>%
          filter(!is.na(epa), !is.na(ep), !is.na(posteam), play_type=="pass" | play_type=="run", season >= 2007) %>%
          group_by(game_id, season, week, defteam, away_team) %>% 
          summarise(def_epa=mean(epa)),
        by = c("game_id", "posteam" = "defteam", "season", "week"),
        all.x = TRUE) %>%
  mutate(opponent = ifelse(posteam == home_team, away_team, home_team)) %>%
  select(game_id, season, week, home_team, away_team, posteam, opponent, off_epa, def_epa) %>%
  collect()
# Merge Back Opponent 
epa_difference <- epa_difference %>%
  left_join(epa_difference %>%
              select(-opponent) %>%
              rename(
                opp_off_epa = off_epa,
                opp_def_epa = def_epa
              ) %>%
              group_by(posteam) %>%
              arrange(season, week) %>%
              mutate(
                opp_def_epa = pracma::movavg(opp_def_epa, n = 10, type = "s"),
                opp_def_epa = lag(opp_def_epa),
                opp_off_epa = pracma::movavg(opp_off_epa, n = 10, type = "s"),
                opp_off_epa = lag(opp_off_epa),
              ), by = c("game_id", "season", "week", "home_team", "away_team", "opponent" = "posteam"),
            all.x = TRUE)
# Merge Back League Mean
epa_difference <- epa_difference %>%
  left_join(epa_difference %>%
              filter(posteam == home_team) %>%
              group_by(season, week) %>%
              summarise(
                league_mean = mean(off_epa + def_epa)
              ) %>%
              ungroup() %>%
              group_by(season) %>%
              mutate(
                league_mean = lag(cummean(league_mean))
              ),
            by = c("season", "week"),
            all.x = TRUE) 
#Adjust EPA
epa_difference <- epa_difference %>%
  mutate(
    off_adjustment_factor = ifelse(!is.na(league_mean), league_mean-opp_def_epa, 0),
    def_adjustment_factor = ifelse(!is.na(league_mean), league_mean-opp_off_epa, 0),
    adjusted_off_epa = off_epa + off_adjustment_factor,
    adjusted_def_epa = def_epa + def_adjustment_factor,
  )
# Group and Get Moving Average
epa_difference <- epa_difference %>%
  group_by(posteam) %>%
  arrange(season, week) %>%
  mutate(
    def_epa = pracma::movavg(def_epa, n = 10, type = "s"),
    def_epa = lag(def_epa),
    off_epa = pracma::movavg(off_epa, n = 10, type = "s"),
    off_epa = lag(off_epa),
    adjusted_off_epa = lag(pracma::movavg(adjusted_off_epa, n = 10, type = "s")),
    adjusted_def_epa = lag(pracma::movavg(adjusted_def_epa, n = 10, type = "s"))
  ) %>%
  ungroup() %>%
  select(-home_team, -away_team) %>%
  select(game_id, posteam, season, week, off_epa, def_epa, adjusted_off_epa, adjusted_def_epa)


############################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################
# Team Box Metrics
############################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################

### Get Schedule and Game Outcomes
NFL_Outcomes_Weekly <- NFL_PBP %>%
  filter(season_type == "REG" | season_type == "POST") %>%
  group_by(season, week, game_date, game_id, home_team, away_team, game_date) %>%
  summarise(home_score = max(total_home_score),
            away_score = max(total_away_score)) %>%
  mutate(winner = ifelse(home_score > away_score, home_team, away_team),
         loser = ifelse(home_score > away_score, away_team, home_team)) %>%
  mutate(team = home_team,
         opponent = away_team) %>% 
  mutate(win = ifelse(team == winner, 1, 0)) %>%
  collect() %>%
  bind_rows(x = .,
            y = NFL_PBP %>%
              filter(season_type == "REG" | season_type == "POST") %>%
              group_by(season, week, game_date, game_id, home_team, away_team, game_date) %>%
              summarise(home_score = max(total_home_score),
                        away_score = max(total_away_score)) %>%
              mutate(winner = ifelse(home_score > away_score, home_team, away_team),
                     loser = ifelse(home_score > away_score, away_team, home_team)) %>%
              mutate(team = away_team,
                     opponent = home_team) %>%
              mutate(win = ifelse(team == winner, 1, 0)) %>%
              collect()) %>%
  mutate(point_differential = ifelse(team == home_team, home_score-away_score, away_score-home_score),
         points_for = ifelse(team == home_team, home_score, away_score),
         points_against = ifelse(team == home_team, away_score, home_score)) %>%
  ungroup()
# Merge Back Opponent
NFL_Outcomes_Weekly <- NFL_Outcomes_Weekly %>%
  left_join(
    NFL_Outcomes_Weekly %>% 
      select(game_id, team, opponent, points_for, points_against) %>%
      rename(
        opp_points_for = points_for,
        opp_points_against = points_against
      ),
    by = c("game_id", "team" = "opponent", "opponent" = "team"),
    all.x = TRUE
  )
# Merge Back Mean
NFL_Outcomes_Weekly <- NFL_Outcomes_Weekly %>%
  left_join(NFL_Outcomes_Weekly %>%
              filter(team == home_team) %>%
              group_by(season, week) %>%
              summarise(
                league_mean = mean(points_for + points_against)
              ) %>%
              ungroup() %>%
              group_by(season) %>%
              mutate(
                league_mean = lag(cummean(league_mean))
              ),
            by = c("season", "week"),
            all.x = TRUE) 
#Adjust Points For
NFL_Outcomes_Weekly <- NFL_Outcomes_Weekly %>%
  mutate(
    off_adjustment_factor = ifelse(!is.na(league_mean), league_mean-opp_points_for, 0),
    def_adjustment_factor = ifelse(!is.na(league_mean), league_mean-opp_points_against, 0),
    adjusted_points_for = points_for + off_adjustment_factor,
    adjusted_points_against = points_against + def_adjustment_factor,
    adjusted_point_differential = adjusted_points_for - adjusted_points_against
  )
#Group and Create the Lagged Moving Average
NFL_Outcomes_Weekly <- NFL_Outcomes_Weekly %>%  
  group_by(team) %>% 
  arrange(season, week) %>%
  mutate(
    pythag_wins = lag(adjusted_points_for^2.37/(adjusted_points_for^2.37+adjusted_points_against^2.37)),
    points_for = lag(pracma::movavg(points_for, n = 10, type = "s")),
    points_against = lag(pracma::movavg(points_against, n = 10, type = "s")),
    point_differential = lag(pracma::movavg(point_differential, n = 10, type = "s")),
    adjusted_points_for = lag(pracma::movavg(adjusted_points_for, n = 10, type = "s")),
    adjusted_points_against = lag(pracma::movavg(adjusted_points_against, n = 10, type = "s")),
    adjusted_point_differential = lag(pracma::movavg(adjusted_point_differential, n = 10, type = "s")),
  ) %>%
  ungroup()

############################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################
# Merge
############################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################

# Merge All Together
Model_Dataset <- NFL_Outcomes_Weekly %>%
  # Add Opponent Box Score Statistics
  merge(y = NFL_Outcomes_Weekly %>%
          select(game_id, team, point_differential, adjusted_point_differential, pythag_wins) %>%
          dplyr::rename(
            opp_point_differential = point_differential,
            opp_adjusted_point_differential = adjusted_point_differential,
            opp_pythag_wins = pythag_wins
            ),
        by.x = c("opponent", "game_id"),
        by.y = c("team", "game_id")) %>%
  # Add EPA Statistics
  left_join(epa_difference, by = c("game_id", "season", "week",  "home_team" = "posteam")) %>%
  left_join(epa_difference %>% 
               rename(
                 opp_off_epa = off_epa,
                 opp_def_epa = def_epa,
                 opp_adjusted_off_epa = adjusted_off_epa,
                 opp_adjusted_def_epa = adjusted_def_epa),
             by = c("game_id", "season", "week", "away_team" = "posteam")) %>%
  filter(home_team == team) %>%
  # Add Home Margin and Playoff Indication
  mutate(home_margin = home_score - away_score
         ) %>%
  select(-opponent, -team, -winner, -loser) %>%
  # Merge out CPOEless statistics
  filter(season > 2006)



############################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################
# Regression
############################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################


# Run a linear regression to get coefficients
summary(lm(data = Model_Dataset, home_margin ~ 
             point_differential +
             adjusted_off_epa +
             adjusted_def_epa))

summary(lm(data = Model_Dataset, home_margin ~ 
             opp_point_differential +
             opp_adjusted_off_epa +
             opp_adjusted_def_epa))

# Add Overall Metric
Model_Dataset <- Model_Dataset %>%
 # mutate(Metric = point_differential*0.50303 + opp_point_differential*-0.29129 + off_epa.x*0.14905 + off_epa.y *-0.31409 + 2.35970)
  mutate(Metric = 0.38475*point_differential + 13.48065*adjusted_off_epa + -9.84522*adjusted_def_epa + 2.34270,
         Opp_Metric = 0.38475*opp_point_differential + 13.48065*opp_adjusted_off_epa + -9.84522*opp_adjusted_def_epa + 2.34270,
         #Opp_Metric = -0.19513*opp_point_differential + -21.88442*opp_adjusted_off_epa + 8.09290*opp_adjusted_def_epa + 2.18263
         ) %>%
  filter(!is.na(Metric))
# Add Playoff Favorite Indicator
Model_Dataset <- Model_Dataset %>%
 mutate(
   playoff = as.factor(ifelse(week > 17, 1, 0))
  ) 

summary(lm(data = Model_Dataset, home_margin ~ 
             Metric + Opp_Metric + playoff + playoff*Metric))

sjPlot::tab_model(glm(data = Model_Dataset, win ~ 
                        Metric + Opp_Metric + playoff + playoff*Metric))
set.seed(123)
NFL_Predict <- train(win ~
                       Metric + Opp_Metric + playoff + playoff*Metric,
                      data = Model_Dataset %>% mutate(win = as.factor(win)),
                      method = 'glm',
                      family = "binomial",
                      preProc = c("scale"),
                      trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))
print(NFL_Predict)

# Get Baseline predictions to compare
#mean(Model_Dataset %>% mutate(prediction = ifelse(fivethirtyeight_home_wp > .5, 1, 0), correct = ifelse(win == prediction, 1, 0)) %>% pull(correct))
mean(Model_Dataset %>% mutate(prediction = ifelse(Metric > Opp_Metric, 1, 0), correct = ifelse(win == prediction, 1, 0)) %>% pull(correct))


# Testing Model
set.seed(123)
Goldberg_Model <- train(win ~
                       Metric + Opp_Metric + playoff + playoff*Metric,
                     data = Model_Dataset %>% mutate(win = as.factor(win)) %>% filter(season < 2018),
                     method = 'glm',
                     family = "binomial",
                     preProc = c("scale"),
                     trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))
print(Goldberg_Model)
