library(DBI)
library(RSQLite)
library(nflfastR)
library(tidyverse)

### Load NFL Data
update_db(
  dbdir = "/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Raw NFL Seasons Data",
  dbname = "pbp_db",
  tblname = "nflfastR_pbp",
  force_rebuild = FALSE
)
NFL_PBP <- tbl(dbConnect(SQLite(), "/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Raw NFL Seasons Data/pbp_db"),
               "nflfastR_pbp")

### Get EPA per game
epa_data <- NFL_PBP %>%
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
epa_data <- epa_data %>%
  left_join(epa_data %>%
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
epa_data <- epa_data %>%
  left_join(epa_data %>%
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
epa_data <- epa_data %>%
  mutate(
    off_adjustment_factor = ifelse(!is.na(league_mean), league_mean-opp_def_epa, 0),
    def_adjustment_factor = ifelse(!is.na(league_mean), league_mean-opp_off_epa, 0),
    adjusted_off_epa = off_epa + off_adjustment_factor,
    adjusted_def_epa = def_epa + def_adjustment_factor,
  )
# Group and Get Moving Average
epa_data <- epa_data %>%
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
  ungroup()
#Clean and Grab Needed Columns
epa_data <- epa_data %>%
  select(game_id, season, week, posteam, home_team, away_team, off_epa, def_epa, adjusted_off_epa, adjusted_def_epa) %>%
  rename(team = posteam)

### Get Game Schedule and Wins
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
      select(game_id, season, week, team, opponent, points_for, points_against) %>%
      rename(
        opp_points_for = points_for,
        opp_points_against = points_against
      ) %>%
      group_by(team) %>%
      arrange(season, week) %>%
      mutate(
        opp_points_for = lag(pracma::movavg(opp_points_for, n = 10, type = "s")),
        opp_points_against = lag(pracma::movavg(opp_points_against, n = 10, type = "s")),
      ) %>%
      select(-season, -week),
    by = c("game_id", "team" = "opponent", "opponent" = "team"),
    all.x = TRUE
  )
# Merge Back Mean
NFL_Outcomes_Weekly <- NFL_Outcomes_Weekly %>%
  left_join(NFL_Outcomes_Weekly %>%
              filter(team == home_team) %>%
              group_by(season, week) %>%
              summarise(
                league_mean = mean(points_for)
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
    point_differential = lag(pracma::movavg(point_differential, n = 10, type = "s")),
    adjusted_point_differential = lag(pracma::movavg(adjusted_point_differential, n = 10, type = "s")),
  ) %>%
  ungroup()
#Clean and Grab Important Columns
NFL_Outcomes_Weekly <- NFL_Outcomes_Weekly %>%
  filter(season > 2006) %>%
  select(game_id, home_team, away_team, team, win, point_differential, adjusted_point_differential)

### Merge Data and Pitor
#merge
nfl_data <- NFL_Outcomes_Weekly %>%
  left_join(
    epa_data,
    by = c("game_id", "home_team", "away_team", "team")
  ) %>%
  filter(!is.na(off_epa))
#add two additional model columns
#pivot long
nfl_data_long <- nfl_data %>%
  pivot_longer(cols = c(-home_team, -away_team, -team, -season, -week, -game_id, -win), names_to = "metric", values_to = "value") 
#Get R-Squared Data
nfl_data_long <- nfl_data_long %>% 
  nest(data = c(-metric)) %>% 
  mutate(
    regression = map(data, ~ glm(win ~ value, data = .x, family = "binomial")),
    r_squared = map(regression, fmsb::NagelkerkeR2)
  ) %>% 
  hoist(r_squared, r.squared = "R2") %>% 
  arrange(desc(r.squared)) %>% 
  select(metric, r.squared)

### Compare this Data to a combined glm data
nfl_data_long <- nfl_data_long %>%
  bind_rows(
    data.frame(
      metric = c("adjusted_off_epa + adjusted_def_epa", "off_epa + def_epa"),
      r.squared = c(fmsb::NagelkerkeR2(glm(data = nfl_data, win ~ adjusted_off_epa + adjusted_def_epa))$R2, fmsb::NagelkerkeR2(glm(data = nfl_data, win ~ off_epa + def_epa))$R2)
      )
  ) %>%
  arrange(-r.squared) %>%
  mutate(name = c("Point Differential", "Adjusted Off EPA + Adjusted DEF EPA", "Adjusted Point Differential", "Off EPA + Def EPA", "OFF EPA", "Adjusted OFF EPA", "Def EPA", "Adjusted Def EPA"))


### Plot it All
nfl_data_long %>%
  mutate(name = c("Point Differential", "Adjusted Off EPA + Adjusted DEF EPA", "Adjusted Point Differential", "Off EPA + Def EPA", "OFF EPA", "Adjusted OFF EPA", "Def EPA", "Adjusted Def EPA")) %>%
  mutate(name = factor(name, levels = name[order(-r.squared)])) %>% 
  ggplot(aes(x = name, y = r.squared, fill = r.squared)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=round(r.squared, 4)), position=position_dodge(width=0.9), vjust=-0.25, size = 4, ) +
  theme_minimal() + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 28), 
        axis.text.x = element_text(angle = 10, size = 9),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  labs(
    x = "Prediction Metric",
    y = "Nagelkerke Pseudo R-Squared",
    title = "Strength of Predictors For The Outcome Of An NFL Game",
    subtitle = "Predictors Are a Moving Average Of a Team's Last 10 Games | Logistic Regression Model to Predict Whether a Team Wins",
    caption = "@gberg1303 | Data from: @nflfastR | Games from 2007-2019 NFL seasons"
  ) + 
  ggsave(("/Users/jonathangoldberg/Downloads/Strength of Predictors For The Outcome Of An NFL Game.jpeg"), width = 16, height = 8.4, unit = "in")


####################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################
### Repeat With Fumbles Excluded and Penalties Included
####################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################
### Get EPA per game
epa_data_fumble_penalty <- NFL_PBP %>%
  filter(!is.na(epa), !is.na(ep), !is.na(posteam), season >= 2007, fumble == 0) %>%
  group_by(game_id, season, week, posteam, home_team) %>%
  summarise(
    off_epa= mean(epa),
  ) %>%
  left_join(NFL_PBP %>%
              filter(!is.na(epa), !is.na(ep), !is.na(posteam), play_type=="pass" | play_type=="run", season >= 2007, fumble == 0) %>%
              group_by(game_id, season, week, defteam, away_team) %>% 
              summarise(def_epa=mean(epa)),
            by = c("game_id", "posteam" = "defteam", "season", "week"),
            all.x = TRUE) %>%
  mutate(opponent = ifelse(posteam == home_team, away_team, home_team)) %>%
  select(game_id, season, week, home_team, away_team, posteam, opponent, off_epa, def_epa) %>%
  collect()
# Merge Back Opponent 
epa_data_fumble_penalty <- epa_data_fumble_penalty %>%
  left_join(epa_data_fumble_penalty %>%
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
epa_data_fumble_penalty <- epa_data_fumble_penalty %>%
  left_join(epa_data_fumble_penalty %>%
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
epa_data_fumble_penalty <- epa_data_fumble_penalty %>%
  mutate(
    off_adjustment_factor = ifelse(!is.na(league_mean), league_mean-opp_def_epa, 0),
    def_adjustment_factor = ifelse(!is.na(league_mean), league_mean-opp_off_epa, 0),
    adjusted_off_epa = off_epa + off_adjustment_factor,
    adjusted_def_epa = def_epa + def_adjustment_factor,
  )
# Group and Get Moving Average
epa_data_fumble_penalty <- epa_data_fumble_penalty %>%
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
  ungroup()
#Clean and Grab Needed Columns
epa_data_fumble_penalty <- epa_data_fumble_penalty %>%
  select(game_id, season, week, posteam, home_team, away_team, off_epa, def_epa, adjusted_off_epa, adjusted_def_epa) %>%
  rename(team = posteam)

### Merge Data and Pitor
#merge
nfl_data_fumble_penalty <- NFL_Outcomes_Weekly %>%
  left_join(
    epa_data_fumble_penalty,
    by = c("game_id", "home_team", "away_team", "team")
  ) %>%
  filter(!is.na(off_epa))
#add two additional model columns
#pivot long
nfl_data_long_fumble_penalty <- nfl_data_fumble_penalty %>%
  pivot_longer(cols = c(-home_team, -away_team, -team, -season, -week, -game_id, -win), names_to = "metric", values_to = "value") 
#Get R-Squared Data
nfl_data_long_fumble_penalty <- nfl_data_long_fumble_penalty %>% 
  nest(data = c(-metric)) %>% 
  mutate(
    regression = map(data, ~ glm(win ~ value, data = .x, family = "binomial")),
    r_squared = map(regression, fmsb::NagelkerkeR2)
  ) %>% 
  hoist(r_squared, r.squared = "R2") %>% 
  arrange(desc(r.squared)) %>% 
  select(metric, r.squared)

### Compare this Data to a combined glm data
nfl_data_long_fumble_penalty <- nfl_data_long_fumble_penalty %>%
  bind_rows(
    data.frame(
      metric = c("adjusted_off_epa + adjusted_def_epa", "off_epa + def_epa"),
      r.squared = c(fmsb::NagelkerkeR2(glm(data = nfl_data_fumble_penalty, win ~ adjusted_off_epa + adjusted_def_epa))$R2, fmsb::NagelkerkeR2(glm(data = nfl_data_fumble_penalty, win ~ off_epa + def_epa))$R2)
    )
  ) %>%
  arrange(-r.squared)


### Grab the Important Metrics and Plot
nfl_data_long_fumble_penalty %>%
  mutate(name = c("Point Differential", "*Adjusted Off EPA + Adjusted DEF EPA*", "Adjusted Point Differential", "*Off EPA + Def EPA*", "*OFF EPA*", "*Adjusted OFF EPA*", "*Def EPA*", "*Adjusted Def EPA*")) %>%
  filter(name != "Point Differential", name != "Adjusted Point Differential") %>%
  mutate(name = factor(name, levels = name[order(-r.squared)])) %>% 
  mutate(fumble_penalty = as.factor("Yes")) %>%
  bind_rows(nfl_data_long %>% mutate(fumble_penalty = as.factor("No"))) %>%
  mutate(name = factor(name, levels = name[order(-r.squared)])) %>% 
  ggplot(aes(x = name, y = r.squared, fill = fumble_penalty)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=round(r.squared, 4)), position=position_dodge(width=0.9), vjust=-0.25, size = 4) +
  theme_minimal() + 
  theme(
        plot.title = element_text(face = "bold", size = 28), 
        axis.text.x = element_text(angle = 10, size = 8),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)
        ) +
  labs(
    x = "Prediction Metric",
    y = "Nagelkerke Pseudo R-Squared",
    title = "Strength of Predictors For The Outcome Of An NFL Game",
    subtitle = "Predictors Are a Moving Average Of a Team's Last 10 Games | Logistic Regression Model to Predict Whether a Team Wins",
    caption = "@gberg1303 | Data from: @nflfastR | Games from 2007-2019 NFL seasons",
    fill = "Fumbles Excluded, Penalties Included"
  ) + 
  ggsave(("/Users/jonathangoldberg/Downloads/Strength of Predictors For The Outcome Of An NFL Game (!Fumbles and with Penalties).jpeg"), width = 16, height = 8.4, unit = "in")




