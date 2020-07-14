library(DBI)
library(RSQLite)
library(nflfastR)

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
#Clean and Grab Needed Columns
epa_data <- epa_data %>%
  select(game_id,season, week, home_team, away_team, off_epa, def_epa, adjusted_off_epa, adjusted_def_epa) %>%
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
#Clean and Grab Important Columns
NFL_Outcomes_Weekly <- NFL_Outcomes_Weekly %>%
  filter(season > 2006) %>%
  select(game_id, home_team, away_team, team, win, point_differential, adjusted_point_differential)

### Divide into two halves of the season and merge back together
#Front 8
nfl_dataset_front <- NFL_Outcomes_Weekly %>% 
  left_join(epa_data,
            by = c("game_id", "home_team", "away_team", "team")) %>%
  filter(week <= 8) %>%
  group_by(team, season) %>%
  summarise(
    games_front = n(),
    win_front = sum(win),
    point_differential_front = mean(point_differential),
    adjusted_point_differential_front = mean(adjusted_point_differential),
    off_epa_front = mean(off_epa),
    def_epa_front = mean(def_epa),
    adjusted_off_epa_front = mean(adjusted_off_epa),
    adjusted_def_epa_front = mean(adjusted_def_epa)
  )
#Back 8
nfl_dataset_back <- NFL_Outcomes_Weekly %>% 
  left_join(epa_data,
            by = c("game_id", "home_team", "away_team", "team")) %>%
  filter(week > 8) %>%
  group_by(team, season) %>%
  summarise(
    games_back = n(),
    win_back = sum(win),
    point_differential_back = mean(point_differential),
    adjusted_point_differential_back = mean(adjusted_point_differential),
    off_epa_back = mean(off_epa),
    def_epa_back = mean(def_epa),
    adjusted_off_epa_back = mean(adjusted_off_epa),
    adjusted_def_epa_back = mean(adjusted_def_epa)
  )
#Merge Together
nfl_dataset <- nfl_dataset_front %>%
  left_join(
    nfl_dataset_back,
    by = c("team", "season")
  )


### Get R-Squared Values from each model 
summary(lm(data = nfl_dataset, win_back))
  