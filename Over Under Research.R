library(tidyverse)

games <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv")

# Home hits implied over
games %>% 
  filter(season >= 2010) %>%
  mutate(
    home_implied_total = (total_line - spread_line)/2,
    away_implied_total = home_implied_total+spread_line,
    home_over_hit = ifelse(home_score > home_implied_total, 1, 0),
    away_over_hit = ifelse(away_score > away_implied_total, 1, 0),
    home_win = ifelse(home_score > away_score, 1, 0),
    home_cover_spread = ifelse(home_score-away_score > spread_line, 1, 0),
    away_cover_spread = 1-home_cover_spread
  ) %>%
  filter(home_over_hit == 1) %>%
  pull(over_line_hit) %>%
  mean()

# Home hits implied over
games %>% 
  filter(season >= 2010) %>%
  mutate(
    home_implied_total = (total_line - spread_line)/2,
    away_implied_total = home_implied_total+spread_line,
    home_over_hit = ifelse(home_score > home_implied_total, 1, 0),
    away_over_hit = ifelse(away_score > away_implied_total, 1, 0),
    home_win = ifelse(home_score > away_score, 1, 0),
    away_win = 1-home_win,
    home_cover_spread = ifelse(home_score-away_score > spread_line, 1, 0),
    away_cover_spread = 1-home_cover_spread
  ) %>%
  filter(away_over_hit == 1) %>%
  pull(away_cover_spread) %>%
  mean()

# Favored Team Hit Implied Over
data <- games %>% 
  filter(season >= 2010) %>%
  mutate(
    home_implied_total = (total_line - spread_line)/2,
    away_implied_total = home_implied_total+spread_line,
    home_over_hit = ifelse(home_score > home_implied_total, 1, 0),
    away_over_hit = ifelse(away_score > away_implied_total, 1, 0),
    home_win = ifelse(home_score > away_score, 1, 0),
    away_win = 1-home_win,
    home_cover_spread = ifelse(home_score-away_score > spread_line, 1, 0),
    away_cover_spread = 1-home_cover_spread,
    favorite_over_hit = ifelse(spread_line >= 0, home_over_hit, away_over_hit),
    favorite_spread = ifelse(spread_line >= 0, home_cover_spread, away_cover_spread),
    favorite_win_hit = ifelse(spread_line >= 0, home_win, away_win),
    over_line_hit = ifelse(home_score + away_score > total_line, 1, 0)
  ) %>%
  filter(favorite_over_hit == 1, !is.na(home_win))


### Predictin