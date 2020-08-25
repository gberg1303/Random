library(nflfastR)
library(DBI)
library(RSQLite)
library(tidyverse)
### Load Data
NFL_PBP <- tbl(dbConnect(SQLite(), "/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Raw NFL Seasons Data/pbp_db"),
               "nflfastR_pbp")

### QB Data
QB_Data <- NFL_PBP %>%
  # Get CPOE
  filter(!is.na(cpoe)) %>%
  filter(!is.na(epa), play_type=="no play" | play_type=="pass" | play_type=="run") %>%
  filter(pass == 1) %>%
  filter(qtr <= 4) %>%
  group_by(passer_player_name, posteam, season) %>%
  summarise(
    cpoe = mean(cpoe)) %>%
  # Merge EPA per Dp
  left_join(NFL_PBP %>%
              filter(!is.na(epa), play_type=="no play" | play_type=="pass" | play_type=="run") %>%
              filter(pass == 1) %>%
              filter(qtr <= 4) %>%
              filter(is.na(penalty_yards) | penalty_yards == 0) %>%
              group_by(passer_player_name, posteam, season) %>%
              summarise(EPApd = mean(epa), 
                        Dropbacks = sum(pass)), 
            by = c("passer_player_name", "posteam", "season")) %>%
  ungroup() %>%
  # Filter and get percentile
  filter(Dropbacks > 150) %>%
  mutate(Composite = cpoe*.009+EPApd*.21+.09,
         Composite_Percentile = percent_rank(Composite), 
         cpoe_percentile = percent_rank(cpoe)) %>% 
  # Compare to prior years to find biggest jumps
  group_by(passer_player_name) %>%
  arrange(passer_player_name, season) %>%
  mutate(Composite_Difference = Composite_Percentile - lag(Composite_Percentile),
         cpoe_difference = cpoe_percentile - lag(cpoe_percentile),
         n= n()) %>%
  collect()



### Lets Visualize Over Time- Create Weekly Dataset
QB_Data <- NFL_PBP %>%
  # Get CPOE
  filter(!is.na(cpoe)) %>%
  filter(!is.na(epa), play_type=="no play" | play_type=="pass" | play_type=="run") %>%
  filter(pass == 1) %>%
  filter(qtr <= 4) %>%
  group_by(passer_player_name, posteam, season, week, game_date) %>%
  summarise(
    cpoe = mean(cpoe)) %>%
  collect() %>%
  # Merge EPA per Dp
  left_join(NFL_PBP %>%
              filter(!is.na(epa), play_type=="no play" | play_type=="pass" | play_type=="run") %>%
              filter(pass == 1) %>%
              filter(qtr <= 4) %>%
              filter(is.na(penalty_yards) | penalty_yards == 0) %>%
              group_by(passer_player_name, posteam, season, week, game_date) %>%
              summarise(EPApd = mean(epa), 
                        Dropbacks = sum(pass)) %>%
              collect(), 
            by = c("passer_player_name", "posteam", "season", "week", "game_date")) %>%
  ungroup() %>%
  # Filter and get percentile
  mutate(Composite = cpoe*.009+EPApd*.21+.09,
         Composite_Percentile = percent_rank(Composite)) %>% 
  # Compare to prior years to find biggest jumps
  group_by(passer_player_name) %>%
  arrange(passer_player_name) %>%
  mutate(Dropbacks = sum(Dropbacks)) %>%
  # Filter non-important DP
  filter(Dropbacks > 150) %>%
  # add moving average
  mutate(moving_composite = pracma::movavg(Composite, n = 3)) %>%
  left_join(nflfastR::teams_colors_logos %>% select(team_abbr, team_color),
            by = c("posteam" = "team_abbr"))

### Make Graphs to Visualize Over Time
QB_Data %>%
  group_by(season) %>%
  mutate(low_qb = quantile(moving_composite, .25),
         med_qb = quantile(moving_composite, .5),
         high_qb = quantile(moving_composite, .85)) %>%
  ungroup() %>%
  filter(season > 2016) %>%
  filter(Dropbacks > 2500) %>%
  ggplot(aes(x = game_date, y = moving_composite, group = passer_player_name)) +
  geom_line(aes(color = team_color)) +
  scale_color_manual(values = set_names(unique(QB_Data$team_color), as.vector(unique(QB_Data$team_color)))) + 
  theme_minimal() +
  geom_hline(aes(yintercept = med_qb), size = .09) +
  geom_hline(aes(yintercept = high_qb), size = .09, color = "light blue") +
  geom_hline(aes(yintercept = low_qb), size = .09, color = "red") +
  facet_wrap(facets = vars(passer_player_name)) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "Date",
    y = "Composite Rating (Moving Average)",
    title = "Moving Average of Quarterback Composite Ratings",
    caption = "@gberg1303 | Composite Rating from @benbbaldwin | data from @nflfastR"
  ) +
  gganimate::transition_reveal(as.Date(game_date))

