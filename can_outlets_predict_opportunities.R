library(tidyverse)

# Load last year fantasy projections
fantasy_predictions_2019 <- read_csv("/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/2019-2020/Projections/Average Fantasy Season Projections 2019.csv") %>%
  mutate(Opportunities = Carries + Targets,
         Touches = Carries + Receptions)

# Historical Data
setwd("/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Historical Statistics Scraping")
source("/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Historical Statistics Scraping/Historical Statistics Function.R")
Historical_Football_Statistics(2018, 2019, 1, 17, Player_Seasons = TRUE, Team_Defense = FALSE, Red_Zone = FALSE, Player_Weeks = FALSE, Draft = FALSE)

data <- Player_Season_Data %>%
  filter(Year == 2019) %>%
  left_join(fantasy_predictions_2019 %>% select(Player, Position, Opportunities, Touches, Average.Fantasy.Points), by = c("Player", "Position")) %>%
  left_join(Player_Season_Data %>% filter(Year == 2018) %>% mutate(prior_opportunities = `Rushing Attempts` + Targets, prior_touches = `Rushing Attempts` + Receptions) %>% select(Player, Position, prior_opportunities, prior_touches),
            by = c("Player", "Position"),
            all.x = TRUE)


summary(lm(
  data = data %>% 
    filter(`Games Played` >= 3, Position == "WR" | Position == "RB"),
  formula = `Fantasy Overall Rank` ~ Opportunities
))$adj.r.squared

summary(lm(
  data = data %>% 
    filter(`Games Played` >= 3, Position == "WR" | Position == "RB"),
  formula = `Fantasy Overall Rank` ~ Touches
))$adj.r.squared

summary(lm(
  data = data %>% 
    filter(`Games Played` >= 3, Position == "WR" | Position == "RB"),
  formula = `Fantasy Overall Rank` ~ Average.Fantasy.Points
))$adj.r.squared

summary(lm(
  data = data %>% 
    filter(`Games Played` >= 3, Position == "WR" | Position == "RB"),
  formula = `Fantasy Overall Rank` ~ prior_opportunities
))$adj.r.squared

summary(lm(
  data = data %>% 
    filter(`Games Played` >= 3, Position == "WR" | Position == "RB"),
  formula = `Fantasy Overall Rank` ~ prior_touches
))$adj.r.squared

summary(lm(
  data = data %>% 
    filter(`Games Played` >= 3, Position == "WR" | Position == "RB"),
  formula = `Fantasy Overall Rank` ~ prior_opportunities + Opportunities
))$adj.r.squared
