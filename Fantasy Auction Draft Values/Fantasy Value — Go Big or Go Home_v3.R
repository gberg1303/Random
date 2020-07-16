library(tidyverse)
library(simpleboot)

#### Load Past Fantasy Data
fantasy_data <- purrr::map_df(2008:2017, function(x) 
{readr::read_csv(glue::glue("/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Fantasy Projections/ffa_customrankings{x}-0.csv")) %>%
    mutate(year = x)}
  )
fantasy_data <- fantasy_data %>% bind_rows(
  read_csv("/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Fantasy Projections/ffa_customrankings2018-0.csv") %>%
    dplyr::rename(
      player = Player,
      team = Team,
      position = Position
    ) %>%
    mutate(year = 2018))
# Filter where there is no auction data
fantasy_data <- fantasy_data %>%
  filter(!is.na(auctionValue))

### Grab Historical Priors
historic_predictions <- read_csv("/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Projection Scraping/Dependent Files/Historical_Projection_Baselines.csv") %>%
# Add an ID for some more mutating
    left_join(
    data.frame(
      Actual = c("Finished.1.5", "Finished.6.10", "Finished.11.15", "Finished.16.20", "Finished.21.25", "Finished.26.30", "Finished.31.35", "Finished.36.40", "Finished.Above.40"), 
      Number = 1:9
    )
  ) %>%
  left_join(
    data.frame(
      Projection = c("Projected 1-5", "Projected 6-10", "Projected 11-15", "Projected 16-20", "Projected 21-25", "Projected 26-30", "Projected 31-35", "Projected 36-40", "Projected Above 40"), 
      Number_2 = 1:9
    )
  ) 
# Add Cumulative Chance of outperforming your prediction
historic_predictions <- historic_predictions %>%
  filter(!is.na(Number)) %>%
  group_by(Position, Projection) %>%
  arrange(Number) %>%
  mutate(outperforming_chance = lag(cumsum(Conversion_Rate)),
         outperforming_chance = ifelse(is.na(outperforming_chance), 0, outperforming_chance))


### Get Mean Auction Data for Top 5
top_five <- fantasy_data %>%
  filter(positionRank <= 5) %>%
  group_by(position) %>%
  summarise(
    auctionValue = mean(auctionValue)
  ) %>%
  mutate(Projection = "Projected 1-5")

### Get Mean Auction Data for Top 10
top_ten <- fantasy_data %>%
  filter(positionRank >= 6, positionRank <= 10) %>%
  group_by(position) %>%
  summarise(
    auctionValue = mean(auctionValue)
  ) %>%
  mutate(Projection = "Projected 6-10")

### Get Mean Auction Data for Top 15
top_fifteen <- fantasy_data %>%
  filter(positionRank >= 11, positionRank <= 15) %>%
  group_by(position) %>%
  summarise(
    auctionValue = mean(auctionValue)
  ) %>%
  mutate(Projection = "Projected 11-15")

### Bind Rows
av_data <- bind_rows(
  top_five, top_ten, top_fifteen
)

### Merge the datasets together
av_data <- historic_predictions %>%
  left_join(
    av_data,
    by = c("Projection", "Position" = "position")
    ) %>% na.omit()
# Add Positional Max Auction Value and other important metrics
av_data <- av_data %>%
  group_by(Position) %>%
  mutate(position_max_av = max(auctionValue),
         players_per_star = round(position_max_av/auctionValue))

### Get Desired Value Groups from Position
results <- av_data %>%
  filter(Number == Number_2) %>%
  mutate(players_per_star = ifelse(players_per_star > 3, 3, players_per_star)) %>%
  mutate(star_chance = 1-((1-outperforming_chance)^players_per_star)) 

