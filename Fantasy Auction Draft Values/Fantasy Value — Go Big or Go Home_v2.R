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

### Mutate Several Other Columns that May Be important
fantasy_data <- fantasy_data %>%
  mutate(position_ten = ifelse(positionRank <= 10, 1, 0),
         position_ten_twenty = ifelse(positionRank > 10 | positionRank <= 20, 1, 0)) 

### Create Dataset of Cheapest top 10 player and the cheapest player from the cohort
fantasy_data %>%
  left_join(
    fantasy_data %>% filter(position_ten == 1) %>% group_by(position, year) %>% summarise(auctionValue = min(auctionValue)) %>% dplyr::rename(av = auctionValue),
    by = c("position", "year")
  ) %>% 
  filter(position_ten == 1 & auctionValue == av)

fantasy_data %>%
  filter(position_ten == 1 & auctionValue == av)
