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
#Add and ID Column
fantasy_data <- fantasy_data %>%
  mutate(new_id = row_number())

### Convert to Long to prepare a bootstrap
boot_data <- fantasy_data %>%
  select(
    new_id, player, position, team, position, lower, points, upper
  ) %>%
  pivot_longer(cols = c(lower, points, upper), names_to = "projection", values_to = "value")

### Run the Loop of Bootstraps
Boot_Outcomes <- data.frame()
pb <- progress_bar$new(total = length((boot_data$new_id %>% unique())))
for(i in (boot_data$new_id %>% unique())){
  pb$tick()
  test_boot <- boot_data %>%
    filter(new_id == i) %>%
    select(new_id, position, value)
  
  Boot_Means <- cbind.data.frame(as.numeric(test_boot$new_id[1]), as.character(test_boot$position[1]), 
                                 (one.boot(data = test_boot$value, mean, R = 1000))$t
  ) %>%
    mutate(Boot_ID = dplyr::row_number())
  colnames(Boot_Means) <- c("new_id", "position", "value", "Boot_ID")
  
  Boot_Outcomes <- bind_rows(Boot_Outcomes, Boot_Means)
}
rm(i, pb)

### Get Points Rank for that Bootstrap
Boot_Results <- Boot_Outcomes %>%
  group_by(Boot_ID) %>%
  mutate(Points.Rank = order(value, decreasing=TRUE)) %>%
  group_by(Boot_ID) %>%
  group_by(position, add = TRUE) %>%
  mutate(Position.Rank = order(order(value, decreasing=TRUE)))

Boot_Results %>%
  group_by(new_id) %>%
  summarise(
    Points.Position.1.5 = mean(Position.Rank <= 5),
    Points.Position.6.10 = mean(Position.Rank <= 10 & Position.Rank > 5),
    Points.Position.11.15 = mean(Position.Rank <= 15 & Position.Rank > 10),
    Points.Position.16.20 = mean(Position.Rank <= 20 & Position.Rank > 15),
    Points.Position.21.25 = mean(Position.Rank <= 25 & Position.Rank > 20),
    Points.Position.26.30 = mean(Position.Rank <= 30 & Position.Rank > 25),
    Points.Position.31.35 = mean(Position.Rank <=35 & Position.Rank > 30),
    Points.Position.36.40 = mean(Position.Rank <= 40 & Position.Rank > 35),
    Points.Position.Above.40 = mean(Position.Rank > 40 & Position.Rank > 40)
  )
