library(tidyverse)
library(ggplot2)


NFL_PBP %>% 
  filter(pass_attempt == 1) %>%
  group_by(passer_player_name, passer_player_id) %>%
  summarise(
    games = n_distinct(game_id)
            ) %>%
  filter(games > 50) %>%
  left_join(NFL_PBP %>% filter(pass_attempt == 1) %>%
                group_by(passer_player_name, passer_player_id, air_yards) %>%
                summarise(count = n()), by = c("passer_player_id", "passer_player_name")) %>%
  ungroup() %>%
  mutate(
    count = count/games
  ) %>%
  ggplot(aes(x = air_yards, y = count)) + 
  geom_bar(stat = "identity") +
  geom_smooth(size = .5, se=FALSE) +
  facet_wrap(facets = c("passer_player_name"))
