library(tidyverse)

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
  mutate(outperforming_chance = (cumsum(Conversion_Rate)),
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

### Get Mean Auction Data for Top 20
top_twenty <- fantasy_data %>%
  filter(positionRank >= 16, positionRank <= 20) %>%
  group_by(position) %>%
  summarise(
    auctionValue = mean(auctionValue)
  ) %>%
  mutate(Projection = "Projected 16-20")

### Get Mean Auction Data for Top 20
top_twentyfive <- fantasy_data %>%
  filter(positionRank >= 21, positionRank <= 25) %>%
  group_by(position) %>%
  summarise(
    auctionValue = mean(auctionValue)
  ) %>%
  mutate(Projection = "Projected 21-25")

### Bind Rows
av_data <- bind_rows(
  top_five, top_ten, top_fifteen, top_twenty, 
  #top_twentyfive
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
av_data %>%
  filter(Number == Number_2) %>%
  mutate(players_per_star = ifelse(players_per_star > 3 & Position == "QB" | players_per_star > 3 & Position == "TE", 3, players_per_star)) %>%
  mutate(star_chance = 1-((1-outperforming_chance)^players_per_star)) 

av_data %>%
  filter(Number == 2) %>%
  mutate(players_per_star = ifelse(players_per_star > 3 & Position == "QB" | players_per_star > 3 & Position == "TE", 3, players_per_star)) %>%
  mutate(star_chance = 1-(1-outperforming_chance)^players_per_star) %>% view()



### Aight Lets Graph this Awful Code

# Top 5 Best Spending
av_data %>%
  ungroup() %>%
  filter(Number == 1) %>% # top 5
  mutate(players_per_star = ifelse(players_per_star > 3 & Position == "QB" | players_per_star > 3 & Position == "TE", 3, players_per_star)) %>%
  mutate(star_chance = 1-(1-outperforming_chance)^players_per_star,
         description = paste0(players_per_star," ", Position,"(s) ",  Projection)) %>%
  mutate(description = factor(description, levels = description[order(-star_chance)])) %>% 
  ggplot(aes(x = description, y = star_chance, fill = star_chance)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=round(star_chance, 3)), position=position_dodge(width=0.9), vjust=-0.25, size = 4, ) +
  theme_minimal() + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 28), 
        axis.text.x = element_text(angle = 10, size = 9),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  labs(
    x = "Draft Strategy at a Position",
    y = "Chance of A Player Finishing Top 5 at Their Position",
    title = "Optimal Auction Draft Strategy: Getting One Player In The Top 5 at a Position",
    subtitle = "*Price is equalized by position: if three players that are projected to finish 11-15 at their position are recommended, then they are roughly equal in price to one playern projected to finish 1-5 at the position.
*Percentages are calculated through historical data; for instance, a player projected to finish 1-5 at their position actually finishes 1-5 at the postition X percent of the time.",
    caption = "@gberg1303 | Data from: fantasyfootballanalytics.com | Fantasy Data from 2007-2018 NFL seasons"
  ) +
  ggsave(("/Users/jonathangoldberg/Downloads/Optimal Auction Top 5.jpeg"), width = 16, height = 8.4, unit = "in")


# Top 10 Best Spending
av_data %>%
  ungroup() %>%
  filter(Number == 2) %>% # top 5
  mutate(players_per_star = ifelse(players_per_star > 3 & Position == "QB" | players_per_star > 3 & Position == "TE", 3, players_per_star)) %>%
  mutate(star_chance = 1-(1-outperforming_chance)^players_per_star,
         description = paste0(players_per_star," ", Position,"(s) ",  Projection)) %>%
  mutate(description = factor(description, levels = description[order(-star_chance)])) %>% 
  ggplot(aes(x = description, y = star_chance, fill = star_chance)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=round(star_chance, 3)), position=position_dodge(width=0.9), vjust=-0.25, size = 4, ) +
  theme_minimal() + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 28), 
        axis.text.x = element_text(angle = 10, size = 9),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  labs(
    x = "Draft Strategy at a Position",
    y = "Chance of A Player Finishing Top 10 at Their Position",
    title = "Optimal Auction Draft Strategy: Getting One Player in The Top 10 at a Position",
    subtitle = "*Price is equalized by position: if three players that are projected to finish 11-15 at their position are recommended, then they are roughly equal in price to one playern projected to finish 1-5 at the position.
*Percentages are calculated through historical data; for instance, a player projected to finish 1-5 at their position actually finishes 1-5 at the postition X percent of the time.",
    caption = "@gberg1303 | Data from: fantasyfootballanalytics.com | Fantasy Data from 2007-2018 NFL seasons"
  ) +
  ggsave(("/Users/jonathangoldberg/Downloads/Optimal Auction Top 10.jpeg"), width = 16, height = 8.4, unit = "in")


# Top 15 Best Spending
av_data %>%
  ungroup() %>%
  filter(Number == 3) %>% # top 5
  mutate(players_per_star = ifelse(players_per_star > 3 & Position == "QB" | players_per_star > 3 & Position == "TE", 3, players_per_star)) %>%
  mutate(star_chance = 1-(1-outperforming_chance)^players_per_star,
         description = paste0(players_per_star," ", Position,"(s) ",  Projection)) %>%
  mutate(description = factor(description, levels = description[order(-star_chance)])) %>% 
  ggplot(aes(x = description, y = star_chance, fill = star_chance)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=round(star_chance, 3)), position=position_dodge(width=0.9), vjust=-0.25, size = 4, ) +
  theme_minimal() + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 28), 
        axis.text.x = element_text(angle = 10, size = 9),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  labs(
    x = "Draft Strategy at a Position",
    y = "Chance of A Player Finishing Top 15 at Their Position",
    title = "Optimal Auction Draft Strategy: Getting One Player In The Top 15 at a Position",
    subtitle = "*Price is equalized by position: if three players that are projected to finish 11-15 at their position are recommended, then they are roughly equal in price to one playern projected to finish 1-5 at the position.
*Percentages are calculated through historical data; for instance, a player projected to finish 1-5 at their position actually finishes 1-5 at the postition X percent of the time.",
    caption = "@gberg1303 | Data from: fantasyfootballanalytics.com | Fantasy Data from 2007-2018 NFL seasons"
  ) +
  ggsave(("/Users/jonathangoldberg/Downloads/Optimal Auction Top 15.jpeg"), width = 16, height = 8.4, unit = "in")


### Lets use current Data

Fantasy_Football_Projections(sources = c(
  "FantasySharks",
  "CBS", 
  "ESPN", 
  "Sleeper", 
  #"Yahoo", 
  "FantasyPros", 
  "NFL"), Week = 0, Season = 2020, 
  Scoring = "Half", VOR = "Standard", MaxBid = FALSE, Keep.Platform.Projections = TRUE, Proper.Floors = TRUE, Predictions = TRUE)

### Two Player Pairs
two_pairs <- Player_Predictions %>%
  mutate(Player = as.character(Player)) %>%
  na.omit() %>%
  group_by(Position) %>%
  mutate(bucket = cut(Position.Rank, 
                      breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, Inf), 
                      labels = c("Projected 1-5", "Projected 6-10", "Projected 11-15", "Projected 16-20", "Projected 21-25", "Projected 26-30", "Projected 31-35", "Projected 36-40", "Projected Above 40")),
         max_auction = max(Average.Auction.Value)
         ) %>%
  base::split(paste(.$Position))%>%
  purrr::map(., 1) %>%
  purrr::map(~combn(.x, m = 2)) %>%
  purrr::map(~t(.x))%>%
  purrr::map_dfr(as_tibble) %>%
  dplyr::rename(Player_1 = V1,
                Player_2 = V2) %>%
  left_join(
    Player_Predictions %>% na.omit() %>% group_by(Position) %>% mutate(Player = as.character(Player), max_auction = max(Average.Auction.Value)) %>% select(Player, Position, Average.Auction.Value, Finished.Top.5, Finished.Top.10, Finished.Top.15, max_auction),
    by = c("Player_1" = "Player")
  ) %>%
  left_join(
    Player_Predictions %>% na.omit() %>% group_by(Position) %>% mutate(Player = as.character(Player), max_auction = max(Average.Auction.Value)) %>% select(Player, Position, Average.Auction.Value, Finished.Top.5, Finished.Top.10, Finished.Top.15, max_auction),
    by = c("Player_2" = "Player", "Position")
  )


three_pairs <- Player_Predictions %>%
  mutate(Player = as.character(Player)) %>%
  na.omit() %>%
  group_by(Position) %>%
  mutate(bucket = cut(Position.Rank, 
                      breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, Inf), 
                      labels = c("Projected 1-5", "Projected 6-10", "Projected 11-15", "Projected 16-20", "Projected 21-25", "Projected 26-30", "Projected 31-35", "Projected 36-40", "Projected Above 40")),
         max_auction = max(Average.Auction.Value)
  ) %>%
  base::split(paste(.$Position))%>%
  purrr::map(., 1) %>%
  purrr::map(~combn(.x, m = 3)) %>%
  purrr::map(~t(.x))%>%
  purrr::map_dfr(as_tibble) %>%
  dplyr::rename(Player_1 = V1,
                Player_2 = V2,
                Player_3 = V3) %>%
  left_join(
    Player_Predictions %>% na.omit() %>% group_by(Position) %>% mutate(Player = as.character(Player), max_auction = max(Average.Auction.Value)) %>% select(Player, Position, Average.Auction.Value, Finished.Top.5, Finished.Top.10, Finished.Top.15, max_auction),
    by = c("Player_1" = "Player")
  ) %>%
  left_join(
    Player_Predictions %>% na.omit() %>% group_by(Position) %>% mutate(Player = as.character(Player), max_auction = max(Average.Auction.Value)) %>% select(Player, Position, Average.Auction.Value, Finished.Top.5, Finished.Top.10, Finished.Top.15, max_auction),
    by = c("Player_2" = "Player", "Position")
  ) %>%
  left_join(
    Player_Predictions %>% na.omit()  %>% group_by(Position) %>% mutate(Player = as.character(Player), max_auction = max(Average.Auction.Value)) %>% select(Player, Position, Average.Auction.Value, Finished.Top.5, Finished.Top.10, Finished.Top.15, max_auction),
    by = c("Player_3" = "Player", "Position")
  )


### Bind the Rows Together 
bind_rows(
  two_pairs, three_pairs) %>%
  mutate(
    combined_cost = ifelse(is.na(Average.Auction.Value),  Average.Auction.Value.x + Average.Auction.Value.y,Average.Auction.Value + Average.Auction.Value.x + Average.Auction.Value.y),
    top_five_chance = ifelse(is.na(Finished.Top.5), 1-((1-Finished.Top.5.x)*(1-Finished.Top.5.y)) , 1-((1-Finished.Top.5)*(1-Finished.Top.5.x)*(1-Finished.Top.5.y))),
    top_ten_chance = ifelse(is.na(Finished.Top.10), 1-((1-Finished.Top.10.x)*(1-Finished.Top.10.y)) , 1-((1-Finished.Top.10)*(1-Finished.Top.10.x)*(1-Finished.Top.10.y))),
    top_fifteen_chance = ifelse(is.na(Finished.Top.15), 1-((1-Finished.Top.15.x)*(1-Finished.Top.15.y)) , 1-((1-Finished.Top.15)*(1-Finished.Top.15.x)*(1-Finished.Top.15.y)))
  ) %>%
  bind_rows(
    Player_Predictions  %>% na.omit() %>% group_by(Position) %>% mutate(Player = as.character(Player), max_auction = max(Average.Auction.Value)) %>% select(Player, Position, Average.Auction.Value, Finished.Top.5, Finished.Top.10, Finished.Top.15, max_auction) %>% dplyr::rename(Player_1 = Player)
  ) %>%
  ungroup() %>%
  mutate(
    combined_cost = ifelse(is.na(combined_cost), Average.Auction.Value,combined_cost),
    top_five_chance = ifelse(is.na(top_five_chance), Finished.Top.5, top_five_chance),
    top_ten_chance = ifelse(is.na(top_ten_chance), Finished.Top.10,top_ten_chance),
    top_fifteen_chance = ifelse(is.na(top_fifteen_chance), Finished.Top.15,top_fifteen_chance)
  ) %>%
  group_by(Position) %>%
  mutate(
    max_top_five_chance = max(Finished.Top.5, na.rm = TRUE),
    max_top_ten_chance = max(Finished.Top.10, na.rm = TRUE),
    max_top_fifteen_chance = max(Finished.Top.15, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(combined_cost <= max_auction) %>%
  filter(
         top_five_chance > max_top_five_chance 
         #top_ten_chance > max_top_ten_chance |
         #top_fifteen_chance > max_top_fifteen_chance
         #top_five_chance > .25 | top_ten_chance > .45 | top_fifteen_chance > .75
         ) %>%  
  group_by(Position) %>% 
  summarise(n())

bind_rows(
  two_pairs, three_pairs) %>%
  mutate(
    combined_cost = ifelse(is.na(Average.Auction.Value),  Average.Auction.Value.x + Average.Auction.Value.y,Average.Auction.Value + Average.Auction.Value.x + Average.Auction.Value.y),
    top_five_chance = ifelse(is.na(Finished.Top.5), 1-((1-Finished.Top.5.x)*(1-Finished.Top.5.y)) , 1-((1-Finished.Top.5)*(1-Finished.Top.5.x)*(1-Finished.Top.5.y))),
    top_ten_chance = ifelse(is.na(Finished.Top.10), 1-((1-Finished.Top.10.x)*(1-Finished.Top.10.y)) , 1-((1-Finished.Top.10)*(1-Finished.Top.10.x)*(1-Finished.Top.10.y))),
    top_fifteen_chance = ifelse(is.na(Finished.Top.15), 1-((1-Finished.Top.15.x)*(1-Finished.Top.15.y)) , 1-((1-Finished.Top.15)*(1-Finished.Top.15.x)*(1-Finished.Top.15.y)))
  ) %>%
  bind_rows(
    Player_Predictions  %>% na.omit() %>% group_by(Position) %>% mutate(Player = as.character(Player), max_auction = max(Average.Auction.Value)) %>% select(Player, Position, Average.Auction.Value, Finished.Top.5, Finished.Top.10, Finished.Top.15, max_auction) %>% dplyr::rename(Player_1 = Player)
  ) %>%
  ungroup() %>%
  mutate(
    combined_cost = ifelse(is.na(combined_cost), Average.Auction.Value,combined_cost),
    top_five_chance = ifelse(is.na(top_five_chance), Finished.Top.5, top_five_chance),
    top_ten_chance = ifelse(is.na(top_ten_chance), Finished.Top.10,top_ten_chance),
    top_fifteen_chance = ifelse(is.na(top_fifteen_chance), Finished.Top.15,top_fifteen_chance)
  ) %>%
  group_by(Position) %>%
  mutate(
    max_top_five_chance = max(Finished.Top.5, na.rm = TRUE),
    max_top_ten_chance = max(Finished.Top.10, na.rm = TRUE),
    max_top_fifteen_chance = max(Finished.Top.15, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(combined_cost <= max_auction) %>%
  filter(
    #top_five_chance > max_top_five_chance 
    top_ten_chance > max_top_ten_chance 
    #top_fifteen_chance > max_top_fifteen_chance
    #top_five_chance > .25 | top_ten_chance > .45 | top_fifteen_chance > .75
  ) %>%  
  group_by(Position) %>% 
  summarise(n())

bind_rows(
  two_pairs, three_pairs) %>%
  mutate(
    combined_cost = ifelse(is.na(Average.Auction.Value),  Average.Auction.Value.x + Average.Auction.Value.y,Average.Auction.Value + Average.Auction.Value.x + Average.Auction.Value.y),
    top_five_chance = ifelse(is.na(Finished.Top.5), 1-((1-Finished.Top.5.x)*(1-Finished.Top.5.y)) , 1-((1-Finished.Top.5)*(1-Finished.Top.5.x)*(1-Finished.Top.5.y))),
    top_ten_chance = ifelse(is.na(Finished.Top.10), 1-((1-Finished.Top.10.x)*(1-Finished.Top.10.y)) , 1-((1-Finished.Top.10)*(1-Finished.Top.10.x)*(1-Finished.Top.10.y))),
    top_fifteen_chance = ifelse(is.na(Finished.Top.15), 1-((1-Finished.Top.15.x)*(1-Finished.Top.15.y)) , 1-((1-Finished.Top.15)*(1-Finished.Top.15.x)*(1-Finished.Top.15.y)))
  ) %>%
  bind_rows(
    Player_Predictions  %>% na.omit() %>% group_by(Position) %>% mutate(Player = as.character(Player), max_auction = max(Average.Auction.Value)) %>% select(Player, Position, Average.Auction.Value, Finished.Top.5, Finished.Top.10, Finished.Top.15, max_auction) %>% dplyr::rename(Player_1 = Player)
  ) %>%
  ungroup() %>%
  mutate(
    combined_cost = ifelse(is.na(combined_cost), Average.Auction.Value,combined_cost),
    top_five_chance = ifelse(is.na(top_five_chance), Finished.Top.5, top_five_chance),
    top_ten_chance = ifelse(is.na(top_ten_chance), Finished.Top.10,top_ten_chance),
    top_fifteen_chance = ifelse(is.na(top_fifteen_chance), Finished.Top.15,top_fifteen_chance)
  ) %>%
  group_by(Position) %>%
  mutate(
    max_top_five_chance = max(Finished.Top.5, na.rm = TRUE),
    max_top_ten_chance = max(Finished.Top.10, na.rm = TRUE),
    max_top_fifteen_chance = max(Finished.Top.15, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(combined_cost <= max_auction) %>%
  filter(
    #top_five_chance > max_top_five_chance 
    #top_ten_chance > max_top_ten_chance 
    top_fifteen_chance > max_top_fifteen_chance
    #top_five_chance > .25 | top_ten_chance > .45 | top_fifteen_chance > .75
  ) %>%  
  group_by(Position) %>% 
  summarise(n())
