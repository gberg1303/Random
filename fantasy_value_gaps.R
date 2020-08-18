library(tidyverse)
library(ggimage)
library(ggplot2)
library(ggplotify)
library(teamcolors)
library(ggrepel)

##### Season Projections
source('/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Projection Scraping/Projections_Main.R', echo=TRUE)
setwd("/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Projection Scraping")
Fantasy_Football_Projections(sources = c(
  "FantasySharks",
  "CBS", 
  "ESPN", 
  "Sleeper", 
  #"Yahoo", 
  "FantasyPros", 
  "NFL"), Week = 0, Season = 2020, 
  Scoring = "Half", VOR = "Custom", MaxBid = FALSE, Keep.Platform.Projections = TRUE, Proper.Floors = TRUE, Predictions = TRUE)

### Get Statitics for Team Projections
team_projections <- Average_Fantasy_Projections %>%
  select(Team, Completions, Receptions, Receiving.Touchdowns, Receiving.Yards, Passing.Yards, Passing.Touchdowns) %>%
  group_by(Team) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  mutate(
    touchdown_gap = Passing.Touchdowns - Receiving.Touchdowns,
    yards_gap = Passing.Yards - Receiving.Yards,
    completion_gap = Completions - Receptions
  )

### Data Vis

# Touchdown Gap
team_projections %>% 
  filter(Team != "FA") %>%
  merge(x = ., y = nflfastR::teams_colors_logos, by.x = "Team", by.y = "team_abbr", all.x = TRUE) %>%
  mutate(Team = factor(Team, levels = Team[order(-touchdown_gap)]),
         team_name = gsub("Las Vegas Raiders", "Oakland Raiders" ,team_name)) %>%
  ggplot(aes(x = Team, y = touchdown_gap)) +
  geom_bar(stat = "identity", size = 1, width = .5, aes(color = team_name, fill = team_name)) +
  theme_minimal() +
  geom_image(aes(image = team_logo_espn), size = .025) +
  geom_label_repel(aes(x = Team, label = Passing.Touchdowns), size = 3) +
  scale_fill_teams(1, name = "team_name") + 
  scale_color_teams(2, name = "team_name") + 
  scale_y_continuous(expand = c(.1, 0)) +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 70),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  labs(y = "Difference Between Projected Passing TDs and Receiving TDs",
       x = "Team",
       subtitle = "Labels are Passing Statistics",
       title = paste0("Touchdown Gap: Difference Between Projected Passing Touchdowns and Receiving Touchdowns"), 
       caption = "@gberg1303 | sources: ESPN, Sleeper, CBS, NFL, Yahoo, Fantasy Sharks | Labels are Passing Stats")

# Completion  Gap
team_projections %>% 
  filter(Team != "FA") %>%
  merge(x = ., y = nflfastR::teams_colors_logos, by.x = "Team", by.y = "team_abbr", all.x = TRUE) %>%
  mutate(Team = factor(Team, levels = Team[order(-completion_gap)]),
         team_name = gsub("Las Vegas Raiders", "Oakland Raiders" ,team_name)) %>%
  ggplot(aes(x = Team, y = completion_gap)) +
  geom_bar(stat = "identity", size = 1, width = .5, aes(color = team_name, fill = team_name)) +
  theme_minimal() +
  geom_image(aes(image = team_logo_espn), size = .025) +
  geom_label_repel(aes(x = Team, label = Completions), size = 3) +
  scale_fill_teams(1, name = "team_name") + 
  scale_color_teams(2, name = "team_name") + 
  scale_y_continuous(expand = c(.1, 0)) +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 70),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  labs(y = "Difference Between Projected Passing Completions and Receiving Completions",
       x = "Team",
       subtitle = "Labels are Passing Statistics",
       title = paste0("Completion Gap: Difference Between Projected Passing Completions and Receiving Completions"), 
       caption = "@gberg1303 | sources: ESPN, Sleeper, CBS, NFL, Yahoo, Fantasy Sharks | Labels are Passing Stats")

# Yards Gap
team_projections %>% 
  filter(Team != "FA") %>%
  merge(x = ., y = nflfastR::teams_colors_logos, by.x = "Team", by.y = "team_abbr", all.x = TRUE) %>%
  mutate(Team = factor(Team, levels = Team[order(-yards_gap)]),
         team_name = gsub("Las Vegas Raiders", "Oakland Raiders" ,team_name)) %>%
  ggplot(aes(x = Team, y = yards_gap)) +
  geom_bar(stat = "identity", size = 1, width = .5, aes(color = team_name, fill = team_name)) +
  theme_minimal() +
  geom_image(aes(image = team_logo_espn), size = .025) +
  geom_label_repel(aes(x = Team, label = Passing.Yards), size = 3) +
  scale_fill_teams(1, name = "team_name") + 
  scale_color_teams(2, name = "team_name") + 
  scale_y_continuous(expand = c(.1, 0)) +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 70),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  labs(y = "Difference Between Projected Passing Yards and Receiving Yards",
       x = "Team",
       subtitle = "Labels are Passing Statistics",
       title = paste0("Yards Gap: Difference Between Projected Passing Yards and Receiving Yards"), 
       caption = "@gberg1303 | sources: ESPN, Sleeper, CBS, NFL, Yahoo, Fantasy Sharks | Labels are Passing Stats")



# Leftover Completions  Gap
Average_Fantasy_Projections %>%
  filter(Position.Rank <= 50) %>%
  select(Team, Completions, Receptions, Receiving.Touchdowns, Receiving.Yards, Passing.Yards, Passing.Touchdowns) %>%
  group_by(Team) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  mutate(
    touchdown_gap = Passing.Touchdowns - Receiving.Touchdowns,
    yards_gap = Passing.Yards - Receiving.Yards,
    completion_gap = Completions - Receptions
  ) %>% 
  filter(Team != "FA") %>%
  merge(x = ., y = nflfastR::teams_colors_logos, by.x = "Team", by.y = "team_abbr", all.x = TRUE) %>%
  mutate(Team = factor(Team, levels = Team[order(-completion_gap)]),
         team_name = gsub("Las Vegas Raiders", "Oakland Raiders" ,team_name)) %>%
  ggplot(aes(x = Team, y = completion_gap)) +
  geom_bar(stat = "identity", size = 1, width = .5, aes(color = team_name, fill = team_name)) +
  theme_minimal() +
  geom_image(aes(image = team_logo_espn), size = .025) +
  geom_label_repel(aes(x = Team, label = Completions), size = 3) +
  scale_fill_teams(1, name = "team_name") + 
  scale_color_teams(2, name = "team_name") + 
  scale_y_continuous(expand = c(.1, 0)) +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 70),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  labs(y = "Leftover Completions",
       x = "Team",
       subtitle = "Labels are Passing Statistics",
       title = paste0("Leftover Completions: How Many Receptions Do Receivers Projected Outside The Top 50 At Their Position Get?"), 
       caption = "@gberg1303 | sources: ESPN, Sleeper, CBS, NFL, Yahoo, Fantasy Sharks | Labels are Passing Stats")
