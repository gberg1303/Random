library(nflfastR)
library(DBI)
library(RSQLite)
library(tidyverse)
### Load Data
NFL_PBP <- tbl(dbConnect(SQLite(), "/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Raw NFL Seasons Data/pbp_db"),
               "nflfastR_pbp")

############## QB Pass Location
### Get Pass Location Data
Pass_Data <- NFL_PBP %>% 
  filter(!is.na(air_yards)) %>%
  group_by(air_yards, season) %>%
  summarise(count = n()) %>%
  filter(air_yards > -5)

### Plot and Animate
Pass_Data %>%
  ggplot(aes(x = air_yards, y = count)) +
  geom_bar(stat = "identity", size = 1, width = .5, color = "light blue", fill = "light blue") +
  labs(
    x = "Air Yards",
    y = "Frequency",
    title = "Has the Passing Game Changed? Air Yards Over Time: ({frame_time})",
    caption = "@gberg1303 | data from @nflfastR"
  ) +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  gganimate::transition_time(season)


################## Go for it Rate
library(ggimage)
library(tidyverse)
library(teamcolors)
library(gganimate)

Go_For_It_Data <- NFL_PBP %>%
  filter(wp > .2 & wp < .8) %>%
  filter(down == 4) %>%
  filter(
    ydstogo == 1 |
      ydstogo == 2 & yardline_100 < 79 |
      ydstogo == 3 & yardline_100 %in% c(59:22) |
      ydstogo == 3 & yardline_100 %in% c(4:3) |
      ydstogo == 4 & yardline_100 %in% c(54:29) |
      ydstogo == 5 & yardline_100 %in% c(50:33) | 
      ydstogo == 6 & yardline_100 %in% c(48:34) | 
      ydstogo == 7 & yardline_100 %in% c(44:36) |
      ydstogo == 8 & yardline_100 %in% c(41:37) | 
      ydstogo == 9 & yardline_100 %in% c(39:38)
  ) %>%
  mutate(Went_For_It = ifelse(pass == 1 | rush == 1, 1, 0)) %>%
  group_by(posteam, season) %>%
  summarise(
    EPA_Total = sum(epa),
    Plays = n(),
    EPA_per_Play = mean(epa),
    Went_For_It = sum(Went_For_It)
  ) %>%
  mutate(GO_Rate = Went_For_It/Plays*100) %>%
  merge(x = ., y = nflfastR::teams_colors_logos, by.x = "posteam", by.y = "team_abbr", all.x = TRUE) %>% 
  group_by(season) %>%
  mutate(team_name = factor(team_name, levels = team_name[order(-GO_Rate)])) %>%
  ungroup()

animate(
  Go_For_It_Data %>%
    group_by(season) %>%
    mutate(order = rank(-GO_Rate, ties.method = "random")) %>%
    arrange(order) %>%
    ggplot(aes(x = order, y = GO_Rate)) + 
    theme_minimal() +
    geom_bar(stat = "identity", size = 1, width = .5, aes(color = team_color2, fill = team_color)) +
    geom_image(aes(image = team_logo_espn), size = .05, nudge_y = 1) +
    scale_fill_manual(values = set_names(unique(Go_For_It_Data$team_color), as.vector(unique(Go_For_It_Data$team_color)))) + 
    scale_color_manual(values = set_names(unique(Go_For_It_Data$team_color2), as.vector(unique(Go_For_It_Data$team_color2)))) + 
    scale_y_continuous(expand = c(.1, 0)) +
    theme(legend.position = "none", 
          plot.title = element_text(face = "bold"), 
          axis.text.x = element_blank(),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold")) +
    labs(y = "4th Down Go-For-It Rate",
         x = "Team",
         title = paste0("NFL ({frame_time}): 4th Down Go-For-It Rate*"), 
         caption = "@gberg1303 | nflfastR | Within 20-80% Win Probabilty | *On Plays Recommended by the NYT") +
    gganimate::transition_time(season),
  width = 1024, height = 512, duration = 63, fps = 20
)
