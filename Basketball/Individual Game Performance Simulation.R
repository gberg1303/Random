### NBA StatR
library(nbastatR)
library(future)
library(tidyverse)
library(ggplot2)
library(lemon)
plan(multiprocess) 

# Scrape Data
Data_NBA_Game_Logs <- game_logs(2020, season_types = "Playoffs")
teams_shots(teams = "Heat", seasons = 2020)
df_dict_nba_teams <- df_nba_team_dict

Data_NBA_Shot_Locations <- bind_rows(teams_shots(all_active_teams = TRUE, seasons = 2020), 
                                     teams_shots(all_active_teams = TRUE, seasons = 2019),
                                     teams_shots(all_active_teams = TRUE, seasons = 2018),
                                     teams_shots(all_active_teams = TRUE, seasons = 2017)
)


### Generate Model
Model <- glm(Made ~ typeAction + typeShot + zoneBasic + distanceShot + typeAction*typeShot + zoneBasic*distanceShot,
    data = Data_NBA_Shot_Locations %>% filter(yearSeason != max(yearSeason)) %>% mutate(Made = ifelse(isShotMade == TRUE, 1, 0)))

### Apply Model to Game's Shots
Game_Shots <- teams_shots(all_active_teams = TRUE, season_types = "Playoffs", seasons = 2020) %>%
  filter(idGame == 41900313) %>%
  mutate(ecp = predict.glm(object = Model, newdata = ., type = c("response")),
         ecp = ifelse(ecp > 1, .99, ecp),
         ecp = ifelse(ecp < 0, .01, ecp))

### Run Simulations

Simulations <- furrr::future_map_dfr(1:10000, function(simulations){
  Game_Shots <- Game_Shots %>%
    mutate(outcome = stats::rbinom(n = nrow(Game_Shots), size = 1, prob=ecp),
           simulation = simulations)
  
  return(Game_Shots)
  })


### Create Graph
Simulations %>%
  mutate(shot = case_when(
    typeShot == "3PT Field Goal" ~ 3,
    typeShot == "2PT Field Goal" ~ 2,
    typeShot == "Free Throw" ~ 1),
    Points = shot*outcome) %>%
  group_by(namePlayer, simulation) %>%
  summarise(
    idGame = max(idGame),
   Points = sum(Points),
  ) %>%
  mutate(
    distro = case_when(
      Points > quantile(Points, .25) & Points < quantile(Points, .75) ~ "Middle",
      Points <= quantile(Points, .25) & Points >= quantile(Points, .75) ~ "Ends"
    )
  ) %>%
  left_join(Data_NBA_Game_Logs %>% select(idGame, namePlayer, pts, ftm),
            by = c("idGame", "namePlayer")) %>%
  mutate(percentage = percent_rank(Points/pts+ftm),
         true = ifelse(Points == pts, percent_rank(Points/pts), NA)) %>%
  ungroup() %>%
  group_by(namePlayer) %>%
  mutate(result_percentile = max(true, na.rm = TRUE)) %>%
  ggplot(aes(x=Points)) +
  geom_density(adjust = 5) +
  geom_text(aes(label = round(result_percentile, 1)), size = 3) +
  facet_wrap(facets = vars(namePlayer), scales="fixed") +
  geom_vline(aes(xintercept =  mean(pts)), color = "red") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold"))
