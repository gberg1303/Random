library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(tidyverse)
library(ebbr)
library(mclust)
library(scales)

# group and summarize
QB_Data <- NFL_PBP %>%
  filter(pass == 1, !is.na(cpoe),!is.na(epa)) %>%
  group_by(game_id, game_date, passer_player_name) %>%
  summarise(
    Dropbacks = sum(qb_dropback),
    Attempts = sum(pass_attempt),
    Completions = sum(complete_pass),
    EPA = mean(epa, na.rm = TRUE),
    cpoe = mean(cpoe, na.rm = TRUE)
  ) %>%
  mutate(Composite = cpoe*.009+EPA*.2+.09) %>%
  mutate(Elite_Game = ifelse(Composite >= quantile(Composite, .9, na.rm=TRUE), 1, 0)) %>%
  filter(!is.na(passer_player_name)) %>%
  group_by(passer_player_name) %>%
  summarise(
    Games = n(),
    Composite = mean(Composite, na.rm = TRUE),
    Elite_Game = sum(Elite_Game)
  ) 

# Create an empirical beta prior. Ebbr available from: https://github.com/dgrtwo/ebbr
# Now fit each player's actual results to the prior
QB_Data <- augment(QB_Data %>% 
                     ebb_fit_prior(Elite_Game, Games), data = QB_Data) %>% 
  arrange(-.fitted)


# Add Tiers based on fitted values
scoreThreshold <- 2.5
setTier <- function(points){
  threshold <- scoreThreshold
  if(is.na(threshold))
    threshold <- 20
  tiers <- rep(as.numeric(NA), length(points))
  tierNum <- 1
  points.order <- order(-points)
  points <- points[points.order]
  repeat{
    tiers[points >= floor(max(points[is.na(tiers)]) - threshold) & is.na(tiers)] <- tierNum
    
    if(all(!is.na(tiers)))
      break
    tierNum <- tierNum + 1
  }
  tiers[points.order] <- tiers
  return(tiers)
}
QB_Data <- QB_Data %>%
  mutate(tier = setTier(.fitted*100))

rm(scoreThreshold, clusterTier, simpleCap, setTier)


# Graph it
QB_Data %>%
  filter(Games >= 25) %>%
  mutate(passer_player_name = factor(passer_player_name, levels = passer_player_name[order(.fitted)])) %>%
  ggplot() +  
  aes(x = passer_player_name, y = .fitted, ymin = .low, ymax = .high, color = as.factor(tier)) +
  geom_errorbar(width = .3) + 
  geom_point(size = 6, color = "white") + 
  geom_text(aes(y = .fitted, label = round(.fitted, 2)), size = 3) +
  geom_text(aes(y = .high, label = passer_player_name, hjust = -.05, angle = 0), size = 3) +
  coord_flip() + 
  theme_minimal() +
  scale_colour_gdocs() +
  theme(legend.position = "none", 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold")) +
  labs(y = "'True' Elite Game Rate",
       title = paste0(min(NFL_PBP$season), "-", max(NFL_PBP$season), " 'True' Elite Game Rate for NFL Quarterbacks"))

