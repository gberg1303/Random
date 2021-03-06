library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(tidyverse)
library(ebbr)
library(mclust)
library(scales)

##### True Composite Grade does not work because the formula is based on attepmts and successes.

# group and summarize 
QB_Data <- NFL_PBP %>%
  filter(pass == 1, !is.na(cpoe),!is.na(epa), season > 2010) %>%
  group_by(passer_player_name) %>%
  summarise(
    Dropbacks = sum(qb_dropback),
    Attempts = sum(pass_attempt),
    Completions = sum(complete_pass),
    EPA = mean(epa, na.rm = TRUE),
    cpoe = mean(cpoe, na.rm = TRUE),
    touchdowns = sum(touchdown)
  ) %>%
  mutate(Composite = cpoe*.009+EPA*.2+.09) %>%
  filter(!is.na(passer_player_name)) %>% collect()

# Create an empirical beta prior. Ebbr available from: https://github.com/dgrtwo/ebbr
# Now fit each player's actual results to the prior
QB_Data <- augment(QB_Data %>% 
                    ebb_fit_prior(Completions, Attempts), data = QB_Data) %>% 
  arrange(-.fitted)

# Add Tiers based on fitted values
scoreThreshold <- 1
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
  filter(Dropbacks >= 300) %>%
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
  labs(y = "'True' Completion Percentage",
       title = paste0("'True' Completion Percentage for NFL Quarterbacks"))

