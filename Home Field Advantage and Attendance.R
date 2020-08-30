library(nflfastR)
library(DBI)
library(RSQLite)
library(tidyverse)
### Load Data
NFL_PBP <- tbl(dbConnect(SQLite(), "/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Raw NFL Seasons Data/pbp_db"),
               "nflfastR_pbp")

### Create Model Dataset 
Model_Data <- goldbeRg::create_nfl_modeldataset()
# Ensure Home Locations Only
Model_Data <- Model_Data %>%
  filter(location == "Home")


### Create Model
set.seed(123)
Goldberg_Model <- caret::train(win ~
                                 point_differential + adjusted_off_epa + adjusted_def_epa +
                                 opp_point_differential + opp_adjusted_off_epa + opp_adjusted_def_epa +
                                 home_team,
                               data = Model_Data %>% mutate(win = as.factor(win)),
                               method = 'glm',
                               family = "binomial",
                               preProc = c("scale"),
                               trControl = caret::trainControl(method = "repeatedcv", number = 10, repeats = 3))
