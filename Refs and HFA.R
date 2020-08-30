### Load NFL FastR Data### NFL PBP Data
library(nflfastR)
library(DBI)
library(RSQLite)
library(tidyverse)
update_db(
  dbdir = "/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Raw NFL Seasons Data",
  dbname = "pbp_db",
  tblname = "nflfastR_pbp",
  force_rebuild = FALSE
)
NFL_PBP <- tbl(dbConnect(SQLite(), "/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Raw NFL Seasons Data/pbp_db"),
               "nflfastR_pbp")

### Load Referee Data
Game_IDs <- purrr::map_df(2015:2019, function(x) espnNFLscraper::get_game_ids(season = x, season_type = "regular"))
Ref_Data <- espnNFLscraper::get_game_details(espn_gameids = Game_IDs$espn_gameid)
Ref_Data <- get_game_details(espn_gameids = espn_nfl_ids %>% filter(season == 2019 & season_type == 2) %>% pull(espn_gameid))
