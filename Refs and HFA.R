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
get_official_data <- function(){
  
  
  
}