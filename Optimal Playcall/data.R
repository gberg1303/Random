library(shiny)
library(shinythemes)
library(tidyverse)
library(nflfastR)
library(readr)
library(nflfastR)
library(DBI)
library(RSQLite)
library(DT)
library(tidyverse)


games <- tbl(dbConnect(SQLite(), "/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Raw NFL Seasons Data/pbp_db"),
             "nflfastR_pbp") %>% filter(season >= 2010, !is.na(posteam), !is.na(home_team)) %>% collect()