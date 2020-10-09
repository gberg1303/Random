### Load Data
NFL_PBP <- tbl(dbConnect(SQLite(), "/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Raw NFL Seasons Data/pbp_db"),
               "nflfastR_pbp")

### Filter and Bin
NFL_PBP %>%
  filter(season > 2010, rush == 1 | pass == 1) %>%
  filter(wp > .2 & wp < .8) %>%
  filter(game_seconds_remaining > 120) %>%
  filter(!game_seconds_remaining %in% c(2820:2700)) %>%
  filter(qtr <= 4) %>%
  mutate(Case = case_when(
    ydstogo > 3 & ydstogo <= 7 ~ "Med",
    ydstogo >= 0 & ydstogo <= 3 ~ "Short",
    ydstogo > 7 ~ "Long"
  )) %>%
  mutate(
    Type = ifelse(qb_dropback == 1, "Pass", "Rush")
  ) %>%
  filter(!is.na(Type), !is.na(down)) %>%
  group_by(down, Type, Case) %>%
  summarise(
    epa = mean(epa, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Case = case_when(
      down == 1 ~ paste("First", "and", Case),
      down == 2 ~ paste("Second", "and", Case),
      down == 3 ~ paste("Third", "and", Case),
      down == 4 ~ paste("Fourth", "and", Case)
    )
  ) %>%
  group_by(Case) %>%
  filter(epa == max(epa)) %>%
  collect() %>% View()
