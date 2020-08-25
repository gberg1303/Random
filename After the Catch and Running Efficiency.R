### Load Data
NFL_PBP <- tbl(dbConnect(SQLite(), "/Users/jonathangoldberg/Google Drive/Random/Sports/Fantasy Football/Data/Raw NFL Seasons Data/pbp_db"),
               "nflfastR_pbp")

### RB Data
RB_Data <- NFL_PBP %>%
  filter(rush == 1, !is.na(epa)) %>%
  group_by(rusher_player_name, rusher_player_id, season) %>%
  summarise(
    epa = mean(epa),
    carries = n(),
    ypc = mean(yards_gained)
  )

### After the Catch Data YAC_Data <- 
YAC_Data <- NFL_PBP %>%
  filter(complete_pass == 1, !is.na(epa)) %>%
  group_by(receiver_player_name, receiver_player_id, season) %>%
  summarise(
    yac_over_expected = yards_after_catch - xyac_mean_yardage,
    receptions = n(),
  )

### Merge
RB_Data <- RB_Data %>%
  left_join(
    YAC_Data,
    by = c("rusher_player_name" = "receiver_player_name", "rusher_player_id" = "receiver_player_id", "season")
  ) %>%
  filter(!is.na(rusher_player_name), carries > 100)

### Get some correlations and other summary statistics
summary(lm(data = RB_Data, epa ~ yac_over_expected))

### Plot it
RB_Data %>%
  ggplot(aes(x = yac_over_expected, y = epa)) +
  geom_point() +
  geom_smooth(se = FALSE)
