team_logs %>%
  group_by(locationGame) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  select(locationGame, fg3mTeam, ptsTeam, astTeam, stlTeam, blkTeam)
 