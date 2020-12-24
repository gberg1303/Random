

### Create and Save Table
table %>%
  mutate_if(is.numeric, round, 2) %>%
  dplyr::rename(
    "Home" = home_logo,
    "Away" = away_logo,
    "Adjusted EPA" = model_home_wp,
    "ESPN FPI" = espn_home_wp,
    "Caesars " = Caesars,
    "Teamrankings " = teamrankings,
    "Numberfire " = numberfire,
    "FiveThirtyEight " = fivethirtyeight_home_wp
  ) %>%
  select(Home, Away, `Adjusted EPA`, `ESPN FPI`, `FiveThirtyEight `, everything()) %>%
  gt() %>%
  tab_header(
    title = md("**NFL 2020: Game Predictions**"),
    subtitle =md( "**Week  16: Home Win Probabilities**")
  ) %>%
  tab_source_note(md("By: @gberg1303 | Data from @nflfastR, ESPN, and 538")) %>%
  tab_spanner(label = "Converted Odds", columns = matches("Caesars|Teamrankings|Numberfire")) %>%
  tab_spanner(label = "EPA", columns = matches("FPI|EPA")) %>%
  tab_spanner(label = "ELO", columns = matches("Five")) %>%
  # Create Images for Home and Away Logos
  text_transform(
    locations = cells_body(vars(Home)),
    fn = function(x) web_image(url = x, height = 50)
  ) %>%
  text_transform(
    locations = cells_body(vars(Away)),
    fn = function(x) web_image(url = x, height = 50)
  ) %>%
  # Align Center Columns for Predictions
  cols_align(columns = contains(" "),
             align = c("center")) %>%
  # Make Decimals Percentages
  fmt_percent(columns = contains(" "),
              decimals = 0) %>%
  # Add Coloring in Scale to probabilities
  data_color(
    columns = contains(" "),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL)
  ) %>%
  # Add Lines to Table
  tab_style(
    style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
    locations = list(cells_column_labels(columns = gt::everything()))
  ) %>%
  tab_style(
    style = list(cell_borders(sides = "left",color = "black", weight = px(3))),
    locations = list(cells_body(columns = vars(`Adjusted EPA`)))
  ) %>%
  cols_hide(
    columns = vars(home_team, away_team)
  ) %>%
  gtsave("/Users/jonathangoldberg/Google Drive/Random/Sports/Data Twitter Account/Football/Game Predictions/2020-2021/Week  16 Game Predictions.png")


### Merge in Logos
season_simulation %>% 
  write_csv("/Users/jonathangoldberg/Google Drive/Random/Sports/Data Twitter Account/Football/Game Predictions/2020-2021/Week  16 Season Simulations.csv") %>%
  left_join(
    nflfastR::teams_colors_logos %>% select(team_abbr, team_logo_espn),
    by = c("team" = "team_abbr")
  ) %>%
  arrange(-wins, -league_winner) %>%
  select(team_logo_espn, everything(), -team, - division_round, -wild_card, -division_winner) %>%
  dplyr::rename(
    "Team" = team_logo_espn,
    "Wins " = wins,
    "Playoff " = playoff,
    "Conf. Champ." = conference_championship,
    "Super Bowl" = super_bowl,
    "Winner " = league_winner
  ) %>%
  gt() %>%
  tab_header(
    title = md("**NFL 2020: Season Simulation**"),
    subtitle =md( "**Pre-Week  16**")
  ) %>%
  tab_source_note(md("By: @gberg13O3 | Data from @nflfastR |10,000 Simulations")) %>%
  # Create Images for Home and Away Logos
  text_transform(
    locations = cells_body(vars(Team)),
    fn = function(x) web_image(url = x, height = 50)
  ) %>%
  # Align Center Columns for Predictions
  cols_align(columns = contains(" "),
             align = c("center")) %>%
  # Add Coloring in Scale to probabilities
  data_color(
    columns = contains(" "),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL)
  ) %>%
  # Add Lines to Table
  tab_style(
    style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
    locations = list(cells_column_labels(columns = gt::everything()))
  ) %>%
  tab_style(
    style = list(cell_borders(sides = "left",color = "black", weight = px(3))),
    locations = list(cells_body(columns = vars(`Wins `)))
  ) %>%
  gtsave("/Users/jonathangoldberg/Google Drive/Random/Sports/Data Twitter Account/Football/Game Predictions/2020-2021/Week  16 Season Simulations.png")

