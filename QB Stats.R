library(ggimage, tidyverse, teamcolors, nflfastR, ggthemes, ggrepel)

NFL_PBP %>%
  filter(season >= 2020, week <= 17, !is.na(epa)) %>%
  collect() %>%
  group_by(season, id, name) %>%
  summarize(
    epa = mean(qb_epa),
    total_epa = sum(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  mutate(rank = rank(-total_epa)) %>%
  filter(rank <= 5) %>%
  arrange(-total_epa) %>% 
  merge(y = nflfastR::teams_colors_logos, by.x = "team", by.y = "team_abbr", all.x = TRUE) %>% 
  left_join(nflfastR::fast_scraper_roster(current_season) %>% 
              select(first_name, last_name, team, headshot_url) %>% 
              mutate(short_name = paste0(substr(first_name, 1, 1), ".", last_name),
                     team = str_replace(team, "LAR", "LA"),
                     team = str_replace(team, "OAK", "LV"),
                     team = str_replace(team, "WSH", "WAS"),
                     short_name = str_replace(short_name, " II", ""),
                     short_name = str_replace(short_name, " Jr.", "")), 
            by = c("name" = "short_name", "team" = "team")) %>%
  mutate(name = factor(name, levels = name[order(-total_epa)])) %>% 
  ggplot(aes(x = name, y = total_epa)) + 
  theme_minimal() +
  geom_bar(stat = "identity", size = 1, width = .5, aes(color = team_color2, fill = team_color)) +
  scale_fill_manual(values = set_names(unique(nflfastR::teams_colors_logos$team_color), as.vector(unique(nflfastR::teams_colors_logos$team_color)))) + 
  scale_color_manual(values = set_names(unique(nflfastR::teams_colors_logos$team_color2), as.vector(unique(nflfastR::teams_colors_logos$team_color2)))) + 
  geom_image(aes(image = headshot_url), size = .1, nudge_y = 1, asp = .5) +
  scale_y_continuous(expand = c(.1, 0)) +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 70, size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  labs(y = "Total EPA",
       x = "Quarterback",
       title = paste0("NFL ", current_season, ": Total QB EPA"), 
       caption = paste("@gberg1303 | @nflfastR | Formulas from @BenbBaldwin"))
