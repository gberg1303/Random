NFL_PBP %>%
  filter(!is.na(epa), !is.na(posteam), play_type=="no play" | play_type=="pass" | play_type=="run") %>%
  group_by(posteam, season)%>%
  summarize(
    n_pass=sum(pass),
    n_rush=sum(rush),
    epa_per_pass=sum(epa*pass)/n_pass,
    epa_per_rush=sum(epa*rush)/n_rush,
    success_per_pass=sum(pass*epa>0)/n_pass,
    success_per_rush=sum(rush*epa>0)/n_rush,
    off_epa=mean(epa)) %>%
  merge(x = ., y = NFL_PBP %>%
          filter(!is.na(epa), !is.na(posteam), play_type=="no play" | play_type=="pass" | play_type=="run") %>%
          group_by(defteam, season) %>% 
          summarise(def_n_pass=sum(pass),
                    def_n_rush=sum(rush),
                    def_epa_per_pass=sum(epa*pass)/def_n_pass,
                    def_epa_per_rush=sum(epa*rush)/def_n_rush,
                    def_success_per_pass=sum(pass*epa>0)/def_n_pass,
                    def_success_per_rush=sum(rush*epa>0)/def_n_rush,
                    def_epa=mean(epa)), by.x = c("posteam", "season"), by.y = c("defteam", "season"), all.x = TRUE) %>%
  merge(x = ., y = nfl_logos_df, by.x = "posteam", by.y = "team_code", all.x = TRUE) %>% 
  merge(x = ., y = teamcolors %>% select(name, primary), by.x = "team", by.y = "name", all.x = TRUE) %>%
  mutate(complete_epa = off_epa*.67-def_epa*.33) %>%
  mutate(compelte_epa_rank = rank(complete_epa)) %>% 
  filter(compelte_epa_rank <= 20) %>%
  ggplot(aes(x = off_epa, y = def_epa)) +
  geom_text_repel(aes(label = season), box.padding = 1) +
  geom_image(aes(image = url), size = 0.05) +
  geom_hline(aes(yintercept = mean(off_epa)), size = .2) +
  geom_vline(aes(xintercept =  mean(def_epa)), size = .2) +
  labs(x = "Offense epa/play",
       y = "Defense epa/play",
       caption = "Jonathan Goldberg | Data from nflscrapR",
       title = paste0("Worst Teams of the Decade")) +
  geom_abline(slope=-1.6, intercept=.4, alpha=.2) +
  geom_abline(slope=-1.6, intercept=.3, alpha=.2) +
  geom_abline(slope=-1.6, intercept=0, alpha=.2) +
  geom_abline(slope=-1.6, intercept=.1, alpha=.2) +
  geom_abline(slope=-1.6, intercept=.2, alpha=.2) +
  geom_abline(slope=-1.6, intercept=-.1, alpha=.2) +
  geom_abline(slope=-1.6, intercept=-.2, alpha=.2) +
  geom_abline(slope=-1.6, intercept=-.3, alpha=.2) +
  theme_bw()+
  scale_y_reverse() +
  theme(
    legend.position = "none", 
    plot.title = element_text(face = "bold"), 
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"))
