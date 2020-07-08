PBP_Complete_Seasons %>% 
  filter(PlayType == "Field Goal") %>%
  drop_na(FieldGoalDistance) %>%
  mutate(FG_Length = cut(FieldGoalDistance, breaks = c(0, 10, 20, 30, 40, 50, 60, 70))) %>%
  group_by(FG_Length) %>%
  summarise(EPA = mean(EPA, na.rm = TRUE))

PBP_Complete_Seasons %>% 
  filter(PlayType == "Field Goal") %>%
  filter(FieldGoalResult == "Good") %>%
  drop_na(FieldGoalDistance) %>%
  mutate(FG_Length = cut(FieldGoalDistance, breaks = c(0, 10, 20, 30, 40, 50, 60, 70))) %>%
  group_by(FG_Length) %>%
  summarise(EPA = mean(EPA, na.rm = TRUE))

PBP_Complete_Seasons %>% 
  filter(PlayType == "Field Goal") %>%
  filter(FieldGoalResult != "Good") %>%
  drop_na(FieldGoalDistance) %>%
  mutate(FG_Length = cut(FieldGoalDistance, breaks = c(19, 29, 39, 49, 59, 69, 70), labels = c("0-19", "20-29", "30-39", "40-49", "51-59", "60-69"))) %>%
  group_by(FG_Length) %>%
  summarise(EPA = mean(EPA, na.rm = TRUE))

tab_dfs(list(PBP_Complete_Seasons %>% 
         filter(PlayType == "Field Goal") %>%
         filter(!is.na(FieldGoalDistance)) %>%
           mutate(FG_Length = cut(FieldGoalDistance, breaks = c(0, 19, 29, 39, 49, 59, 100))) %>%
           group_by(FG_Length) %>%
           summarise(EPA = round(mean(EPA, na.rm = TRUE), 2)), 
       PBP_Complete_Seasons %>% 
         filter(PlayType == "Field Goal") %>%
         filter(FieldGoalResult == "Good") %>%
         filter(!is.na(FieldGoalDistance)) %>%
         mutate(FG_Length = cut(FieldGoalDistance, breaks = c(0, 19, 29, 39, 49, 59, 100), labels = FALSE)) %>%
         group_by(FG_Length) %>%
         summarise(EPA = round(mean(EPA, na.rm = TRUE), 2)), 
       PBP_Complete_Seasons %>% 
         filter(PlayType == "Field Goal") %>%
         filter(FieldGoalResult != "Good") %>%
         filter(!is.na(FieldGoalDistance)) %>%
         mutate(FG_Length = cut(FieldGoalDistance, breaks = c(0, 19, 29, 39, 49, 59, 100), labels = FALSE)) %>%
         group_by(FG_Length) %>%
         summarise(EPA = round(mean(EPA, na.rm = TRUE), 2))),
       titles = c("All Field Goals", "Good Field Goals", "Bad Field Goals"),
       file = "/Users/jonathangoldberg/Downloads/FieldGoalEPA.html")
