library(tidyverse)
library(ggplot2)
library(ggimage)

larry_data <- readr::read_csv("/Users/jonathangoldberg/Downloads/larry data.csv")

larry_data %>%
  pivot_longer(
    -Type,
    names_to = "bucket",
    values_to = "count"
  ) %>%
  filter(Type == "total") %>%
  mutate(n = row_number(),
         curve = c(.05, .15, .20, .30, .20, .10))%>%
  mutate(bucket = factor(bucket, levels = bucket[order(n)])) %>% 
  ggplot(aes(x = bucket, y = count, fill = -count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=round(count, 2)), position=position_dodge(width=0.9), vjust=-0.25, size = 4) +
  geom_text(aes(label=round(curve, 2)), color = "red", vjust=-1.4)+
  geom_smooth(aes(x = as.numeric(bucket), y = curve), se = FALSE, color = "red") +
  scale_y_continuous(expand = c(0, .05)) +
  theme_minimal() + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 28), 
        axis.text.x = element_text(angle = 10, size = 9),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  labs(
    x = "Quota",
    y = "Percentage of Active Reps in Perf Buckets",
    title = "June YTD Sales Bookings Quota Performance Distrobution"
  ) +
  ggsave("/Users/jonathangoldberg/Downloads/larry_graph.jpeg", device = "jpeg")


  