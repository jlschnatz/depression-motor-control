
processed %>%
  filter(!id_day_num > 12) %>%
  mutate(minute = minute(timestamp),
         hour = hour(timestamp)) %>%
  group_by(id_participant, id_group,hour, minute) %>%
  summarise(avg_by_time = mean(activity)) %>%
  group_by(id_group, hour, minute) %>%
  mutate(avg_by_group = mean(avg_by_time)) %>%
  group_by(id_participant, id_group) %>%
  mutate(id_time = row_number()) %>%
  mutate(id_group = str_to_sentence(id_group)) %>%
  ggplot(aes(x = id_time, y = avg_by_time, group = id_participant)) + 
  geom_line(alpha = 0.5, color = "grey") + 
  geom_line(
    aes(y = avg_by_group, x = id_time),
    inherit.aes = FALSE,
    size = 0.5
  ) +
  scale_x_continuous(
    limits = c(0, 1440),
    breaks = seq(0,1440,120),
    labels = seq(0,1440,120)/60,
    expand = c(0,0)
  ) + 
  scale_y_continuous(
    limits = c(0,1800),
    breaks = seq(0,1800, 300),
    expand = c(0,0)
  ) +
  labs(x = "Hour", y = "Activity\n(min. epochs)") + 
  facet_wrap(~id_group, ncol = 2) + 
  theme_bw() + 
  theme(
    axis.title = element_text(face = "bold", size = 11),
    axis.title.y = element_text(margin = margin(r = 7)),
    axis.title.x = element_text(margin = margin(t = 7)),
    panel.spacing = unit(0.7, "lines"),
    strip.background = element_rect(fill = NA, color = NA),
    strip.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(7, 7, 7, 7)
    
  )


ggsave(
  here("results", "activity_comparison_min_epoch.tiff"),
  width = 11, 
  height = 6,
  bg = "white",
  compression = "lzw"
)