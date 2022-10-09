processed_data %>% 
  group_by(id_day, id_participant) %>% 
  mutate(id_time = row_number()) %>%
  ungroup() %>% 
  select(contains("id"), activity) %>% 
  group_by(id_time, id_group) %>% 
  summarise(
    mean_activity = mean(activity),
    .groups = "drop"
  ) %>% 
  mutate(id_group = str_to_title(id_group)) %>% 
  ggplot(aes(x = id_time, y = mean_activity, color = id_group)) + 
  geom_point(size = 0.4, alpha = .25) + 
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    se = F, size = 1.2) + 
  scale_color_manual(
    name = "Group",
    values = wesanderson::wes_palette("Royal1", 2)
  ) + 
  scale_x_continuous(
    name = "Time",
    breaks = seq(0, 1440, 120),
    labels = seq(0, 24, 2),
    expand = c(0, 0)
  ) + 
  scale_y_continuous(
    name = "Mean activity",
    breaks = seq(0, 600, 100),
    limits = c(0, 600),
    expand = c(0,0)
  ) + 
  coord_cartesian(clip = "off") +
  theme_scientific() + 
  theme(
    legend.position = c(0.12, 0.92),
    legend.title.align = 0
  )



ggsave(here("results/activity_comp.pdf"),
       width = 6, height = 5,
)