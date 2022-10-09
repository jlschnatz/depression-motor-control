library(tidyverse)
library(here)
library(lubridate)
library(scico)
library(sysfonts)
library(showtext)

showtext_auto()
showtext_opts(dpi = 600)
font_add_google("Noto Sans", "noto-sans")

here("data/processed") %>%
  list.files(
    pattern = "^combined_data\\.rds$",
    full.names = TRUE) %>%
  map(.x = ., ~ read_rds(.x)) %>%
  set_names(nm = "combined_data") %>%
  iwalk(~ assign(..2, ..1, envir = .GlobalEnv))

combined_data %>%
  select(contains("id"), timestamp, activity) %>%
  mutate(
    weekday = factor(weekdays(timestamp)),
    hour = factor(hour(timestamp)),
    id_group = fct_relabel(id_group, str_to_title)
  ) %>%
  group_by(weekday, hour, id_group) %>%
  summarise(avg_act = mean(activity), .groups = "drop") %>%
  mutate(weekday = fct_relevel(
    weekday,
    c(
      "Monday", "Tuesday", "Wednesday",
      "Thursday", "Friday", "Saturday", "Sunday"
    )
  )) %>%
  ggplot(aes(
    x = hour,
    y = fct_rev(weekday),
    fill = avg_act
  )) +
  geom_tile(color = NA) +
  facet_wrap(
    ~id_group,
    nrow = 2,
  ) +
  labs(x = "Hour") + 
  scale_fill_scico(
    palette = "acton", 
    direction = 1,
    name = "Average \nactivity",
    breaks = seq(0, 400, 100),
    limits = c(0, 400)) + 
  guides(
    fill = guide_colorbar(title.vjust = 1)
  ) +
  theme(
    text = element_text(family = "noto-sans"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(color = NA, fill = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 10, color = "black"),
    axis.title.x = element_text(margin = margin(t = 7)),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(
      size = 10,
      margin = margin(b = 7),
      color = "black"
    ),
    legend.text = element_text(size = 9)
  )

ggsave(
  here("results", "weekday-hour_activity_comparison.tiff"),
  width = 7,
  height = 6,
  dpi = 600,
  compression = "lzw",
  bg =  "white"
)
