library(tidyverse)
library(here)
library(lubridate)
library(slider)
library(scico)
library(rstatix)
library(sysfonts)
library(showtext)

showtext_auto()
showtext_opts(dpi = 600)

font_add_google("Noto Sans", "noto-sans")

here("data/processed") %>%
  list.files(pattern = "\\.rds$", full.names = TRUE) %>%
  map(.x = ., ~ read_rds(.x)) %>%
  set_names(nm = c(
    "combined_data",
    "madrs_data",
    "motor_activity_data"
  )) %>%
  imap(~ assign(..2, ..1, envir = .GlobalEnv))


combined_data %>%
  select(contains("id"), timestamp, activity) %>%
  mutate(
    weekday = factor(weekdays(timestamp)),
    hour = factor(hour(timestamp)),
  ) %>%
  group_by(weekday, hour, id_group) %>%
  summarise(
    max_act = max(activity),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = "id_group",
    values_from = "max_act"
  ) %>%
  mutate(act_higher_control = control > condition) %>%
  pivot_longer(
    control:condition,
    names_to = "id_group",
    values_to = "max_act"
  ) %>%
  summarise(
    n = n(),
    total_higher_abs = sum(act_higher_control),
    total_higher_rel = total_higher_abs / n
  )


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



wilcox_test_effsize <- function(data, formula, paired = FALSE,
                                ci = TRUE,
                                conf_level = 0.95,
                                nboot = 1000) {
  list(
    test = rstatix::wilcox_test(
      data, formula, paired
    ),
    effsize = rstatix::wilcox_effsize(
      data, formula, paired,
      ci,
      conf.level = conf_level, nboot
    )
  )
}

set.seed(42)
wilcox_madrs <-
  madrs_data %>%
  filter(id_group == "condition") %>%
  select(id_participant, contains("madrs"), ) %>%
  pivot_longer(
    cols = -id_participant,
    names_to = "measure_time",
    values_to = "madrs_score"
  ) %>%
  wilcox_test_effsize(
    data = .,
    formula = madrs_score ~ measure_time,
    paired = TRUE,
    ci = TRUE,
    conf_level = 0.95,
    nboot = 1000
  )


madrs_data %>%
  select(id_participant, id_group, contains("madrs")) %>%
  filter(id_group == "condition") %>%
  mutate(abs_diff = abs(madrs_started - madrs_stopped)) %>%
  pivot_longer(
    cols = contains("madrs"),
    names_to = "measure_time",
    values_to = "madrs_score"
    ) %>%
summarise(avg_abs_diff = mean(abs_diff))
