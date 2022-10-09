library(tidyverse)
library(here)
library(lubridate)
library(slider)
library(rstatix)

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
