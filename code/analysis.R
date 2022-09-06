library(tidyverse)
library(here)
library(lubridate)
library(slider)

here("data/processed") %>%
  list.files(pattern = "\\.rds$", full.names = TRUE) %>%
  map(.x = ., ~ read_rds(.x)) %>%
  set_names(nm = c(
    "combined_data",
    "madrs_data",
    "motor_activity_data"
  )) %>%
  imap(~ assign(..2, ..1, envir = .GlobalEnv))


processed_data <- 
combined_data %>% 
  mutate(year = year(timestamp),
         month = month(timestamp),
         day = day(timestamp),
         hour = hour(timestamp),
         minute = minute(timestamp)) 


processed_data %>%
  group_by(id_group) %>%
  summarise(avg_activity = mean(activity),
            sd_activity = sd(activity)) %>% view
