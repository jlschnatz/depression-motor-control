library(here)
library(tidyverse)
library(lubridate)
source(here("R/functions.R"))

here("data/processed") %>%
  list.files(
    pattern = "^combined_data\\.rds$",
    full.names = TRUE
  ) %>%
  map(.x = ., ~ read_rds(.x)) %>%
  set_names(nm = "combined_data") %>%
  iwalk(~ assign(..2, ..1, envir = .GlobalEnv))

processed_data <- combined_data %>% 
  create_day_id(
    .data = .,
    .timestamp = timestamp, 
    .groups = c(id_participant, id_group)
    ) %>%
   relocate(id_day, .after = id_group) %>%
   ensure_valid_seq(
     .data = .,
     .groups = c(id_participant, id_day),
     n_minute = 1440
     ) %>%
  apply_mask(
    .data = ., .treshold = 240,
    .mask = "both",
    .activity_col = activity,
    .groups = c(id_participant, id_day)
    ) %>% 
  pluck("data_masked")
  
glimpse(processed_data)

write_rds(
  here("data/processed/processed_data.rds"),
  x = processed_data,
  compress = "gz"
)

