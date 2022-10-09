library(plyr)
library(tidyverse)
library(here)
library(lubridate)
library(glue)
library(rlang)
library(slider)
source(here::here("R/functions.R"))

here::here("data/processed") %>%
  list.files(
    pattern = "^processed_data\\.rds$",
    full.names = TRUE
  ) %>%
  map(.x = ., ~ read_rds(.x)) %>%
  set_names(nm = "processed_data") %>%
  iwalk(~ assign(..2, ..1, envir = .GlobalEnv))

.fns <- list(
  L5 = activity_extreme,
  L5_onset = activity_extreme_onset,
  M10 = activity_extreme,
  M10_onset = activity_extreme_onset,
  TDA = activity_tot,
  APAH = activity_active_hour,
  ADI = inactivity_rel,
  RA = rel_amplitude,
  IS = interdaily_stability,
  IV = interdaily_variability
)

.args <- list(
  list(which_extreme = "L5"),
  list(which_extreme = "L5_onset"),
  list(which_extreme = "M10"),
  list(which_extreme = "M10_onset"),
  list(), list(), list(),
  list(), list(), list()
)

features <- map2(
  .x = .args, .y = .fns,
  .f = ~ exec(.y, processed_data, !!!.x)
) %>%
  join_all(by = c("id_participant", "id_group")) %>%
  as_tibble()



