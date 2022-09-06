library(tidyverse)
library(vroom)
library(here)
library(glue)

# motor activity data
motor_activity_data <- here("data/raw") %>%
  list.files(
    pattern = "(condition|control).*\\.csv$",
    full.names = TRUE,
    recursive = TRUE
  ) %>%
  set_names() %>%
  map_dfr(vroom, .id = "id_participant_group") %>%
  mutate(id_participant_group = str_extract(
    id_participant_group,
    "[\\w-]+\\.[\\w-]*$"
  )) %>%
  select(-date) %>%
  mutate(id_participant_group = str_remove(
    id_participant_group,
    "\\.csv$")
    ) %>%
  separate(
    id_participant_group,
    into = c("id_group", "id_participant"),
    sep = "_"
  ) %>%
  mutate(
    id_participant = as.numeric(id_participant),
    id_participant = if_else(
      id_group == "condition",
      id_participant + 32,
      id_participant
      ),
    id_participant = as.character(id_participant),
    id_participant = str_pad(id_participant, pad = "0", width = 2)
  ) %>%
  relocate(id_participant, .before = everything()) %>%
  arrange(id_participant)

# madrs data
madrs_data <- here("data/raw") %>%
  list.files(
    pattern = ".*\\.csv$",
    full.names = TRUE
  ) %>%
  read_csv(file = .) %>%
  separate(
    number,
    into = c("id_group", "id_participant"),
    sep = "_"
  ) %>%
  mutate(
    id_participant = as.numeric(id_participant),
    id_participant = if_else(
      id_group == "condition",
      id_participant + 32,
      id_participant
      ),
    id_participant = as.character(id_participant),
    id_participant = str_pad(id_participant, pad = "0", width = 2)
  ) %>%
  mutate(
    gender = factor(
      gender,
      levels = c(1, 2),
      labels = c("female", "male")
      ),
    age = factor(age),
    afftype = factor(
      afftype,
      levels = c(1:3),
      labels = c("bipolar II", "unipolar depressive", "bipolar I")
    ),
    melanch = factor(
      melanch,
      levels = c(1, 2),
      labels = "melancholia", "no melancholia"
      ),
    inpatient = factor(
      inpatient,
      levels = c(1, 2),
      labels = c("inpatient", "outpatient")
      ),
    edu = factor(edu),
    marriage = factor(
      marriage,
      levels = c(1, 2),
      labels = c("married or cohabiting", "singe")
    ),
    work = factor(
      work,
      levels = c(1, 2),
      labels = c("working or studying", "unemployed/sick/leave/pension")
      )
  ) %>%
  rename(madrs_started = madrs1, madrs_stopped = madrs2) %>%
  arrange(id_participant)

# combined data
combined_data <-
  inner_join(
    motor_activity_data,
    madrs_data,
    by = c("id_participant", "id_group")
  ) %>%
  mutate(
    id_group = factor(id_group, levels = c("control", "condition")),
    id_participant = factor(id_participant)
  )

# save all data as seperate .rds files
list(
  motor_activity_data,
  madrs_data,
  combined_data
) %>%
  set_names(
    "motor_activity_data",
    "madrs_data",
    "combined_data"
  ) %>%
  map2(
    .x = ., .y = names(.),
    ~ write_rds(.x,
      file = here("data/processed", glue("{.y}.rds")),
      compress = "gz"
    )
  )
