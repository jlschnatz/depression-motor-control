rle_id <- function(x) {
  x <- rle(x)$lengths
  rep(seq_along(x), times = x)
}

mask_inactivity <- function(data, threshold, activity_col, .groups = NULL) {
  .groupings <- enquo(.groups)
  act_col <- enquo(activity_col)
  data %>%
    dplyr::mutate(activity_bin = dplyr::if_else(
      !!act_col <= threshold, true = 0, false = 1
    )) %>% 
    dplyr::with_groups(
      .groups = !!.groupings,
      ~ dplyr::mutate(.x, id_rl = rle_id(activity_bin))
    ) %>%
    dplyr::with_groups(
      .groups = c(id_rl, !!.groupings),
      ~ dplyr::mutate(.x, consecutive_zero = length(activity_bin))
    ) %>%
    dplyr::mutate(zero_mask = if_else(
      activity_bin == 0 & consecutive_zero > threshold,
      1, 0
    )) %>%
    dplyr::select(-id_rl)
}

mask_spurious_activity <- function(data, threshold, activity_col, .groups = NULL) {
  .groupings <- enquo(.groups)
  data %>%
    dplyr::with_groups(
      .groups = !!.groupings,
      .f = ~ mutate(.x, id_rl = rle_id({{ activity_col }}))
    ) %>%
    dplyr::with_groups(
      .groups = c(id_rl, !!.groupings),
      .f = ~ dplyr::mutate(.x, consecutive_spurious = length(activity))
    ) %>%
    dplyr::mutate(spurious_mask = if_else(
      consecutive_spurious > threshold,
      1, 0
    )) %>%
    dplyr::select(-id_rl)
}

apply_mask <- function(.data, .treshold,
                       .mask = c("zero_act", "spurious_act", "both"),
                       .activity_col,
                       .groups = NULL) {
  .groupings <- enquo(.groups)
  rlang::arg_match(.mask)
  if (.mask == "zero_act") {
    df_mask <- mask_inactivity(
      .data, .treshold, {{.activity_col}},
      groups = !!.groupings
    )
    dropped <- df_mask %>%
      filter(zero_mask == 1) %>%
      select(id_participant, id_day) %>%
      distinct()
    df_filtered <- df_mask %>% 
      anti_join(dropped) %>% 
      select(-c(consecutive_zero, zero_mask,
                activity_bin))
    return(list(dropped_days = dropped, data_masked = df_filtered))
  } else if (.mask == "spurious_act") {
    df_mask <- mask_inactivity(
      .data, .treshold, {{.activity_col}},
      .groups = !!.groupings
    )
    dropped <- df_mask %>%
      filter(spurious_mask == 1) %>%
      select(id_participant, id_day) %>%
      distinct()
    df_filtered <- df_mask %>% 
      anti_join(dropped) %>% 
      select(-c(consecutive_spurious, spurious_mask,
                activity_bin))
    return(list(dropped_days = dropped, data_masked = df_filtered))
  } else if (.mask == "both") {
    df_mask <- .data %>%
      mask_inactivity(
        .treshold, {{.activity_col}}, .groups = !!.groupings
      ) %>%
      mask_spurious_activity(
        .treshold, {{.activity_col}}, .groups = !!.groupings
      )
    dropped <- df_mask %>%
      filter(spurious_mask == 1 | zero_mask == 1) %>%
      select(id_participant, id_day) %>%
      distinct()
    df_filtered <- df_mask %>% 
      anti_join(dropped %>% select(-contains("mask"))) %>% 
      select(-c(consecutive_spurious, spurious_mask,
                consecutive_zero, zero_mask,
                activity_bin))
    return(list(dropped_days = dropped, data_masked = df_filtered))
  }
}

create_day_id <- function(.data, .timestamp, .groups = NULL) { 
  .groupings <- enquo(.groups)
  create_day_id_ <- function(x) {
    assertive::assert_is_posixct(x)
    min_date <- lubridate::as_date(min(x))
    id_day <- lubridate::as_date(x)
    return(as.numeric(abs(min_date - id_day)) + 1)
  }
  out <- with_groups(
    .data = .data,
    .groups = !!.groupings,
    .f = ~mutate(.x, id_day = create_day_id_({{.timestamp}})) 
  ) 
  return(out)
}

ensure_valid_seq <- function(.data, .groups = NULL, n_minute = 1440) {
  if(!"id_day" %in% colnames(.data)) {
    stop("Error: No day identifier variable, 
         use create_day_id() to create id_day.")
  } else if (!"days_rec" %in% colnames(.data)) {
    stop("Error: No total number of recorded days (day_rec) specified")
  } else {
    .groupings <- enquo(.groups)
    .data %>% 
      filter(id_day <= days_rec) %>% 
      group_by(id_participant, id_day) %>%
      summarise(n_obs = n(), .groups = "drop") %>% 
      inner_join(.data) %>% 
      filter(n_obs == n_minute) %>%
      select(-n_obs) 
  }
}