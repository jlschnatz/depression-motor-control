activity_extreme_onset <- function(data,
                                   which_extreme = c("M10_onset", "L5_onset")) {
  rlang::arg_match(which_extreme)
  name <- rlang::as_name(which_extreme)
  df <- data %>%
    dplyr::mutate(hour = lubridate::hour(timestamp)) %>%
    dplyr::group_by(
      id_participant,
      id_group,
      hour
    ) %>%
    dplyr::summarise(
      avg_hour_participant = mean(activity),
      .groups = "drop_last"
    )
  if (which_extreme == "L5_onset") {
    slide_period <- 5
    output <- df %>%
      dplyr::mutate(avg_slide10_activity = slider::slide_dbl(
        .x = avg_hour_participant,
        .after = slide_period,
        .f = ~base::mean(.x)
      )) %>%
      dplyr::mutate(rank_activity = base::rank(
        avg_slide10_activity,
        ties.method = "random"
      )) %>%
      dplyr::slice_min(order_by = rank_activity, n = 1) %>%
      dplyr::select(-rank_activity) %>%
      dplyr::summarise(value = hour, .groups = "drop") %>%
      dplyr::rename(!!name := value)
  } else if (which_extreme == "M10_onset") {
    slide_period <- 10
    output <- df %>%
      dplyr::mutate(avg_slide10_activity = slider::slide_dbl(
        .x = avg_hour_participant,
        .after = slide_period,
        .f = ~base::mean(.x)
      )) %>%
      dplyr::mutate(rank_activity = base::rank(
        avg_slide10_activity,
        ties.method = "random"
      )) %>%
      dplyr::slice_max(order_by = rank_activity, n = 1) %>%
      dplyr::select(-rank_activity) %>%
      dplyr::summarise(value = hour, .groups = "drop") %>%
      dplyr::rename(!!name := value)
  }
  return(output)
}

activity_extreme <- function(data,
                             which_extreme = c("M10", "L5")) {
  rlang::arg_match(which_extreme)
  name <- rlang::as_name(which_extreme)
  df <-
    data %>%
    dplyr::mutate(hour = lubridate::hour(timestamp)) %>%
    dplyr::group_by(
      id_participant,
      id_group,
      hour
    ) %>%
    dplyr::summarise(
      avg_hour_participant = mean(activity),
      .groups = "drop_last"
    )
  if (which_extreme == "L5") {
    slide_period <- 5
    output <- df %>%
      dplyr::mutate(avg_slide10_activity = slider::slide_dbl(
        .x = avg_hour_participant,
        .after = slide_period,
        .f = base::mean
      )) %>%
      dplyr::mutate(rank_activity = base::rank(
        avg_slide10_activity,
        ties.method = "random"
      )) %>%
      dplyr::slice_min(order_by = rank_activity, n = 5) %>%
      dplyr::select(-rank_activity) %>%
      dplyr::summarise(value = base::mean(avg_hour_participant), .groups = "drop") %>%
      dplyr::rename(!!name := value)
  } else if (which_extreme == "M10") {
    slide_period <- 10
    output <- df %>%
      dplyr::mutate(avg_slide10_activity = slider::slide_dbl(
        .x = avg_hour_participant,
        .after = slide_period,
        .f = base::mean
      )) %>%
      dplyr::mutate(rank_activity = base::rank(
        avg_slide10_activity,
        ties.method = "random"
      )) %>%
      dplyr::slice_max(order_by = rank_activity, n = 10) %>%
      dplyr::select(-rank_activity) %>%
      dplyr::summarise(value = base::mean(avg_hour_participant), .groups = "drop") %>%
      dplyr::rename(!!name := value)
  }
  return(output)
}

rel_amplitude <- function(data) {
  L5 <- activity_extreme(data, which_extreme = "L5")
  M10 <- activity_extreme(data, which_extreme = "M10")
  output <- dplyr::inner_join(
    x = L5,
    y = M10,
    by = c("id_participant", "id_group")
  ) %>%
    dplyr::mutate(RA = (M10 - L5) / (M10 + L5)) %>%
    dplyr::select(contains("id"), RA)
  return(output)
}

activity_tot <- function(data) {
  output <- processed_data %>%
    dplyr::group_by(
      id_participant,
      id_group,
      id_day
    ) %>%
    dplyr::summarise(
      tot_activity_by_day = base::sum(activity),
      .groups = "drop"
    ) %>%
    dplyr::group_by(id_participant, id_group) %>%
    dplyr::summarise(
      TDA = base::mean(tot_activity_by_day),
      .groups = "drop"
    )
  return(output)
}

activity_active_hour <- function(data) {
  data %>%
    dplyr::filter(activity > 0) %>%
    dplyr::group_by(id_participant, id_group, id_day) %>%
    dplyr::summarise(
      sum_activity = base::sum(activity),
      n_active = n(),
      .groups = "drop_last"
    ) %>%
    dplyr::summarise(
      APAH = base::mean(sum_activity / n_active),
      .groups = "drop"
    )
}

inactivity_rel <- function(data) {
  data %>%
    dplyr::mutate(is_inactive = dplyr::if_else(activity == 0, TRUE, FALSE)) %>%
    dplyr::group_by(id_participant, id_group, id_day) %>%
    dplyr::summarise(
      adi_by_day = base::sum(is_inactive / 1440),
      .groups = "drop_last"
    ) %>%
    dplyr::summarise(ADI = base::mean(adi_by_day), .groups = "drop")
}

interdaily_variability <- function(data) {
  output <- data %>%
    dplyr::select(dplyr::contains("id"), timestamp, activity) %>%
    dplyr::mutate(hour = hour(timestamp)) %>%
    dplyr::group_by(
      id_participant, id_group,
      id_day, hour
    ) %>%
    dplyr::summarise(
      activity = mean(activity),
      .groups = "drop"
    ) %>%
    dplyr::group_by(id_participant, id_group) %>%
    dplyr::summarise(
      total_avg_activity = base::mean(activity),
      numerator = base::sum(base::diff(activity)^2) / (base::length(activity) - 1),
      denominator = base::sum((activity - total_avg_activity)^2) / base::length(activity),
      .groups = "keep"
    ) %>%
    dplyr::transmute(IV = numerator / denominator) %>%
    dplyr::ungroup()
  return(output)
}

interdaily_stability <- function(data) {
  output <- data %>%
    dplyr::select(dplyr::contains("id"), timestamp, activity, days_rec) %>%
    dplyr::mutate(hour = lubridate::hour(timestamp)) %>%
    dplyr::group_by(
      id_participant, id_group,
      hour, days_rec, id_day
    ) %>%
    dplyr::summarise(
      act_xi = base::mean(activity),
      .groups = "drop"
    ) %>%
    dplyr::group_by(id_participant, hour) %>%
    dplyr::mutate(act_xh = base::mean(act_xi)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id_participant, id_group, days_rec) %>%
    dplyr::mutate(act_x = base::mean(act_xi)) %>%
    dplyr::summarise(
      numerator = (1 / (days_rec * 24)) * base::sum((act_xh - act_x)^2),
      denominator = stats::var(act_xi),
      .groups = "keep"
    ) %>%
    dplyr::summarise(IS = numerator / denominator) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::select(-days_rec)
  return(output)
}