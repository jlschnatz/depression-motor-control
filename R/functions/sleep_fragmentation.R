rev_cumsum <- function(x) rev(cumsum(rev(x)))

find_longest_seq <- function(x) {
  run_length <- rle(x)
  out <- tibble(
    values = run_length$values,
    lengths = run_length$length,
    end_ind = cumsum(run_length$lengths),
    start_ind = end_ind - run_length$lengths + 1
  ) %>%
    relocate(start_ind, .before = end_ind) %>%
    filter(values == TRUE & lengths == max(lengths))
  return(out)
}

transition_prob <- function(data, activity_col, threshold = 0, id) {
  act_col <- enquo(activity_col)
  x <- data %>%
    dplyr::mutate(activity_bin = dplyr::if_else(
      !!act_col <= threshold, true = 0, false = 1
    )) %>% 
    filter(id_participant == !!enquo(id)) %>% 
    pull(activity_bin)
  
  r <- rle(x)
  df_init <- tibble(
    t = r$lengths,
    transition_from = r$values
  ) %>%
    arrange(transition_from, t) %>% 
    mutate(transition_from = factor(
      transition_from, 
      levels = c(0, 1),
      labels = c("rest", "activity")))
  
  df_out <- df_init %>%
    group_by(t, transition_from) %>%
    summarise(N_t = n(), .groups = "drop") %>%
    arrange(transition_from) %>%
    group_by(transition_from) %>%
    mutate(N_t = rev_cumsum(N_t)) %>%
    mutate(
      num = N_t - lead(N_t, n = 1),
      d = abs((lead(t, n = 1) - t)),
      denom = N_t * d,
      prob = num / denom
    ) %>%
    mutate(prob_weights = sqrt(N_t - lead(N_t, 1))) %>%
    drop_na() %>%
    select(transition_from, t, N_t, prob, prob_weights)
  
  return(df_out)
}

transition_prob_sustain_region <- function(data, frac = 0.3) {
  loess_resid <- loess(
    data = data,
    formula = prob ~ t,
    span = frac
  ) %>%
    resid() %>%
    abs() 
  
  prob_sd <- data %>%
    summarise(sd(prob)) %>%
    pull()
  is_one_sigma <- loess_resid < prob_sd
  
  index <- find_longest_seq(is_one_sigma)
  k_ra <- data %>%
    select(prob, prob_weights) %>%
    slice(index$start_ind:index$end_ind) %>%
    summarise(k_ra = mean(prob * prob_weights)) %>%
    pull(k_ra)
  return(k_ra)
}


p_ra <- function(data, activity_col, threshold = 0, id) {
  act_enquo <- enquo(activity_col)
  out <- transition_prob(data, !!act_enquo, threshold, id) %>% 
    filter(transition_from == "rest") %>% 
    ungroup() %>% 
    mutate(transition_from = as.character(transition_from))
  return(out)
}

p_ar <- function(data, activity_col, threshold = 0, id) {
  act_enquo <- enquo(activity_col)
  out <- transition_prob(data, !!act_enquo, threshold, id) %>% 
    filter(transition_from == "activity") %>% 
    ungroup() %>% 
    mutate(transition_from = as.character(transition_from))
  return(out)
}


k_ra <- function(data, activity_col, threshold = 0, frac = 0.3, id) {
  act_enquo <- enquo(activity_col)
  pRA <- p_ra(
    data = data, activity_col = !!act_enquo, 
    threshold = threshold, id = id
  )
  transition_prob_sustain_region(pRA, frac)
}

k_ar <- function(data, activity_col, threshold = 0, frac = 0.3, id) {
  act_enquo <- enquo(activity_col)
  pAR <- p_ar(
    data = data, activity_col = !!act_enquo, 
    threshold = threshold, id = id
  )
  transition_prob_sustain_region(pAR, frac)
}
