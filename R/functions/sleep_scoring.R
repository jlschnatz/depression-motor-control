

apply_cole_kripke <- function(data, count) {
  cole_kripke_alg <- function(x) {
    0.0033 * (106 * lag(x, 4, default = 0)  + 
                54  * lag(x, 3, default = 0)  + 
                58  * lag(x, 2, default = 0)  + 
                76  * lag(x, 1, default = 0)  + 
                230 * x                       + 
                74  * lead(x, 1, default = 0) + 
                67  * lead(x, 2, default = 0)
    )
    
    # S = 0.0033 × (1.06. × A−4 + 
    #               0.54 × A−3 + 
    #               0.58 × A−2 + 
    #               0.76 × A−1 + 
    #               2.30 × A−0 + 
    #               0.74 × A+1 + 
    #               0.67 × A+2
  }
  out <- data %>% 
    mutate(
      #   scaled_count = pmin(data %>% pull({{ count }})/100, 300),
      cole_kripke_score = cole_kripke_alg(x = data %>% pull({{ count }})), 
      sleep = if_else(cole_kripke_score < 1, "S", "W"),
    )
  return(out)
}

