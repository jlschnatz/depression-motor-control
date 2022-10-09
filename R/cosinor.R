
id_subject_group <- 
  processed_data %>% 
  select(id_participant, id_group) %>%
  distinct()

cosinor <- 
  processed %>% 
  filter(!id_day_num > 12) %>%
  dplyr::mutate(hour = hour(timestamp)) %>%
  group_by(id_participant, id_group, id_day_num, hour) %>%
  dplyr::summarise(avg_act = mean(activity)) %>%
  ungroup() %>%
  cosinor(
    angle = "hour",
    x = "avg_act",
    code = "id_participant",
    data = .,
    hours = 12 * 24,
    opti = FALSE
  ) %>%
  as.data.frame() %>%
  rownames_to_column(var = "id_participant") %>%
  as_tibble() %>%
  mutate(id_participant = factor(id_participant)) %>%
  inner_join(x = ., y = id_subject_group, by = "id_participant") %>%
  relocate(id_group, .after = id_participant)

cosinor %>%
  group_by(id_group) %>%
  dplyr::summarise(across(
    .cols = avg_act.phase:avg_act.intercept,
    .fns = mean
  ))



test.model <- 
  processed %>% 
  filter(!id_day_num > 12) %>%
  mutate(hour = hour(timestamp))  %>%
  group_by(id_participant, id_group,  hour) %>%
  dplyr::summarise(avg_act = mean(activity)) %>%
  ungroup() %>%
  as.data.frame() %>%
  create.cosinor.param(
    time = "hour",
    period = 24,
    data = .)


f1.test <- fit.cosinor.mixed(
  y = "avg_act",
  x = "id_group",
  random = "1|id_participant",
  data = test.model
) 

f1.test %>% summary()

#emm_options(pbkrtest.limit = 20000)

db.means <- get.means.ci.cosinor(
  f1.test, 
  # ncpus = 10,
  contrast.frm = "~id_group",
  nsim = 300) 

db.delta <- get.contrasts.ci.cosinor(
  f1.test, 
  # ncpus = 10,
  contrast.frm = "~id_group",
  nsim = 300
)



p <- ggplot.cosinor.lmer(
  object=f1.test,
  x_str="id_group",
  period=24,
  db.means=db.means,
  DATA=test.model)

p + labs(x="Time (hours)", y="HRV SDNN, (ms)")+
  scale_color_manual(
    values=c(Man="pink", 
             Woman="purple"))+
  geom_hline(
    aes(yintercept=MESOR),
    linetype=2,
    color="black")+
  theme_bw()+
  facet_wrap(~id_group, nrow = 1)+
  guides(color=guide_legend(title="Gender"))+
  geom_segment(
    aes(x = T_AMP, 
        y = MESOR+0.05, 
        xend = T_AMP, 
        yend = MESOR+Amplitude-0.05),
    arrow = arrow(length = unit(0.15, "cm"),ends="both"), 
    size=0.5, 
    color="gray50",
    linetype=1)+
  geom_segment(
    aes(x = 0, 
        y =  MESOR+Amplitude+0.1, 
        xend = T_AMP, 
        yend = MESOR+Amplitude+0.1),
    arrow = arrow(length = unit(0.15, "cm")),
    size=0.5, 
    color="gray50",
    linetype=1)+
  geom_line(
    aes(color=factor(GROUP, levels = c("Man", "Woman")))
  ) 




