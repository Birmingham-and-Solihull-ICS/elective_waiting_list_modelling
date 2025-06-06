library(tidyverse)
library(scales)

# Quantiles of the distribution
exp_dt <-data.frame(
  `6` = qexp(p = seq(0.01,1, 0.01), rate = 1/6),
  `6` = qexp(p = seq(0.01,1, 0.01), rate = 1/6),
  `9` = qexp(p = seq(0.01,1, 0.01), rate = 1/9),
  `12` = qexp(p = seq(0.01,1, 0.01), rate = 1/12),
  `15` = qexp(p = seq(0.01,1, 0.01), rate = 1/15),
  `18` = qexp(p = seq(0.01,1, 0.01), rate = 1/18)
)




exp_dt <-
  exp_dt %>% 
  pivot_longer(everything(), names_repair = "minimal") %>% 
  mutate(`Mean waiting time` = factor(substring(name,2)
                                      , levels = c("6", "9", "12", "15", "18")
                                      ,labels = c("6 (5% > 18 weeks)"
                                                  , "9 (14% > 18 weeks)"
                                                  , "12 (23% > 18 weeks)"
                                                  , "15 (31% > 18 weeks)"
                                                  , "18 (37% > 18 weeks)" )
                                      )
         , uppertail = ifelse(value > 18, 1, 0)) 

# Count percent
exp_dt %>% 
  group_by(`Mean waiting time`, uppertail) %>% 
  count()

a <- exp_dt %>% 
  filter(name == "X6")
  

# Plot
exp_dt %>% 
  ggplot(aes(x=value, fill = `Mean waiting time`, col=`Mean waiting time`, group = `Mean waiting time`))+
  #geom_histogram(bins=50, alpha=1, position = "identity", alpha = 0.6)+
  geom_density(position = "identity", alpha = 0.3)+
  geom_vline(xintercept=18, colour = "coral")+
  scale_fill_viridis_d(aesthetics = c("fill", "colour"))+
  scale_y_continuous(labels = percent)+
  annotate("text", x=18, y=0.075, hjust=-0.2,label="18 weeks",  colour = "coral")+
  labs(title = "Distribution of waiting times by mean wait", y = "Proportion of waiting list",
       x = "Weeks")+
  theme_minimal()+
  theme(legend.position = c(0.80, 0.5),
        legend.background = element_rect(fill = "white", color = "grey"))


##########
# Tom's revision
##########

# exponential density data
distributions_df <- tibble::tibble(
  x = seq(0, 65, length.out = 1000),
  `6` = dexp(x, 1/6),
  `7.12` = dexp(x, 1/7.12),
  `9` = dexp(x, 1/9),
  `12` = dexp(x, 1/12),
  `15` = dexp(x, 1/15),
  `18` = dexp(x, 1/18)
) |> 
  pivot_longer(- x) |> 
  mutate(
    `Mean waiting time (weeks)` = factor(
      name, 
      levels = c("6", "7.12", "9", "12", "15", "18"),
      labels = c(
        "6 (5% > 18 weeks)", 
        "7.12 (8% > 18 weeks)",
        "9 (14% > 18 weeks)",
        "12 (23% > 18 weeks)", 
        "15 (31% > 18 weeks)", 
        "18 (37% > 18 weeks)"
      )
    )
  )

ggplot(distributions_df, aes(x = x), group = `Mean waiting time (weeks)`) + 
  scale_color_viridis_d(direction = -1) +
  scale_fill_viridis_d(direction = -1) +
  scale_y_continuous(labels = scales::label_percent()) +
  geom_line(aes(y = value, colour = `Mean waiting time (weeks)`), linewidth = 1) +
  geom_area(aes(y = value, fill = `Mean waiting time (weeks)`), alpha = 0.1, position = 'identity') +
  geom_vline(xintercept=18, colour = "coral")+
  annotate("text", x=18, y=0.075, hjust=-0.2,label="18 weeks",  colour = "coral")+
  theme_minimal() +
  theme(legend.position = c(0.80, 0.5),
        legend.background = element_rect(fill = "white", color = "grey")) +
  labs(
    title = "Distribution of waiting times by mean wait", 
    x = "Weeks",
    y = "Proportion of waiting list"
  )

# table of breach probabilities
# given a target waiting time, and a waiting list average wait, 
# what proportion of the list meets the target?
breach_df <- tibble::tibble(
  waiting_time_target_weeks = c(rep(18, 12), rep(4, 12)),
  mean_wait_weeks = rep(c(1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), 2),
  percentage_meeting_target = round(pexp(waiting_time_target_weeks, 1/mean_wait_weeks) * 100, 2),
  percentage_breaching_target = 100 - percentage_meeting_target
)
breach_df

# table of performance thresholds
# given an RTT performance level (eg. 65% seen by x weeks), and a waiting list average wait, 
# after how many weeks is the performance level breached
performance_df <- tibble::tibble(
  perf_target_percentage = rep(c(0.5, 0.6, 0.65, 0.7, 0.8, 0.9, 0.92, 0.95), 2) * 100,
  mean_wait = c(rep(18, 8), rep(6, 8)),
  tail_in_weeks = round(qexp(perf_target_percentage / 100, 1 / mean_wait), 1),
)
performance_df
