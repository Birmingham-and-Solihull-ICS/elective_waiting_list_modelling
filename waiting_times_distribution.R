library(tidyverse)


# Quantiles of the distribution
exp_dt <-data.frame(
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

a<- exp_dt %>% 
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
