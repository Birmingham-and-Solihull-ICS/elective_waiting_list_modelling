library(extrafont)
loadfonts(device = "win")
library(NHSRwaitinglist)
library(tidyverse)
library(scales)




#import WL statistics

wL_dt <-
tibble::tribble(
                              ~Specialty, ~IP_Activity, ~OP_Proc_Activity, ~Total_Proc_Activity, ~IP_Waiters, ~OP_Waiters, ~OP_Proc_Proxy_Waiters, ~Total_Proxy_Waiters,
  "110 - Trauma and Orthopaedic Service",        2885L,             2675L,                5560L,        225L,       2491L,                   211L,                 436L,
            "140 - Oral Surgery Service",        1503L,               92L,                1595L,        240L,       1921L,                    63L,                 303L,
         "100 - General Surgery Service",         814L,               74L,                 888L,         77L,        221L,                     9L,                  86L,
             "502 - Gynaecology Service",         374L,             2205L,                2579L,          6L,        251L,                    73L,                  79L,
     "120 - Ear Nose and Throat Service",         203L,             6346L,                6549L,          0L,         31L,                    18L,                  18L,
        "107 - Vascular Surgery Service",         212L,                0L,                 212L,         13L,         23L,                     0L,                  13L,
                 "101 - Urology Service",          75L,              463L,                 538L,          4L,         33L,                     8L,                  12L,
          "108 - Spinal Surgery Service",          41L,                0L,                  41L,          6L,        110L,                     0L,                   6L,
        "301 - Gastroenterology Service",         462L,               55L,                 517L,          0L,          2L,                     0L,                   0L
  )



# target mean waiting times.

mean_wt <- data.frame(
  target = c(0.65, 0.85, 0.92, 0.95)
)

mean_wt$target_mean_wait <- calc_target_mean_wait(18, qexp(mean_wt$target))

calc_target_mean_wait(52, 13)

pexp(4, lower)

# Target Weekly Demand: demand * mean_wt$target


# Calculate target capacity

targets <-
  wL_dt |> 
  select(Specialty, Total_Proc_Activity, Total_Proxy_Waiters) |> 
  mutate(target_capacity_65 = calc_target_capacity(Total_Proc_Activity/52, 18, qexp(0.65)),
         target_capacity_85 = calc_target_capacity(Total_Proc_Activity/52, 18, qexp(0.85)),
         target_capacity_92 = calc_target_capacity(Total_Proc_Activity/52, 18, qexp(0.92)),
         target_capacity_95 = calc_target_capacity(Total_Proc_Activity/52, 18, qexp(0.95)))




targets <- 
  targets |> 
  mutate(target_queue65 = calc_target_queue_size(demand = Total_Proc_Activity/52, 18, qexp(0.65)),
         target_queue85 = calc_target_queue_size(demand = Total_Proc_Activity/52, 18, qexp(0.85)),
         target_queue92 = calc_target_queue_size(demand = Total_Proc_Activity/52, 18, qexp(0.92)),
         target_queue95 = calc_target_queue_size(demand = Total_Proc_Activity/52, 18, qexp(0.95)))


109.6 + ((2 * (1 + (4 * qexp(0.65))))
         /
           18)

targets <- 
  targets |> 
  mutate(wl_growth_capacity65 = target_queue65 - Total_Proxy_Waiters,
         wl_growth_capacity85 = target_queue85 - Total_Proxy_Waiters,
         wl_growth_capacity92 = target_queue92 - Total_Proxy_Waiters,
         wl_growth_capacity95 = target_queue95 - Total_Proxy_Waiters)


# # Manual version, replaced with relief capacity
# targets <-
#   targets |> 
#   mutate(reduce_capacity65 = (1-((wl_growth_capacity65 / 52) / target_capacity_65)) * target_capacity_65,
#          reduce_capacity85 = (1-((wl_growth_capacity85 / 52) / target_capacity_85)) * target_capacity_85,
#          reduce_capacity92 = (1-((wl_growth_capacity92 / 52) / target_capacity_92)) * target_capacity_92,
#          reduce_capacity95 = (1-((wl_growth_capacity95 / 52) / target_capacity_95)) * target_capacity_95)


targets <-
  targets |> 
  mutate(reduce_capacity65 = calc_relief_capacity(Total_Proc_Activity/52, Total_Proxy_Waiters, target_queue65, 52),
         reduce_capacity85 = calc_relief_capacity(Total_Proc_Activity/52, Total_Proxy_Waiters, target_queue85, 52),
         reduce_capacity92 = calc_relief_capacity(Total_Proc_Activity/52, Total_Proxy_Waiters, target_queue92, 52),
         reduce_capacity95 = calc_relief_capacity(Total_Proc_Activity/52, Total_Proxy_Waiters, target_queue95, 52))
         


# set up current starting wait list
wl_setup <- list(NROW(targets))

for (i in seq(NROW(targets))) {
  wl_setup[[i]] <-
  data.frame(
    Referral = rep(as.Date("2024-03-31"), targets$Total_Proxy_Waiters[i])
    , Removal = rep(as.Date(NA), targets$Total_Proxy_Waiters[i])
  )
}




# Only a single day in our waiting list so we don't need `tail()` this time
#wl_queue_size(current_wl1)


#initilise list to collect results
sims <- list(NROW(targets))

for (i in #seq(NROW(targets))
     1) {
  sims[[i]] <-
    wl_simulator(start_date = '2025-04-01'
                 , end_date = '2026-03-31'
                 , demand = targets$Total_Proc_Activity[i]/52
                 , capacity = targets$reduce_capacity65[i]
                 , waiting_list = wl_setup[[i]]
                 )
}

wl_queue_size(sims[[1]])

sims_wls <- lapply(sims, wl_queue_size)

wl_queue_size(sims[[1]])

sims_wls <- bind_rows(sims_wls, .id = "column_label")

sims_wls$column_label <- factor(sims_wls$column_label, levels = as.character(seq(9)),
                                labels = targets$Specialty)

sims_wls |> 
  #left_join(targets, by = c("column_label" = "Specialty")) |> 
  #filter(column_label == "110 - Trauma and Orthopaedic Service") |> 
  ggplot(aes(y = queue_size, x = dates)) +
  geom_line() +
  scale_x_date(limits = as.Date(c("2025-04-01", "2026-04-01"))) +
  geom_hline(aes(yintercept = target_queue65), col="red"
           , data = filter(targets, Specialty == "110 - Trauma and Orthopaedic Service")) +
  theme(text = element_text(family = "Open Sans"))
  # facet_wrap(~column_label)




sim_func <- function(run_id) {
  sims <- list(NROW(targets))
  
  for (i in seq(NROW(targets))) {
    sims[[i]] <-
      cbind(
        wl_simulator(start_date = '2025-04-01'
                   , end_date = '2026-03-31'
                   , demand = targets$Total_Proc_Activity[i]/52
                   , capacity = targets$reduce_capacity65[i]
                   , waiting_list = wl_setup[[i]]
        ), column_label = i
      )
      
  }
  
  # index_q <- function(x) {
  #   
  #   out <- cbind(wl_queue_size(x), x$column_label)
  # }
  
  sims_wls <- lapply(sims, wl_queue_size)
  
  sims_wls <- bind_rows(sims_wls, .id = "column_label")
  
  #sims_wls <- do.call("rbind", sims_wls)
  
  
}

# sequence to iterate over
run_sequence <- 1:100
# run mc sim
raised_capacity_wl_mc <- lapply(run_sequence, sim_func)


#raised_capacity_wl_mc[


# sims_wls <- bind_rows(sims_wls, .id = "column_label")
# 
# sims_wls$column_label <- factor(sims_wls$column_label, levels = as.character(seq(9)),
#                                 labels = targets$Specialty)

#lift_to_df_list  <- do.call("rbind", sim_func)

#lift_to_df_list <- lapply(raised_capacity_wl_mc, bind_rows, .id = "column_label")

#lift_to_df_list[[1]]

#sims_wls2 <- bind_rows(lift_to_df_list , .id = "runid")

# Pull up to data frame from list
sims_wls2 <- bind_rows(raised_capacity_wl_mc , .id = "runid")

# rename the list index as the specialty
sims_wls2$column_label <- factor(sims_wls2$column_label, levels = as.character(seq(9)),
                                labels = targets$Specialty)

sims_wls2 <- rename(sims_wls2, "Specialty" = "column_label")

# Calculate point-wise mean WL size
mc_agg <- 
  sims_wls2 |> 
  group_by(Specialty, dates) |> 
  summarise(mean_q = mean(queue_size))


# mc_agg <-
#   aggregate(
#     queue_size ~ dates + Specialty
#     , data = sims_wls2
#     , FUN = \(x) {
#       c(mean_q = mean(x),
#         median_q = median(x),
#         lower_95CI = mean(x) -  (1.96 * (sd(x) / sqrt(length(x)))),
#         upper_95CI = mean(x) +  (1.96 * (sd(x) / sqrt(length(x)))),
#         q_25 = quantile(x, .025, names = FALSE),
#         q_75 = quantile(x, .975, names = FALSE))
#     }
#   )
# mc_agg <- data.frame(dates = as.Date(mc_agg$dates), Specialty =  mc_agg$Specialty , unlist(mc_agg$queue_size))

# Plot
sims_wls2 |> 
  #left_join(targets, by = c("column_label" = "Specialty")) |> 
  #filter(column_label == "110 - Trauma and Orthopaedic Service") |> 
  ggplot(aes(y = queue_size, x = dates, col = Specialty)) +
  geom_line(show.legend = FALSE, alpha = 0.4) +
  geom_line(aes(y=mean_q), show.legend = FALSE, data = mc_agg) +
  geom_hline(aes(yintercept = target_queue65), col = "black", linetype = "dashed"
             , data = targets) +
  scale_x_date(limits = as.Date(c("2025-04-01", "2026-04-01")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels=comma)+
  labs(title = "Simulated waiting list growth for ISP procedures in 2025/26",
       subtitle = "black dashed lines are target waiting list sizes for 65% seen within 18 weeks",
       caption = "funnels represent 100 replicates with average line plotted",
       y = "Queue size",
       x = "Date")+
 facet_wrap(~Specialty, scales = "free_y")+
  theme(text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Open Sans", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic"))
