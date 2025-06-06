library(NHSRwaitinglist)

# Catacts ISPs
# Average wait is ~2 weeks 
# Annual activity: 15,110
# weekly activity = 291
# Target is 65% within 18 weeks


func_wlcalc <- function(annual_activity, waiting_list_current = 0, target = 18, target_percent=0.65, growth_period_years=1){
  
  weekly_activity <- annual_activity/52
  
  target_q <- NHSRwaitinglist::calc_target_queue_size(weekly_activity, target, qexp(target_percent))
  
  afforded_growth <- target_q - waiting_list_current
  
  weekly_growth <- afforded_growth/(52*growth_period_years)
  
  return(data.frame(target_queue_size = (target_q)
                    , afforded_growth = (afforded_growth)
                    , weekly_WL_growth_average = (weekly_growth)
                    , weekly_capacity_reduction = (weekly_activity - weekly_growth)
                    , weekly_capacity_reduction_percent = 100 * ((weekly_activity - weekly_growth)-weekly_activity)/weekly_activity
                    , weekly_capacity_reduction_percent2 = 100 * (1-(weekly_activity - weekly_growth)/weekly_activity)
                    )
         )
  
  
}


#annual_activity = 208

func_wlcalc(15110
            ,waiting_list_current = 200
            , growth_period_years = 1)


func_wlcalc(104
            ,waiting_list_current = 0
            , growth_period_years = 1)


# Current activity in, WL in ->  WL we can sustain, how much can we reduce our activity by: 1 year, 2 year.
# Weekly or annual is fine for either.

# Knee and HIP  ISP
# HRG list for each sum.  What is the waiting list?
# 1 and 2 years for 3 providers.
# NW to waiting list for each.



# 




# simulate to check


func_wlcalc_sim <- function(annual_activity, demand_reduction_proportion, waiting_list_current = 0,
                       start_date = "2025-04-01", growth_period_years=2, 
                       target = 18, target_percent=0.65
                       ){
  require(ggplot2)
  weekly_activity <- (annual_activity/52)
  demand <- weekly_activity
  capacity <- weekly_activity * (1 - demand_reduction_proportion)
  
  
  target_q <- NHSRwaitinglist::calc_target_queue_size(weekly_activity, target, qexp(target_percent))
  
  # work out end_date
  end_date <- as.POSIXlt(start_date)
  end_date$year <- end_date$year+growth_period_years
  end_date <- as.character(end_date)
  
  if(waiting_list_current > 0){ #Dump current waiting list figure into the day before the modelling period
    #WL warm-up
    wl_date <- as.POSIXlt(start_date)
    wl_date$mon <- wl_date$mon - 1
    wl_date <- as.character(wl_date)
    
    # wl_date <- as.POSIXlt(start_date)
    # wl_date <- wl_date-86400 # number of seconds in a day
    # wl_date <- as.character(wl_date)
    # 
    
    # current_wl <-NHSRwaitinglist::wl_simulator(start_date = wl_date,
    #                                            end_date = start_date,
    #                                            demand = waiting_list_current,
    #                                            capacity = waiting_list_current * 0.8)
    # 
    current_wl <- data.frame(Referral = rep(as.Date(wl_date),waiting_list_current) 
                             , Removal = rep(as.Date(NA), waiting_list_current)
    )
    
    current_q <-  wl_queue_size(current_wl)
    
    sim1<- NHSRwaitinglist::wl_simulator(start_date = start_date,
                                         end_date = end_date,
                                         demand = demand,
                                         capacity = capacity,
                                         waiting_list = current_wl 
                                          )
  } else {  #Simulate without queue if not present
    sim1<- NHSRwaitinglist::wl_simulator(start_date = start_date,
                                         end_date = end_date,
                                         demand = demand,
                                         capacity = capacity
    )
  }
  
  # Build queue
  sim_wl <-  wl_queue_size(sim1)
  
  wl_plot <- ggplot(sim_wl, aes(dates, queue_size)) +
    geom_line() +
    geom_hline(yintercept = target_q, col="purple3" )+
    annotate("text", x=as.Date(start_date, format="%Y-%m-%d"), y=target_q
             , hjust = -0.25, vjust=1.2, label = "Target list size", col="purple3" )+
    geom_hline(yintercept = waiting_list_current, col="sienna3" )+
    annotate("text", x=as.Date(start_date, format="%Y-%m-%d"), y=waiting_list_current
             , hjust = -0.25, vjust=-1, label = "Current list size", col="sienna3" )+
    scale_x_date("Months"
                 , limits = c(as.Date(start_date, format="%Y-%m-%d")
                              , as.Date(end_date, format="%Y-%m-%d"))
                 , date_labels = "%b-%y", date_breaks = "2 months", expand =expansion(mult = 0, add = 1))+
    scale_y_continuous("Queue Size", labels = scales::comma)+
    labs(
      title = paste0("Simulated waiting list, removing ", 100 * demand_reduction_proportion, "% activity"),
      subtitle = paste("Weekly demand: ", round(demand,2), ", capacity:", round(capacity,2)),
      y = "Queue Size",
      x = "Month"
    )+
    theme_minimal()+
    theme(plot.subtitle = element_text(face="italic", size = 12))
      
    
    return(list(gg_waitinglist = wl_plot, simulated_data = sim1, simulated_waiting_list = sim_wl))
}




## Cateracts
# 1 year
func_wlcalc(15110, waiting_list_current = 0, growth_period_years = 1)

cat1 <- func_wlcalc_sim(15110, 0.30, waiting_list_current = 0, growth_period_years = 1)
cat1$gg_waitinglist

# 1 year
func_wlcalc(15110, waiting_list_current = 0, growth_period_years = 2)
cat2 <- func_wlcalc_sim(15110, 0.16, waiting_list_current = 0, growth_period_years = 2)
cat2$gg_waitinglist


## Hip & Knee
# 1 year

#324 hip
# 718 knee

# 190 waiting list

((312+470)/9) * 12
# 1042.667  ~ 1043

782/1856  #~ 42% of waitign list is hip & knee.  WL is 190
0.42* 190 # 79.8 #~80 on WL

# 1 year

func_wlcalc(1043, waiting_list_current = 80, growth_period_years = 1)
hk1 <- func_wlcalc_sim(1043, 0.25, waiting_list_current = 80, growth_period_years = 1)
hk1$gg_waitinglist

# 2 years
func_wlcalc(1043, waiting_list_current = 80, growth_period_years = 2)
hk2 <- func_wlcalc_sim(1043, 0.12, waiting_list_current = 80, growth_period_years = 2)
hk2$gg_waitinglist



#  bootstrap tno 1 year

library(future.apply)

1043/52

cat_ctrl1 <- data.frame(added = 15110/52, removed = ((1-0.3) * (15110/52)), start_date = "01/04/2025",
                       end_date = "31/03/2026" )
cat_ctrl2 <- data.frame(added = (15110/52), removed = ((1-0.16) * (15110/52)), start_date = "01/04/2025",
                        end_date = "31/03/2027" )

tno_ctrl1 <- data.frame(added = 1043/52, removed = ((1-0.22) * (1043/52)), start_date = "01/04/2025",
                        end_date = "31/03/2026" )

tno_ctrl2 <- data.frame(added = (1043/52), removed = ((1-0.10) * (1043/52)), start_date = "01/04/2025",
                        end_date = "31/03/2027" )


plan(multisession, workers = 6)

start_1 <- Sys.time()
future_out1 <- do.call(list, future_replicate(100, bsol_simulate_WL(cat_ctrl1, starting_wl = 0), simplify = FALSE))
end_1 <- Sys.time()

start_2 <- Sys.time()
future_out2 <- do.call(list, future_replicate(100, bsol_simulate_WL(cat_ctrl2, starting_wl = 0), simplify = FALSE))
end_2 <- Sys.time()

#future_out3_24 <- future_out3
start_3 <- Sys.time()
future_out3 <- do.call(list, future_replicate(100, bsol_simulate_WL(tno_ctrl1, starting_wl = 80), simplify = FALSE))
end_3 <- Sys.time()

start_4 <- Sys.time()
future_out4 <- do.call(list, future_replicate(100, bsol_simulate_WL(tno_ctrl2, starting_wl = 80), simplify = FALSE))
end_4 <- Sys.time()


end_1 - start_1
end_2 - start_2
end_3 - start_3
end_4 - start_4

plan(sequential)


e <- do.call("rbind", future_out1)
e2 <- do.call("rbind", future_out2)
e3 <- do.call("rbind", future_out3)
e4 <- do.call("rbind", future_out4)

f<- aggregate(queue_size~dates, data=e, FUN = \(x) 
              c(mean_q = mean(x),
                median_q = median(x), 
                sd = sd(x),
                lower_95CI = mean(x) - (qnorm(0.975)*(sd(x)/sqrt(length(x)-1))),
                upper_95CI = mean(x) + (qnorm(0.975)*(sd(x)/sqrt(length(x)-1))),
                q_25 = quantile(x, .25, names = FALSE), 
                q_75 = quantile(x, .75, names = FALSE))
)

f2<- aggregate(queue_size~dates, data=e2, FUN = \(x) 
               c(mean_q = mean(x),
                 median_q = median(x),
                 sd = sd(x),
                 lower_95CI = mean(x) - (qnorm(0.975)*(sd(x)/sqrt(length(x)-1))),
                 upper_95CI = mean(x) + (qnorm(0.975)*(sd(x)/sqrt(length(x)-1))),
                 q_25 = quantile(x, .25, names = FALSE), 
                 q_75 = quantile(x, .75, names = FALSE))
)

f3<- aggregate(queue_size~dates, data=e3, FUN = \(x) 
              c(mean_q = mean(x),
                median_q = median(x), 
                sd = sd(x),
                lower_95CI = mean(x) - (qnorm(0.975)*(sd(x)/sqrt(length(x)-1))),
                upper_95CI = mean(x) + (qnorm(0.975)*(sd(x)/sqrt(length(x)-1))),
                q_25 = quantile(x, .25, names = FALSE), 
                q_75 = quantile(x, .75, names = FALSE))
)

f4<- aggregate(queue_size~dates, data=e4, FUN = \(x) 
               c(mean_q = mean(x),
                 median_q = median(x),
                 sd = sd(x),
                 lower_95CI = mean(x) - (qnorm(0.975)*(sd(x)/sqrt(length(x)-1))),
                 upper_95CI = mean(x) + (qnorm(0.975)*(sd(x)/sqrt(length(x)-1))),
                 q_25 = quantile(x, .25, names = FALSE), 
                 q_75 = quantile(x, .75, names = FALSE))
)


f <- data.frame(dates=as.Date(f$dates), unlist(f$queue_size))
f2 <- data.frame(dates=as.Date(f2$dates), unlist(f2$queue_size))
f3 <- data.frame(dates=as.Date(f3$dates), unlist(f3$queue_size))
f4 <- data.frame(dates=as.Date(f4$dates), unlist(f4$queue_size))

#quantile(seq(10), 0.2, names = FALSE)[1]


library(tidyverse)
library(scales)
library(BSOLTheme)

colours <- RColorBrewer::brewer.pal(n = 3, name="Dark2")

fcat <- 
  f %>% 
  mutate(projection_period = "1 year") %>% 
  bind_rows(mutate(f2, projection_period = "2 years")) %>% 
  select(dates, median_q, q_25, q_75, projection_period)

ftno <- 
  f3 %>% 
  mutate(projection_period = "1 year") %>% 
  bind_rows(mutate(f4, projection_period = "2 years")) %>% 
  select(dates, median_q, q_25, q_75, projection_period)


# Cateract monte carlo plot
ggplot(fcat, aes(y=median_q, x=dates, col=projection_period
                 , fill = projection_period, group = projection_period)) +
  geom_line() +
  geom_ribbon(aes(ymin=q_25, ymax=q_75),
              , alpha=0.5, linetype="dotted", position = position_identity()) +
  #geom_line(col= colours[2], data=f2, position = position_identity()) +
  # geom_ribbon(aes(ymin=q_25, ymax=q_75),
  #             , alpha=0.5, linetype="dotted") +
  #geom_line(aes(y=upper_95CI),col= colours[1], linetype="dotted") +
  geom_hline(yintercept=4982, col=colours[3])+
  scale_color_brewer("Projection period", palette = "Dark2", aesthetics = c("colour", "fill"))+
  scale_y_continuous(labels=comma)+
  scale_x_date(date_breaks = "3 month"
               , date_labels = "%b-%y"
               , limits = c(
                 as.Date("2025-04-01")
                 , as.Date("2027-04-01")
                 
               )
               , expand = c(0,0))+
  labs(
    title = paste0("Simulated cateract ISP waiting list, removing 30% over 1 year or 16% over 2 years"),
    subtitle = paste("Ribbon represents 25-75th percentiles, or 1000 simulations"),
    y = "Queue Size",
    x = "Month"
  )+
  theme_minimal()+
  theme(plot.title = element_text(size=12),
        plot.subtitle = element_text(size=9, face="italic"),
        axis.text.x = element_text(angle=90),
        plot.margin = margin(2,4,2,2, "mm") 
  )


# TnO monte carlo plot  
ggplot(ftno, aes(y=median_q, x=dates, col=projection_period
                 , fill = projection_period, group = projection_period)) +
  geom_line() +
  geom_ribbon(aes(ymin=q_25, ymax=q_75),
              , alpha=0.5, linetype="dotted", position = position_identity()) +
  #geom_line(col= colours[2], data=f2, position = position_identity()) +
  # geom_ribbon(aes(ymin=q_25, ymax=q_75),
  #             , alpha=0.5, linetype="dotted") +
  #geom_line(aes(y=upper_95CI),col= colours[1], linetype="dotted") +
  geom_hline(yintercept=344, col=colours[3])+
  scale_color_brewer("Projection period", palette = "Set1", aesthetics = c("colour", "fill"), direction = -1)+
  scale_y_continuous(labels=comma)+
  scale_x_date(date_breaks = "3 month"
               , date_labels = "%b-%y"
               , limits = c(
                 as.Date("2025-04-01")
                 , as.Date("2027-04-01")
                 
               )
               , expand = c(0,0))+
  labs(
    title = paste0("Simulated hip & knee ISP waiting list, removing 24% over 1 year or 10% over 2 years"),
    subtitle = paste("Ribbon represents 25-75th percentiles, or 1000 simulations"),
    y = "Queue Size",
    x = "Month"
  )+
  theme_minimal()+
  theme(plot.title = element_text(size=12),
        plot.subtitle = element_text(size=9, face="italic"),
        axis.text.x = element_text(angle=90),
        plot.margin = margin(2,4,2,2, "mm") 
  )



# ggplot(f, aes(y=median_q, x=dates)) +
#   geom_line(col= colours[1]) +
#   geom_ribbon(aes(ymin=q_25, ymax=q_75), col= colours[1], fill= colours[1],
#               , alpha=0.5, linetype="dotted", data=f, position = position_identity()) +
#   geom_line(col= colours[2], data=f2, position = position_identity()) +
#   geom_ribbon(aes(ymin=q_25, ymax=q_75),col= colours[2], fill= colours[2],
#               , alpha=0.5, linetype="dotted", data=f2) +
#   #geom_line(aes(y=upper_95CI),col= colours[1], linetype="dotted") +
#   geom_hline(yintercept=344, col=colours[3])+
#   scale_y_continuous( labels=comma)+
#   scale_x_date(date_breaks = "3 month"
#                , date_labels = "%b-%y"
#                , limits = c(
#                  as.Date("2025-04-01")
#                  , as.Date("2027-04-01")
# 
#                )
#                , expand = c(0,0))+
# 
#   theme_minimal()+
#   theme(plot.title = element_text(size=12),
#         plot.subtitle = element_text(size=9, face="italic"),
#         axis.text.x = element_text(angle=90),
#         plot.margin = margin(2,4,2,2, "mm") 
#   )
# 



ggplot(e, aes(x=dates, y=queue_size, group = dates))+
  geom_boxplot()


library(dplyr)

e %>% 
  group_by(dates) %>% 
  summarise(p = shapiro.test(queue_size)$p.value)

a$p.value

e %>% 
  group_by(dates) %>% 
  summarise(p = ks.test(queue_size, 'pnorm')$p.value)

