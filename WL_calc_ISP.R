library(NHSRwaitinglist)

# Catacts ISPs
# Average wait is ~2 weeks 
# Annual activity: 15,110
# weekly activity = 291
# Target is 65% within 18 weeks


func_wlcalc<- function(annual_activity, waiting_list_current = 0, target = 18, target_percent=0.65, growth_period_years=1){
  
  weekly_activity <- annual_activity/52
  
  target_q <- NHSRwaitinglist::calc_target_queue_size(weekly_activity, target, qexp(target_percent))
  
  afforded_growth <- target_q - waiting_list_current
  
  weekly_growth <- afforded_growth/(52*growth_period_years)
  
  return(data.frame("target_queue_size" = floor(target_q)
                    , "afforded_growth" = floor(afforded_growth)
                    , "weekly_growth" = floor(weekly_growth)
                    , "weekly_capacity_reduction" = floor(weekly_activity - weekly_growth)
                    , "weekly_capacity_change %" = (floor(weekly_activity - weekly_growth) - weekly_activity)/weekly_activity)
         )
  
}




func_wlcalc(15110
            ,waiting_list_current = 200
            , growth_period_years = 1)

# Current activity in, WL in ->  WL we can sustain, how much can we reduce our activity by: 1 year, 2 year.
# Weekly or annual is fine for either.

# Knee and HIP  ISP
# HRG list for each sum.  What is the waiting list?
# 1 and 2 years for 3 providers.
# NW to waiting list for each.



# 




# simulate to check


func_wlcalc_sim<- function(annual_activity, demand_reduction, waiting_list_current = 0,
                       start_date = "2025-04-01", growth_period_years=2, 
                       target = 18, target_percent=0.65
                       ){
  require(ggplot2)
  weekly_activity <- (annual_activity/52)
  demand <- round(weekly_activity)
  capacity <- round(weekly_activity * (1 - demand_reduction))
  
  
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
             , hjust = -0.25, vjust=-1, label = "Curret list size", col="sienna3" )+
    scale_x_date("Months", limits = c(as.Date(start_date, format="%Y-%m-%d"), as.Date(end_date, format="%Y-%m-%d")))+
    scale_y_continuous("Queue Size", labels = scales::comma)+
    labs(
      title = paste0("Simulated waiting list, removing ", 100 * demand_reduction, "% activity"),
      subtitle = paste("Weekly demand: ", demand, ", capacity:", capacity),
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

cat1<- func_wlcalc_sim(15110, 0.30, waiting_list_current = 0, growth_period_years = 1)
cat1

# 1 year
func_wlcalc(15110, waiting_list_current = 0, growth_period_years = 2)
cat2 <- func_wlcalc_sim(15110, 0.16, waiting_list_current = 0, growth_period_years = 2)
cat2
