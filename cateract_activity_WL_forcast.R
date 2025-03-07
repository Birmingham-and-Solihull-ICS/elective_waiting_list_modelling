library(NHSRwaitinglist)

# Catacts ISPs
# Average wait is ~2 weeks 
# Annual activity: 15,110
# weekly activity = 291
# Target is 65% within 18 weeks

15110 / 52
# 291 per week

qexp(0.85)

calc_target_mean_wait(18, qexp(0.65))
calc_target_mean_wait(18, qexp(0.85))
calc_target_mean_wait(18, qexp(0.95))

calc_target_queue_size(291, 18, qexp(0.65))
calc_target_queue_size(291, 18, qexp(0.85))
calc_target_queue_size(291, 18, qexp(0.95))



291 * 0.65
# 189  need to be seen within 18 weeks
# Target mean wait of 17

291 * 0.85
# 247  need to be seen within 18 weeks
# Target mean weight of 9.5

# Need to calculate the capacity needed to meet weekly demand of 291 within 18 weeks at 65% and 85%

calc_target_capacity(291, 18, qexp(0.65))

calc_target_capacity(291, 18, qexp(0.85))

calc_relief_capacity(291, 0, )


demand <- 291# weeks
target_wait <- 18 # weeks
factor <- qexp(0.65)

# number of operations per week to have mean wait of 52/4
calc_target_capacity(demand, target_wait)


291 * (1**2 + 1**2) / 17

calc_target_capacity

1**2

(291*2)/17


a<- wl_simulator(start_date = "2025-04-01",
                                      ,
                end_date = "2027-03-31",
                                   
                demand = 291,
                 capacity = 261)

sim_wl <-  wl_queue_size(a)


b<- wl_simulator(start_date = "2027-04-01",
                 ,
                 end_date = "2028-03-31",
                 
                 demand = 291,
                 capacity = 291,
                 waiting_list = a)

sim_wl2 <-  wl_queue_size(b)

library(ggplot2)


ggplot(sim_wl, aes(dates, queue_size)) +
  geom_line() +
  geom_hline(yintercept = 4989, col="purple3" )+
  geom_hline(yintercept = 2761, col="sienna3" )+
  
  labs(
    title = "Simulated waiting list for cateracts, removing 20% activity from ISPs",
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )


ggplot(sim_wl2, aes(dates, queue_size)) +
  geom_line() +
  geom_hline(yintercept = 4989, col="purple3" )+
  geom_hline(yintercept = 2761, col="sienna3" )+
  
  labs(
    title = "Simulated waiting list for cateracts, removing 20% activity from ISPs",
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )




