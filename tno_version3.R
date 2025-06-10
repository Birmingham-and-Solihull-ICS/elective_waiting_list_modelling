#Generate artificial T&O scenario using real data control parameters
library(BSOLTheme)
library(tidyverse)
library(NHSRwaitinglist)
library(scales)
library(extrafont)
library(ggtext)
library(zoo)


# set ggplot theme
theme_set(
  theme_minimal() +
    theme(plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 9, face = "italic")
    )
)

colours <- RColorBrewer::brewer.pal(n = 3, name = "Dark2")


# programme timepoints
programme_dts <- 
  data.frame(
    startdate = as.Date(c('01/01/2026', '01/07/2026'#, "02/01/2027"
    ), '%d/%m/%Y'),
    enddate = as.Date(c('30/06/2026', '01/07/2026'#, "31/03/2029"
    ), '%d/%m/%Y'),
    descr = c("T2", "T3"
              # , "T4"
    )
  )

# soft launch jan - 26 10 %   - 1. T&O and Gastro, 2. urology, ENT, gynae & cardiology, 3. liver, dermatology, neurology, CYP?, - November 2026
# all practises April 20% -   live august, August = soft launch 3 months 50%, THEN FULL,  3 as 2 from November 2026 
# July - 26 - 100




# Two things needed
# A&G cost reduction, then RTT achievement is secondary
# A&G stuff should be the same 1, 20 then 100 but reduction on the specialities, and Lynda has sent through.
# What's the outpatient impact and the A&G reductions.


############## Naive forecast - last point forward #####################################

control_periods <- 
tibble::tribble(
          ~Start,         ~end, ~Period,    ~WL, ~Adds, ~Removes, ~WL_pressure, ~Adds_20, ~Removes_20, ~Adds_40, ~Removes_40,
    "01/10/2022", "31/12/2022",      1L, 10024L, 1254L,     475L,         2.64,    1254L,        475L,    1254L,        475L,
    "01/01/2023", "31/03/2023",      2L,  9323L,  441L,     479L,         0.92,     441L,        479L,     441L,        479L,
    "01/04/2023", "30/06/2023",      3L,  9205L,  506L,     516L,         0.98,     506L,        516L,     506L,        516L,
    "01/07/2023", "30/09/2023",      4L,  8803L,  465L,     490L,         0.95,     465L,        490L,     465L,        490L,
    "01/10/2023", "31/12/2023",      5L,  7942L,  418L,     519L,         0.81,     418L,        519L,     418L,        519L,
    "01/01/2024", "31/03/2024",      6L,  6691L,  399L,     481L,         0.83,     399L,        481L,     399L,        481L,
    "01/04/2024", "30/06/2024",      7L,  5909L,  415L,     446L,         0.93,     415L,        446L,     415L,        446L,
    "01/07/2024", "30/09/2024",      8L,  5707L,  456L,     495L,         0.92,     456L,        495L,     456L,        495L,
    "01/10/2024", "31/12/2024",      9L,  5123L,  480L,     512L,         0.94,     480L,        512L,     480L,        512L,
    "01/01/2025", "31/03/2025",     10L,  4524L,  435L,     479L,         0.91,     435L,        479L,     435L,        479L,
    "01/04/2025", "30/06/2025",     11L,  4409L,  490L,     502L,         0.98,     490L,        502L,     490L,        502L,
    "01/07/2025", "30/09/2025",     12L,     NA,  490L,     502L,         0.98,     490L,        502L,     490L,        502L,
    "01/10/2025", "31/12/2025",     13L,     NA,  490L,     502L,         0.98,     490L,        502L,     490L,        502L,
    "01/01/2026", "31/03/2026",     14L,     NA,  490L,     502L,         0.98,     480L,        502L,     471L,        502L,
    "01/04/2026", "30/06/2026",     15L,     NA,  495L,     502L,         0.99,     475L,        502L,     455L,        502L,
    "01/07/2026", "31/03/2027",     16L,     NA,  495L,     502L,         0.99,     396L,        502L,     297L,        502L,
    "01/04/2027", "31/03/2028",     17L,     NA,  500L,     502L,            1,     400L,        502L,     300L,        502L,
    "01/04/2028", "31/03/2029",     18L,     NA,  505L,     502L,         1.01,     404L,        502L,     303L,        502L
    )




control_periods$Start <- as.Date(control_periods$Start, format = "%d/%m/%Y")
control_periods$end <- as.Date(control_periods$end, format = "%d/%m/%Y")


# version 1 with 5% referral growth

# set up empty list slots for speed of append and iteration
tno_sims <- list(NROW(control_periods))
tno_sims_20 <- list(NROW(control_periods))
tno_sims_40 <- list(NROW(control_periods))

# set random number generation to defined start
set.seed(124)

tno_sims[[1]] <-
  wl_simulator(control_periods$Start[1]
               , control_periods$end[1]
               , control_periods$Adds[1]
               , control_periods$Removes[1])

# Pull out queueing numbers
tno_queue1 <-
  wl_queue_size(tno_sims[[1]])

# Check queue visually
ggplot(tno_queue1, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", control_periods$Removes[1], ", Demand=", control_periods$Adds[1]),
    y = "Queue Size",
    x = "Month"
  )

tail(tno_queue1)

# Create empty lists for 20 and 40% benefit scenarios with same starting point
tno_sims_20[[1]] <- tno_sims[[1]]
tno_sims_40[[1]] <-  tno_sims[[1]]



#  Run simulation in each sceanario
start <- Sys.time()
for (i in seq(2, NROW(control_periods),1)) {
  set.seed(125)  
  tno_sims[[i]] <-
    wl_simulator(control_periods$Start[i]
                 , control_periods$end[i]
                 , control_periods$Adds[i]
                 , control_periods$Removes[i]
                 , waiting_list = tno_sims[[i - 1]]
    )
  
}
Sys.time() - start


start <- Sys.time()
for (i in seq(2, NROW(control_periods),1)) {
  set.seed(125)  
  tno_sims_20[[i]] <-
    wl_simulator(control_periods$Start[i]
                 , control_periods$end[i]
                 , control_periods$Adds_20[i]
                 , control_periods$Removes_20[i]
                 , waiting_list = tno_sims_20[[i - 1]]
    )
  
}
Sys.time() - start



start <- Sys.time()
for (i in seq(2, NROW(control_periods),1)) {
  set.seed(125)  
  tno_sims_40[[i]] <-
    wl_simulator(control_periods$Start[i]
                 , control_periods$end[i]
                 , control_periods$Adds_40[i]
                 , control_periods$Removes_40[i]
                 , waiting_list = tno_sims_40[[i - 1]]
    )
  
}
Sys.time() - start



# Pull out each queue
tno_queue <-
wl_queue_size(tno_sims[[NROW(tno_sims)]])

tno_queue_20 <-
  wl_queue_size(tno_sims_20[[NROW(tno_sims_20)]])

tno_queue_40 <-
  wl_queue_size(tno_sims_40[[NROW(tno_sims_40)]])


# Visual check
ggplot(tno_queue, aes(dates, queue_size)) +
  geom_line(col = colours[2], data = tno_queue_20) +
  geom_line(col = colours[3], data = tno_queue_40) +
  geom_line(col = colours[1]) +
  labs(
    title = bquote(bold("T&O:") ~ "First GP referral to first Outpatients waiting list:"),
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )


# Is target met?
tno_queue <-
  tno_queue |> 
  mutate(
    target_queue = calc_target_queue_size(tail(control_periods,1)$Adds, 6, factor = 1),
    meet_target_without = ifelse((queue_size <= target_queue) & (dates > as.Date('2023-01-01')), 1,0),
    meet_target_with = ifelse((queue_size <= target_queue) & (dates > as.Date('2023-01-01')), 1,0)
  )

tno_queue_20 <-
  tno_queue_20 |> 
  mutate(
    target_queue = calc_target_queue_size(tail(control_periods,1)$Adds_20, 6, factor = 1),
    meet_target_without = ifelse((queue_size <= target_queue) & (dates > as.Date('2023-01-01')), 1,0),
    meet_target_with = ifelse((queue_size <= target_queue) & (dates > as.Date('2023-01-01')), 1,0)
  )

tno_queue_40 <-
  tno_queue_40 |> 
  mutate(
    target_queue = calc_target_queue_size(tail(control_periods,1)$Adds_40, 6, factor = 1),
    meet_target_without = ifelse((queue_size <= target_queue) & (dates > as.Date('2023-01-01')), 1,0),
    meet_target_with = ifelse((queue_size <= target_queue) & (dates > as.Date('2023-01-01')), 1,0)
  )



# Create list of summary data
tno_summary <-
  list(tno_sim = 
         list(
           target_queue_size = calc_target_queue_size(control_periods$Adds[NROW(control_periods)], 18, factor = qexp(0.95)),
           current_target_capacity = calc_target_capacity(control_periods$Adds[11], 18, factor = qexp(0.95)),
           future_target_capacity = calc_target_capacity(control_periods$Adds[NROW(control_periods)], 18, factor = qexp(0.95)),
           queue_at_t3 = tno_queue |> filter(dates >= as.Date(programme_dts[programme_dts[, "descr"] == "T3",1]
                                                              , "%d/%m/%Y")) |>  slice_head(n = 1) |> pull(queue_size),
           target_met = tno_queue |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(meet_target_with),
           target_met_date =  tno_queue |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(dates)
           
           
           ),
       tno_sim_20 = 
         list(
           target_queue_size = calc_target_queue_size(control_periods$Adds_20[NROW(control_periods)], 18, factor = qexp(0.95)),
           current_target_capacity = calc_target_capacity(control_periods$Adds_20[11], 18, factor = qexp(0.95)),
           future_target_capacity = calc_target_capacity(control_periods$Adds_20[NROW(control_periods)], 18, factor = qexp(0.95)),
           queue_at_t3 = tno_queue_20 |> filter(dates >= as.Date(programme_dts[programme_dts[, "descr"] == "T3",1]
                                                              , "%d/%m/%Y")) %>% slice_head(n = 1) %>% pull(queue_size),
           target_met = tno_queue_20 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(meet_target_with),
           target_met_date =  tno_queue_20 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(dates)
           
         ),
       tno_sim_40 = 
         list(
           target_queue_size = calc_target_queue_size(control_periods$Adds_40[NROW(control_periods)], 18, factor = qexp(0.95)),
           current_target_capacity = calc_target_capacity(control_periods$Adds_40[11], 18, factor = qexp(0.95)),
           future_target_capacity = calc_target_capacity(control_periods$Adds_40[NROW(control_periods)], 18, factor = qexp(0.95)),
           queue_at_t3 = tno_queue_40 |> filter(dates >= as.Date(programme_dts[programme_dts[, "descr"] == "T3",1]
                                                                 , "%d/%m/%Y")) %>% slice_head(n = 1) %>% pull(queue_size),
           target_met = tno_queue_40 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(meet_target_with),
           target_met_date =  tno_queue_40 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(dates)
           
         )
  )


# add difference calculations that can't be done in list creation.
tno_summary <- lapply(tno_summary, function(x) {c(x, "distance_from_target_t3" = x$queue_at_t3 - x$target_queue_size)})
tno_summary <- lapply(tno_summary, function(x) {c(x, "weekly_capacity_release" = x$current_target_capacity - x$future_target_capacity)})






############## Naive forecast - last point forward #####################################

control_periods_2 <- 
  tibble::tribble(
           ~Start,         ~end, ~Period,    ~WL, ~Adds, ~Removes, ~WL_pressure, ~Adds_20, ~Removes_20, ~Adds_40, ~Removes_40,
     "01/10/2022", "31/12/2022",      1L, 10024L, 1254L,     475L,         2.64,    1254L,        475L,    1254L,        475L,
     "01/01/2023", "31/03/2023",      2L,  9323L,  441L,     479L,         0.92,     441L,        479L,     441L,        479L,
     "01/04/2023", "30/06/2023",      3L,  9205L,  506L,     516L,         0.98,     506L,        516L,     506L,        516L,
     "01/07/2023", "30/09/2023",      4L,  8803L,  465L,     490L,         0.95,     465L,        490L,     465L,        490L,
     "01/10/2023", "31/12/2023",      5L,  7942L,  418L,     519L,         0.81,     418L,        519L,     418L,        519L,
     "01/01/2024", "31/03/2024",      6L,  6691L,  399L,     481L,         0.83,     399L,        481L,     399L,        481L,
     "01/04/2024", "30/06/2024",      7L,  5909L,  415L,     446L,         0.93,     415L,        446L,     415L,        446L,
     "01/07/2024", "30/09/2024",      8L,  5707L,  456L,     495L,         0.92,     456L,        495L,     456L,        495L,
     "01/10/2024", "31/12/2024",      9L,  5123L,  480L,     512L,         0.94,     480L,        512L,     480L,        512L,
     "01/01/2025", "31/03/2025",     10L,  4524L,  435L,     479L,         0.91,     435L,        479L,     435L,        479L,
     "01/04/2025", "30/06/2025",     11L,  4409L,  490L,     502L,         0.98,     490L,        502L,     490L,        502L,
     "01/07/2025", "30/09/2025",     12L,     NA,  490L,     500L,         0.98,     480L,        500L,     471L,        500L,
     "01/10/2025", "31/12/2025",     13L,     NA,  490L,     500L,         0.98,     471L,        500L,     451L,        500L,
     "01/01/2026", "31/03/2026",     14L,     NA,  490L,     500L,         0.98,     480L,        500L,     471L,        500L,
     "01/04/2026", "30/06/2026",     15L,     NA,  495L,     500L,         0.99,     475L,        500L,     455L,        500L,
     "01/07/2026", "31/03/2027",     16L,     NA,  495L,     500L,         0.99,     396L,        500L,     297L,        500L,
     "01/04/2027", "31/03/2028",     17L,     NA,  500L,     500L,            1,     400L,        500L,     300L,        500L,
     "01/04/2028", "31/03/2029",     18L,     NA,  505L,     500L,         1.01,     404L,        500L,     303L,        500L
     )





control_periods_2$Start <- as.Date(control_periods_2$Start, format = "%d/%m/%Y")
control_periods_2$end <- as.Date(control_periods_2$end, format = "%d/%m/%Y")


# version 1 with 5% referral growth

# set up empty list slots for speed of append and iteration
tno_sims_2 <- list(NROW(control_periods_2))
tno_sims_20_2 <- list(NROW(control_periods_2))
tno_sims_40_2 <- list(NROW(control_periods_2))

# set random number generation to defined start
set.seed(124)

tno_sims_2[[1]] <-
  wl_simulator(control_periods_2$Start[1]
               , control_periods_2$end[1]
               , control_periods_2$Adds[1]
               , control_periods_2$Removes[1])

# Pull out queueing numbers
tno_queue1_2 <-
  wl_queue_size(tno_sims_2[[1]])

# Check queue visually
ggplot(tno_queue1_2, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", control_periods_2$Removes[1], ", Demand=", control_periods_2$Adds[1]),
    y = "Queue Size",
    x = "Month"
  )

tail(tno_queue1_2)

# Create empty lists for 20 and 40% benefit scenarios with same starting point
tno_sims_20_2[[1]] <- tno_sims_2[[1]]
tno_sims_40_2[[1]] <-  tno_sims_2[[1]]



#  Run simulation in each sceanario
start <- Sys.time()
for (i in seq(2, NROW(control_periods_2),1)) {
  set.seed(125)  
  tno_sims_2[[i]] <-
    wl_simulator(control_periods_2$Start[i]
                 , control_periods_2$end[i]
                 , control_periods_2$Adds[i]
                 , control_periods_2$Removes[i]
                 , waiting_list = tno_sims_2[[i - 1]]
    )
  
}
Sys.time() - start


start <- Sys.time()
for (i in seq(2, NROW(control_periods_2),1)) {
  set.seed(125)  
  tno_sims_20_2[[i]] <-
    wl_simulator(control_periods_2$Start[i]
                 , control_periods_2$end[i]
                 , control_periods_2$Adds_20[i]
                 , control_periods_2$Removes_20[i]
                 , waiting_list = tno_sims_20_2[[i - 1]]
    )
  
}
Sys.time() - start



start <- Sys.time()
for (i in seq(2, NROW(control_periods_2),1)) {
  set.seed(125)  
  tno_sims_40_2[[i]] <-
    wl_simulator(control_periods_2$Start[i]
                 , control_periods_2$end[i]
                 , control_periods_2$Adds_40[i]
                 , control_periods_2$Removes_40[i]
                 , waiting_list = tno_sims_40_2[[i - 1]]
    )
  
}
Sys.time() - start



# Pull out each queue
tno_queue_2 <-
  wl_queue_size(tno_sims[[NROW(tno_sims)]])

tno_queue_20_2 <-
  wl_queue_size(tno_sims_20[[NROW(tno_sims_20)]])

tno_queue_40_2 <-
  wl_queue_size(tno_sims_40[[NROW(tno_sims_40)]])


# Visual check
ggplot(tno_queue_2, aes(dates, queue_size)) +
  geom_line(col = colours[2], data = tno_queue_20) +
  geom_line(col = colours[3], data = tno_queue_40) +
  geom_line(col = colours[1]) +
  labs(
    title = bquote(bold("T&O:") ~ "First GP referral to first Outpatients waiting list:"),
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )


# Is target met?
tno_queue_2 <-
  tno_queue_2 |> 
  mutate(
    target_queue = calc_target_queue_size(tail(control_periods_2,1)$Adds, 6, factor = 1),
    meet_target_without = ifelse((queue_size <= target_queue) & (dates > as.Date('2023-01-01')), 1,0),
    meet_target_with = ifelse((queue_size <= target_queue) & (dates > as.Date('2023-01-01')), 1,0)
  )

tno_queue_20_2 <-
  tno_queue_20_2 |> 
  mutate(
    target_queue = calc_target_queue_size(tail(control_periods_2,1)$Adds_20, 6, factor = 1),
    meet_target_without = ifelse((queue_size <= target_queue) & (dates > as.Date('2023-01-01')), 1,0),
    meet_target_with = ifelse((queue_size <= target_queue) & (dates > as.Date('2023-01-01')), 1,0)
  )

tno_queue_40_2 <-
  tno_queue_40_2 |> 
  mutate(
    target_queue = calc_target_queue_size(tail(control_periods_2,1)$Adds_40, 6, factor = 1),
    meet_target_without = ifelse((queue_size <= target_queue) & (dates > as.Date('2023-01-01')), 1,0),
    meet_target_with = ifelse((queue_size <= target_queue) & (dates > as.Date('2023-01-01')), 1,0)
  )



# Create list of summary data
tno_summary_2 <-
  list(tno_sim = 
         list(
           target_queue_size = calc_target_queue_size(control_periods_2$Adds[NROW(control_periods_2)], 18, factor = qexp(0.95)),
           current_target_capacity = calc_target_capacity(control_periods_2$Adds[11], 18, factor = qexp(0.95)),
           future_target_capacity = calc_target_capacity(control_periods_2$Adds[NROW(control_periods_2)], 18, factor = qexp(0.95)),
           queue_at_t3 = tno_queue_2 |> filter(dates >= as.Date(programme_dts[programme_dts[, "descr"] == "T3",1]
                                                              , "%d/%m/%Y")) |>  slice_head(n = 1) |> pull(queue_size),
           target_met = tno_queue_2 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(meet_target_with),
           target_met_date =  tno_queue_2 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(dates)
           
           
         ),
       tno_sim_20 = 
         list(
           target_queue_size = calc_target_queue_size(control_periods_2$Adds_20[NROW(control_periods_2)], 18, factor = qexp(0.95)),
           current_target_capacity = calc_target_capacity(control_periods_2$Adds_20[11], 18, factor = qexp(0.95)),
           future_target_capacity = calc_target_capacity(control_periods_2$Adds_20[NROW(control_periods_2)], 18, factor = qexp(0.95)),
           queue_at_t3 = tno_queue_20_2 |> filter(dates >= as.Date(programme_dts[programme_dts[, "descr"] == "T3",1]
                                                                 , "%d/%m/%Y")) %>% slice_head(n = 1) %>% pull(queue_size),
           target_met = tno_queue_20_2 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(meet_target_with),
           target_met_date =  tno_queue_20_2 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(dates)
           
         ),
       tno_sim_40 = 
         list(
           target_queue_size = calc_target_queue_size(control_periods_2$Adds_40[NROW(control_periods_2)], 18, factor = qexp(0.95)),
           current_target_capacity = calc_target_capacity(control_periods_2$Adds_40[11], 18, factor = qexp(0.95)),
           future_target_capacity = calc_target_capacity(control_periods_2$Adds_40[NROW(control_periods_2)], 18, factor = qexp(0.95)),
           queue_at_t3 = tno_queue_40_2 |> filter(dates >= as.Date(programme_dts[programme_dts[, "descr"] == "T3",1]
                                                                 , "%d/%m/%Y")) %>% slice_head(n = 1) %>% pull(queue_size),
           target_met = tno_queue_40_2 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(meet_target_with),
           target_met_date =  tno_queue_40_2 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(dates)
           
         )
  )


# add difference calculations that can't be done in list creation.
tno_summary_2 <- lapply(tno_summary_2, function(x) {c(x, "distance_from_target_t3" = x$queue_at_t3 - x$target_queue_size)})
tno_summary_2 <- lapply(tno_summary_2, function(x) {c(x, "weekly_capacity_release" = x$current_target_capacity - x$future_target_capacity)})






library(future.apply)


plan(multisession, workers = 6)

start_5 <- Sys.time()
future_out1 <- future_replicate(100
                                , bsol_simulate_WL(control_periods_2, start_date_name = "Start", end_date_name = "end"
                                                   , adds_name = "Adds",  removes_name = "Removes")
                                , simplify = FALSE)
end_5 <- Sys.time()

start_6 <- Sys.time()
future_out2 <- future_replicate(100
                                , bsol_simulate_WL(control_periods_2, start_date_name = "Start", end_date_name = "end"
                                                   , adds_name = "Adds_20",  removes_name = "Removes_20")
                                , simplify = FALSE)
end_6 <- Sys.time()

start_7 <- Sys.time()
future_out3 <- future_replicate(100
                                , bsol_simulate_WL(control_periods_2, start_date_name = "Start", end_date_name = "end"
                                                   , adds_name = "Adds_40",  removes_name = "Removes_40")
                                , simplify = FALSE)
end_7 <- Sys.time()

plan(sequential)

test_rep <- bsol_simulate_WL(control_periods_2
                             , start_date_name = "Start"
                              , end_date_name = "end"
                              , adds_name = "Adds"
                              ,  removes_name = "Removes"
                             # , starting_wl = 10018
                             )

e <- do.call("rbind", future_out1)
e2 <- do.call("rbind", future_out2)
e3 <- do.call("rbind", future_out3)
f <- aggregate(queue_size~dates, data=e, FUN = \(x) 
              c(mean_q = mean(x),
                median_q = median(x), 
                lower_95CI = mean(x) - (qnorm(0.975)*(sd(x)/sqrt(length(x)))),
                upper_95CI = mean(x) + (qnorm(0.975)*(sd(x)/sqrt(length(x)))),
                q_25 = quantile(x, .025, names = FALSE), 
                q_75 = quantile(x, .975, names = FALSE))
)


f <- data.frame(dates = as.Date(f$dates), unlist(f$queue_size))

f2 <- aggregate(queue_size~dates, data=e2, FUN = \(x) 
               c(mean_q = mean(x),
                 median_q = median(x), 
                 lower_95CI = mean(x) - (qnorm(0.975)*(sd(x)/sqrt(length(x)))),
                 upper_95CI = mean(x) + (qnorm(0.975)*(sd(x)/sqrt(length(x)))),
                 q_25 = quantile(x, .025, names = FALSE), 
                 q_75 = quantile(x, .975, names = FALSE))
)


f2 <- data.frame(dates = as.Date(f2$dates), unlist(f2$queue_size))

f3 <- aggregate(queue_size~dates, data=e3, FUN = \(x) 
               c(mean_q = mean(x),
                 median_q = median(x), 
                 lower_95CI = mean(x) - (qnorm(0.975)*(sd(x)/sqrt(length(x)))),
                 upper_95CI = mean(x) + (qnorm(0.975)*(sd(x)/sqrt(length(x)))),
                 q_25 = quantile(x, .025, names = FALSE), 
                 q_75 = quantile(x, .975, names = FALSE))
)


f3 <- data.frame(dates = as.Date(f3$dates), unlist(f3$queue_size))


# Visual check
ggplot(f, aes(dates, mean_q)) +
  geom_line(col = colours[2], data = f2) +
  geom_ribbon(aes(y = mean_q, ymin = q_25, ymax = q_75)
              , alpha = 0.5, data = f2, fill = colours[2]) +
  geom_line(col = colours[3], data = f3) +
  geom_ribbon(aes(y = mean_q, ymin = q_25, ymax = q_75)
              , alpha = 0.5, data = f3, fill = colours[3]) + 
  geom_line(col = colours[1]) +
  geom_ribbon(aes(y = mean_q, ymin = q_25, ymax = q_75)
              , alpha = 0.5, fill = colours[1]) +
  labs(
    title = bquote(bold("T&O:") ~ "Monte Carlo sim:"),
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )


#saveRDS(future_out1, "./output/v3/tno_mc1.rds")

# Summary on mc
# Is target met?
f <-
  f |> 
  mutate(
    target_queue = calc_target_queue_size(tail(control_periods_2,1)$Adds, 6, factor = 1),
    meet_target_without = ifelse((mean_q <= target_queue) & (dates > as.Date('2023-01-01')), 1,0),
    meet_target_with = ifelse((mean_q <= target_queue) & (dates > as.Date('2023-01-01')), 1,0)
  )

f2 <-
  f2 |> 
  mutate(
    target_queue = calc_target_queue_size(tail(control_periods_2,1)$Adds_20, 6, factor = 1),
    meet_target_without = ifelse((mean_q <= target_queue) & (dates > as.Date('2023-01-01')), 1,0),
    meet_target_with = ifelse((mean_q <= target_queue) & (dates > as.Date('2023-01-01')), 1,0)
  )

f3 <-
  f3 |> 
  mutate(
    target_queue = calc_target_queue_size(tail(control_periods_2,1)$Adds_40, 6, factor = 1),
    meet_target_without = ifelse((mean_q <= target_queue) & (dates > as.Date('2023-01-01')), 1,0),
    meet_target_with = ifelse((mean_q <= target_queue) & (dates > as.Date('2023-01-01')), 1,0)
  )


# Add columns for mc values to output
#- 
tno_summary_2$tno_sim$queue_at_t3_mc <- f |> filter(dates >= as.Date(programme_dts[programme_dts[, "descr"] == "T3",1]
                                                     , "%d/%m/%Y")) |>  slice_head(n = 1) |> pull(mean_q)
tno_summary_2$tno_sim$target_met_mc <- f |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(meet_target_with)
tno_summary_2$tno_sim$target_met_date_mc =  f |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(dates)

#20
tno_summary_2$tno_sim_20$queue_at_t3_mc <- f2 |> filter(dates >= as.Date(programme_dts[programme_dts[, "descr"] == "T3",1]
                                                                     , "%d/%m/%Y")) |>  slice_head(n = 1) |> pull(mean_q)
tno_summary_2$tno_sim_20$target_met_mc <- f2 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(meet_target_with)
tno_summary_2$tno_sim_20$target_met_date_mc =  f2 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(dates)

#40
tno_summary_2$tno_sim_40$queue_at_t3_mc <- f3 |> filter(dates >= as.Date(programme_dts[programme_dts[, "descr"] == "T3",1]
                                                                     , "%d/%m/%Y")) |>  slice_head(n = 1) |> pull(mean_q)
tno_summary_2$tno_sim_40$target_met_mc <- f3 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(meet_target_with)
tno_summary_2$tno_sim_40$target_met_date_mc =  f3 |> filter(meet_target_with == 1) |> slice_head(n = 1) |> pull(dates)



# add difference calculations that can't be done in list creation.
tno_summary_2 <- lapply(tno_summary_2, function(x) {c(x, "distance_from_target_t3_mc" = x$queue_at_t3_mc - x$target_queue_size)})
