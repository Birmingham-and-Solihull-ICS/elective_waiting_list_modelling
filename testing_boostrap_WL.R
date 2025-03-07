
#### Test parallelism approaches ####

start_2 <-Sys.time()
b1<- bsol_simulate_WL(control_periods)
b2<- bsol_simulate_WL(control_periods)
b3<- bsol_simulate_WL(control_periods)
b3<- bsol_simulate_WL(control_periods)
end_2 <-Sys.time()


start_3 <-Sys.time()

d <- replicate(100, bsol_simulate_WL(control_periods2), simplify = FALSE)

end_3 <-Sys.time()

e <- do.call("rbind", h)
f<- aggregate(queue_size~dates, data=e, FUN = \(x) 
              c(mean_q = mean(x),
                median_q = median(x), 
                lower_95CI = mean(x) - (qnorm(0.975)*(sd(x)/sqrt(length(x)))),
                upper_95CI = mean(x) + (qnorm(0.975)*(sd(x)/sqrt(length(x)))),
                q_25 = quantile(x, .025, names = FALSE), 
                q_75 = quantile(x, .975, names = FALSE))
)
f <- data.frame(dates=as.Date(f$dates), unlist(f$queue_size))



library(dplyr)

e %>% 
  arrange(dates) %>% 
  group_by(dates) %>% 
  summarise(t.test(queue_size))


sub1 <- e %>% 
  filter(dates == ' 2022-10-01')
t.test(sub1$queue_size)

group_by(dates) %>% 
  summarise(mean(queue_size))


d[[1]][1,]
d[[2]][1,]

# # start_2 <-Sys.time()
# # b<- bsol_simulate_WL2(.data=control_periods,seed = 123)
# # end_2 <-Sys.time()
# 
# a_q <- NHSRwaitinglist::wl_queue_size(a)
# b_q <- NHSRwaitinglist::wl_queue_size(b)
# 
# end_1 - start_1
# end_2 - start_2



# Try parallel

library(parallel)

detectCores(logical = FALSE)

cores <-6

my_cluster <- makePSOCKcluster(cores)

clusterExport(my_cluster, varlist = c("bsol_simulate_WL", "control_periods2")) 

start_4 <-Sys.time()

g <- clusterEvalQ(my_cluster, replicate(4, bsol_simulate_WL(control_periods2), simplify = FALSE))
#clusterEvalQ(cl = my_cluster, bsol_simulate_WL(control_periods))

end_4 <-Sys.time()

stopCluster(my_cluster)


library(foreach)
library(doParallel)


registerDoParallel(my_cluster)

iterations <-12

?icount

start_4 <- Sys.time()
h<-foreach(j=icount(iterations), #.combine = "list",
           .export= c("bsol_simulate_WL", "control_periods")) %dopar% {
             #if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=iterations)
             # setTkProgressBar(pb, j)
             
             bsol_simulate_WL(control_periods)
           }
end_4 <-Sys.time()

stopImplicitCluster()



library(doFuture)
library(iterators)
library(doRNG)

registerDoFuture()
plan(future.callr::callr)

iterations <-100

start_4 <- Sys.time()
h<-foreach(j=icount(iterations), .combine = "list",
           .export= c("bsol_simulate_WL", "control_periods")) %dorng% {
             #if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=iterations)
             # setTkProgressBar(pb, j)
             
             bsol_simulate_WL(control_periods)
           }
end_4 <-Sys.time()

stopImplicitCluster()



end_2 - start_2
end_3 - start_3
end_4 - start_4
end_5 - start_5
end_6 - start_6



e <- do.call("rbind", h)
f<- aggregate(queue_size~dates, data=e, FUN = \(x) 
              c(mu = mean(x), 
                lower_95 = mean(x) - (1.96*(sd(x)/sqrt(length(x)))),
                upper = mean(x) + (1.96*(sd(x)/sqrt(length(x)))),
                low = quantile(x, .025), 
                up = quantile(x, .975)))



# Control periods # ENT as test
control_periods <- 
  tibble::tribble(
    ~start_date,     ~end_date, ~Period,    ~WL, ~added, ~removed, 
    "01/10/2022", "31/12/2022",      1L, 16284L, 1629L,     430L,  
    "01/01/2023", "31/03/2023",      2L, 15566L,  394L,     392L,  
    "01/04/2023", "30/06/2023",      3L, 15630L,  376L,     423L,  
    "01/07/2023", "30/09/2023",      4L, 15074L,  360L,     441L,  
    "01/10/2023", "31/12/2023",      5L, 14045L,  387L,     402L,  
    "01/01/2024", "31/03/2024",      6L, 13354L,  401L,     462L,  
    "01/04/2024", "30/06/2024",      7L, 13447L,  395L,     404L,  
    "01/07/2024", "30/09/2024",      8L, 12025L,  396L,     494L,  
    "01/10/2024", "31/03/2025",      9L,     NA,  396L,     494L,  
    "01/04/2025", "31/03/2026",     10L,     NA,  400L,     494L,  
    "01/04/2026", "30/06/2026",     11L,     NA,  404L,     494L,  
    "01/07/2026", "30/09/2026",     12L,     NA,  404L,     494L,  
    "01/10/2026", "31/12/2026",     13L,     NA,  404L,     494L,  
    "01/01/2027", "31/03/2027",     14L,     NA,  404L,     494L,  
    "01/04/2027", "31/03/2028",     15L,     NA,  408L,     494L,  
    "01/04/2028", "31/03/2029",     16L,     NA,  412L,     494L,  
    "01/04/2029", "31/03/2030",     17L,     NA,  416L,     494L    )

control_periods <- as.data.frame(control_periods)



control_periods2 <- 
  tibble::tribble(
    ~start_date,   ~end_date,   ~Period,    ~WL, ~added, ~removed,
    "01/10/2022", "31/12/2022",      1L, 16284L,    1629L,        430L,   
    "01/01/2023", "31/03/2023",      2L, 15566L,     394L,        392L,   
    "01/04/2023", "30/06/2023",      3L, 15630L,     376L,        423L,   
    "01/07/2023", "30/09/2023",      4L, 15074L,     360L,        441L,   
    "01/10/2023", "31/12/2023",      5L, 14045L,     387L,        402L,   
    "01/01/2024", "31/03/2024",      6L, 13354L,     401L,        462L,   
    "01/04/2024", "30/06/2024",      7L, 13447L,     395L,        404L,   
    "01/07/2024", "30/09/2024",      8L, 12025L,     396L,        494L,   
    "01/10/2024", "31/03/2025",      9L,     NA,     396L,        404L,   
    "01/04/2025", "31/03/2026",     10L,     NA,     400L,        404L,   
    "01/04/2026", "30/06/2026",     11L,     NA,     404L,        404L,   
    "01/07/2026", "30/09/2026",     12L,     NA,     396L,        404L,   
    "01/10/2026", "31/12/2026",     13L,     NA,     388L,        404L,   
    "01/01/2027", "31/03/2027",     14L,     NA,     323L,        404L,   
    "01/04/2027", "31/03/2028",     15L,     NA,     327L,        404L,   
    "01/04/2028", "31/03/2029",     16L,     NA,     330L,        404L,   
    "01/04/2029", "31/03/2030",     17L,     NA,     333L,        404L  
  )



###############
# testing parallel method
#########################

plan(multisession(workers = 6))
d %<-% replicate(20, bsol_simulate_WL(control_periods), simplify = FALSE)

d

library(future.apply)


plan(multisession, workers = 6)

start_5 <- Sys.time()
future_out1 <- future_replicate(4, bsol_simulate_WL(control_periods), simplify = FALSE)
end_5 <- Sys.time()

start_6 <- Sys.time()
future_out2 <- do.call(list, future_replicate(100, bsol_simulate_WL(control_periods2), simplify = FALSE))
end_6 <- Sys.time()


plan(sequential)

stopImplicitCluster()
stopCluster(my_cluster)

saveRDS(h, "data/testing_h.rds")

e2 <- do.call("rbind", future_out2)
e <- do.call("rbind", h)
f<- aggregate(queue_size~dates, data=e, FUN = \(x) 
              c(mean_q = mean(x),
                median_q = median(x), 
                lower_95CI = mean(x) - (qnorm(0.975)*(sd(x)/sqrt(length(x)))),
                upper_95CI = mean(x) + (qnorm(0.975)*(sd(x)/sqrt(length(x)))),
                q_25 = quantile(x, .025, names = FALSE), 
                q_75 = quantile(x, .975, names = FALSE))
)

f2<- aggregate(queue_size~dates, data=e2, FUN = \(x) 
               c(mean_q = mean(x),
                 median_q = median(x), 
                 lower_95CI = mean(x) - (qnorm(0.975)*(sd(x)/sqrt(length(x)))),
                 upper_95CI = mean(x) + (qnorm(0.975)*(sd(x)/sqrt(length(x)))),
                 q_25 = quantile(x, .025, names = FALSE), 
                 q_75 = quantile(x, .975, names = FALSE))
)


f <- data.frame(dates=as.Date(f$dates), unlist(f$queue_size))
f2 <- data.frame(dates=as.Date(f2$dates), unlist(f2$queue_size))

quantile(seq(10), 0.2, names = FALSE)[1]

library(ggplot2)
library(scales)
library(BSOLTheme)
colours <- RColorBrewer::brewer.pal(n = 3, name="Dark2")

ggplot(f, aes(y=mean_q, x=dates)) +
  geom_line(col= colours[1]) +
  geom_ribbon(aes(ymin=q_25, ymax=q_75),col= colours[1], fill= colours[1],
              , alpha=0.5, linetype="dotted") +
  geom_line(col= colours[2], data=f2) +
  geom_ribbon(aes(ymin=q_25, ymax=q_75),col= colours[2], fill= colours[2],
              , alpha=0.5, linetype="dotted", data=f2) +
  #geom_line(aes(y=upper_95CI),col= colours[1], linetype="dotted") +
  scale_y_continuous(labels=comma)+
  scale_x_date(date_breaks = "3 month"
               , date_labels = "%b-%y"
               , limits = c(
                 as.Date("2023-01-01")
                 , as.Date("2029-04-01")
                 
               )
               , expand = c(0,0))+
  theme_minimal()+
  theme(plot.title = element_text(size=12),
        plot.subtitle = element_text(size=9, face="italic"),
        axis.text.x = element_text(angle=90),
        plot.margin = margin(2,4,2,2, "mm") 
  )
