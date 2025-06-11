bsol_simulate_WL <-
  function(.data, seed, start_date_name = "start_date"
           , end_date_name = "end_date", adds_name = "added", removes_name="removed"
           , starting_wl = 0, reference_date
  ){
    
    
    # Make sure date columns are right format
    if (!is(object = .data[[start_date_name]], "Date")) {
      .data[[start_date_name]] <- as.Date(.data[[start_date_name]], format = "%d/%m/%Y")
    }

    if (!is(object = .data[[end_date_name]], "Date")) {
      .data[[end_date_name]] <- as.Date(.data[[end_date_name]], format = "%d/%m/%Y")
    }

    
    # rows to iterate over in control table
    sim_period_n <- nrow(.data)
    
    if (!missing(seed)) {set.seed(seed)}
    
    sims <- list(NROW(.data))
    
    seq_periods <- 
    
    for (i in seq(1, sim_period_n)) {
      if (i == 1) {
        
        inpt <- unlist(.data[1,])
        
        # handle starting waiting list
        if (starting_wl > 0) { #Dump current waiting list figure into the day before the modelling period
          #WL warm-up
          wl_date <- as.POSIXlt(.data[[start_date_name]][i])
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
          current_wl <- data.frame(Referral = rep(as.Date(wl_date), starting_wl) 
                                   , Removal = rep(as.Date(NA), starting_wl)
          )
      
     
      
          sims[[i]] <- NHSRwaitinglist::wl_simulator(as.Date(.data[[start_date_name]][i], format = "%d/%m/%Y")
                                                , as.Date(.data[[end_date_name]][i], format = "%d/%m/%Y")
                                                , as.integer(.data[[adds_name]][i])
                                                , as.integer(.data[[removes_name]][i])
                                                 , waiting_list = current_wl)
        } else {
          
          sims[[i]] <- NHSRwaitinglist::wl_simulator(as.Date(.data[[start_date_name]][i], format = "%d/%m/%Y")
                                                 , as.Date(.data[[end_date_name]][i], format = "%d/%m/%Y")
                                                 , as.integer(.data[[adds_name]][i])
                                                 , as.integer(.data[[removes_name]][i])
          )
        }
    } else {
      
      if (!missing(seed)) {set.seed(seed)}  
        
      
          sims[[i]] <- NHSRwaitinglist::wl_simulator(as.Date(.data[[start_date_name]][i], format = "%d/%m/%Y")
                            , as.Date(.data[[end_date_name]][i], format = "%d/%m/%Y")
                            , as.integer(.data[[adds_name]][i])
                            , as.integer(.data[[removes_name]][i])
                            ,  waiting_list = sims[[i - 1]]
          )
   
            
      
      } 
    }
   
    if (!missing(reference_date)) {
      referral_after_t3 <- ifelse(sims[[NROW(.data)]]$Referral < reference_date, 0, 1)
      
      t4_date <- tryCatch((sims[[NROW(.data)]] |> dplyr::filter(referral_after_t3 == 1) |> 
                             dplyr::slice_head(n=1) |>  dplyr::pull(Referral)), error = function(e) NA)
    
      return(cbind(NHSRwaitinglist::wl_queue_size(sims[[NROW(.data)]]), t4_date = as.Date(t4_date, format = "%Y-%m-%d")))
      
      } else {
    
    return(NHSRwaitinglist::wl_queue_size(sims[[NROW(.data)]]))
    }
}






