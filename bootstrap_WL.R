bsol_simulate_WL <-
  function(.data, seed, start_date_name = "start_date"
           , end_date_name = "end_date", adds_name = "added", removes_name="removed"
  ){
    
    
    # Make sure date columns are right format
    if(!is(object = .data$start_date, "Date")){
      .data$start_date <- as.Date(.data$start_date, format = "%d/%m/%Y")
    }
    
    if(!is(object = .data$end_date, "Date")){
      .data$end_date <- as.Date(.data$end_date, format = "%d/%m/%Y")
    }
    
    
    # rows to iterate over in control table
    sim_period_n <- nrow(.data)
    
    if(!missing(seed)){set.seed(seed)}
    
    seq_periods <- 
    
    for(i in seq(1, sim_period_n) ){
      if(i == 1){
      
      inpt <- unlist(.data[1,])
      
      sim_1 <- NHSRwaitinglist::wl_simulator(as.Date(inpt[start_date_name], format = "%d/%m/%Y")
                                             , as.Date(inpt[end_date_name], format = "%d/%m/%Y")
                                             , as.integer(inpt[adds_name])
                                             , as.integer(inpt[removes_name]))
    } else {
      
      if(!missing(seed)){set.seed(seed)}  
        
      inpt <- unlist(.data[i,])
        
      eval(
        call("<-"
             , as.name(paste0("sim_",as.character(i)))
             , NHSRwaitinglist::wl_simulator(as.Date(inpt[start_date_name], format = "%d/%m/%Y")
                            , as.Date(inpt[end_date_name], format = "%d/%m/%Y")
                            , as.integer(inpt[adds_name])
                            , as.integer(inpt[removes_name])
                            ,  waiting_list = get(paste0("sim_", as.character(i-1))))
              )
        )
            
      
      } 
    }
    
    return(NHSRwaitinglist::wl_queue_size(get(paste0("sim_", as.character(i)))))
    
  }



