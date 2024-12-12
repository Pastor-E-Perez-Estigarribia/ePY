require(odin)

################################################################################
# Odin model
################################################################################
sir_model <- odin::odin({
  
  scaling <- interpolate(interp_ts, interp_scaling, "constant")
  introduction_i <- interpolate(interp_ts, interp_introductions, "constant")
  
  # Derivatives
  s_to_i <- scaling * state_s * state_i / population #sir_beta0
  deriv(state_s) <- -s_to_i # Susceptible
  deriv(state_i) <-  s_to_i - sir_gamma * state_i + introduction_i # Infectious
  
  # Initial conditions
  initial(state_s) <- state_s0
  initial(state_i) <- state_i0
  
  # Add incident number of infections to output
  output(i_inf) <- s_to_i # Incident number of infections
  
  # Parameters
  population <- user()
  #sir_beta0  <- user()
  sir_gamma  <- user()
  state_s0   <- user()
  state_i0   <- user()
  # Scaling
  interp_ts[] <- user()
  interp_scaling[] <- user()
  interp_introductions[] <- user()
  
  dim(interp_ts) <- user()
  dim(interp_scaling) <- length(interp_ts)
  dim(interp_introductions) <- length(interp_ts)
})


################################################################################
# Helper function for inference
################################################################################
create_model <- function(data, population, fits_coeff, location, prop_immune0, sir_gamma,
                         temperature_function, rainfall_function,
                         scaling_function, introduction_function) {
  
  # #Debug
  # param_set = 1
  # data = input_case
  # population = pop_paraguay
  # fits_coeff = coeffs_weather
  # location="National"
  # prop_immune0 = params_fixed$immune_0[param_set]
  # sir_gamma = 1 / 2
  # #reporting = params_fixed$reporting[param_set],
  # #temperature_function = average_temperature,
  # #rainfall_function = average_rainfall,
  # scaling_function = sine_scaling
  # introduction_function=no_introduction
  
  
  # NOTE: dataframe 'data' must have columns 'date' and 'i_cases'
  n_data <- nrow(data)
  
  # Simulation time points
  ts <- data$date
  ts_int <- 0:(length(ts) - 1)
  
  ##############################################################################
  # Initial parameter values
  #   1 = prop of infected individuals at t0
  #   X = beta0 (removed for now)
  #   2 = reporting
  #   3 = neg shape
  #  >3 = weekly scaling
  ##############################################################################
  params0 <- c(1.348612e-07*100, #prop i0
               #runif(1,0.1,0.5),  #1./7 #beta0
               runif(1,0.01,0.1), #0.05,  #reporting
               log(1e3), #negbin shape
               runif(length(ts),0.05,4))   #rep(0.1,length(ts))) #weekly scaling
  
  ##############################################################################
  # Climate effect
  #####################################-#########################################
  # interp_scaling <- scaling_function(temperature_function(date=ts,location=location,coeff_table=fits_coeff),
  #                                    rainfall_function(date=ts,location=location,coeff_table=fits_coeff))
  interp_scaling = function(s_v) scaling_function(t_date = ts, scaling_vect = s_v)
  
  
  ##############################################################################
  # Introducation of infections
  ##############################################################################
  interp_introductions <- introduction_function(date=ts)
  
  ##############################################################################
  # Simulation function (for model fitting)
  ##############################################################################
  simulator <- sir_model$new(
    population = population,
    #sir_beta0 = params0[2],
    sir_gamma = sir_gamma,
    state_s0 = population * (1 - prop_immune0) * (1 - params0[1]),
    state_i0 = population * (1 - prop_immune0) * params0[1],
    interp_ts = ts_int,
    interp_scaling = interp_scaling(s_v = params0[4:length(params0)]),
    interp_introductions = interp_introductions
  )
  
  simulate <- function(params, t_points) {
    # NOTE:
    #   params[1] = prop of individuals infected at t0
    #   params[2] = beta0
    #   params[3] = reporting rate
    simulator$set_user(
      #sir_beta0 = params[2],
      state_s0 = population * (1 - prop_immune0) * (1 - params[1]),
      state_i0 = population * (1 - prop_immune0) * params[1],
      interp_scaling = interp_scaling(s_v = params[4:length(params0)])
    )
    simulator$run(t_points)
  }
  
  
  ##############################################################################
  # Observation process
  ##############################################################################
  observe <- function(sims, params) {
    #rpois(nrow(sims), lambda = params[3] * sims[, "i_inf"])
    rnbinom(nrow(sims), mu = params[2] * sims[, "i_inf"], size = exp(params[3])) #i_inf incialmente
  }
  
  ##############################################################################
  # Log-likelihood function
  ##############################################################################
  observed <- data$i_cases
  observed_sero_pos = data$n_pos
  observed_sero_tot = data$n_tot
  
  compute_loglik <- function(params) { 
    sims <- simulate(params, ts_int)
    expected <- params[2] * sims[, "i_inf"] #i_inf incialmente
    expected_sero = (population - sims[,"state_s"])/population
    
    #POSSON LIKELIHOOD
    # loglik = sum(dpois(observed, lambda = expected+1e-6, log = TRUE), na.rm=T) + #Case data likelihood
    #   1*sum(dbinom(x=observed_sero_pos,size=observed_sero_tot,prob=expected_sero,log=TRUE), na.rm=T)

    #NEGBIN LIKELIHOOD
    loglik =  sum(dnbinom(observed, mu = expected+1e-6, size=exp(params[3]),log = TRUE), na.rm=T) + #Case data likelihood
      sum(dbinom(x=observed_sero_pos,size=observed_sero_tot,prob=expected_sero,log=TRUE), na.rm=T)


    return(loglik) #Sero data likelihood
  }
  
  ##############################################################################
  # Function that checks if parameter value is invalid
  ##############################################################################
  is_invalid <- function(k, value) {
    if ((k == 1) & (value > 0.1)) return(TRUE) # Prop infected at t0
    #if ((k == 2) & (value > 10)) return(TRUE) # beta0
    if ((k == 2) & (value > 1)) return(TRUE) # reporting
    if ((k == 3) & (value < log(5))) return(TRUE) # negbin shape
    if ((k > 3) & ((value < 0) | (value>10))) return(TRUE) # Weekly scaling
    FALSE
  }
  
  ##############################################################################
  # Indices to update
  ##############################################################################
  inds_to_update <- (1:length(params0))[-c(1,3)]
  
  ##############################################################################
  # Proposals
  ##############################################################################
  proposal_type <- rep("lognorm", length(params0))
  
  list(simulate = simulate,observe = observe, compute_loglik = compute_loglik,
       is_invalid = is_invalid, inds_to_update = inds_to_update,
       proposal_type = proposal_type, params0 = params0)
}
