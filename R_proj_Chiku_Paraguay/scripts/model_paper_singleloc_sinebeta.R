require(odin)

################################################################################
# Odin model
################################################################################
sir_model <- odin::odin({
  
  scaling <- interpolate(interp_ts, interp_scaling, "constant")
  introduction_i <- interpolate(interp_ts, interp_introductions, "constant")
  
  # Derivatives
  s_to_i <- sir_beta0 * scaling * state_s * state_i / population
  deriv(state_s) <- -s_to_i # Susceptible
  deriv(state_i) <-  s_to_i - sir_gamma * state_i + introduction_i # Infectious
  
  # Initial conditions
  initial(state_s) <- state_s0
  initial(state_i) <- state_i0
  
  # Add incident number of infections to output
  output(i_inf) <- s_to_i # Incident number of infections
  
  # Parameters
  population <- user()
  sir_beta0  <- user()
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
  #   2 = beta0
  #   3 = reporting
  #   4 = neg shape
  #   5 = peak_phase
  ##############################################################################
  params0 <- c(0.001, 1./7, 0.05, log(1e3), 2)
  
  ##############################################################################
  # Climate effect
  ##############################################################################
  # interp_scaling <- scaling_function(temperature_function(date=ts,location=location,coeff_table=fits_coeff),
  #                                    rainfall_function(date=ts,location=location,coeff_table=fits_coeff))
  interp_scaling = function(p_p) sine_scaling(t_date = ts, peak_phase = p_p)
  
  ##############################################################################
  # Introducation of infections
  ##############################################################################
  interp_introductions <- introduction_function(date=ts)
  
  ##############################################################################
  # Simulation function (for model fitting)
  ##############################################################################
  simulator <- sir_model$new(
    population = population,
    sir_beta0 = params0[2],
    sir_gamma = sir_gamma,
    state_s0 = population * (1 - prop_immune0) * (1 - params0[1]),
    state_i0 = population * (1 - prop_immune0) * params0[1],
    interp_ts = ts_int,
    interp_scaling = interp_scaling(p_p = params0[5]),
    interp_introductions = interp_introductions
  )
  
  simulate <- function(params, t_points) {
    # NOTE:
    #   params[1] = prop of individuals infected at t0
    #   params[2] = beta0
    #   params[3] = reporting rate
    simulator$set_user(
      sir_beta0 = params[2],
      state_s0 = population * (1 - prop_immune0) * (1 - params[1]),
      state_i0 = population * (1 - prop_immune0) * params[1],
      interp_scaling = interp_scaling(p_p = params[5])
    )
    simulator$run(t_points)
  }
  
  ##############################################################################
  # Simulation function (for forecasting)
  ##############################################################################
  ts_int_forecast <- 0:(52*2)
  ts_forecast <- min(as.Date((data$date))) + ts_int_forecast *7
  
  interp_scaling_forecast = function(p_p) sine_scaling(t_date = ts_forecast, peak_phase = p_p)
  
  interp_introductions_forecast <- introduction_function(date=ts_forecast)
  
  simulator_forecast <- sir_model$new(
    population = population,
    sir_beta0 = params0[2],
    sir_gamma = sir_gamma,
    state_s0 = population * (1 - prop_immune0) * (1 - params0[1]),
    state_i0 = population * (1 - prop_immune0) * params0[1],
    interp_ts = ts_int_forecast,
    interp_scaling = interp_scaling_forecast(p_p = params0[5]),
    interp_introductions = interp_introductions_forecast
  )
  
  simulate_forecast <- function(params, t_points) {
    # NOTE:
    #   params[1] = prop of individuals infected at t0
    #   params[2] = beta0
    simulator_forecast$set_user(
      sir_beta0 = params[2],
      state_s0 = population * (1 - prop_immune0) * (1 - params[1]),
      state_i0 = population * (1 - prop_immune0) * params[1],
      interp_scaling = interp_scaling_forecast(p_p = params[5])
    )
    simulator_forecast$run(t_points)
  }
  
  ##############################################################################
  # Observation process
  ##############################################################################
  observe <- function(sims, params) {
    #rpois(nrow(sims), lambda = params[3] * sims[, "i_inf"])
    rnbinom(nrow(sims), mu = params[3] * sims[, "i_inf"], size = exp(params[4]))
  }
  
  ##############################################################################
  # Log-likelihood function
  ##############################################################################
  observed <- data$i_cases
  observed_sero_pos = data$n_pos
  observed_sero_tot = data$n_tot
  
  compute_loglik <- function(params) {
    sims <- simulate(params, ts_int)
    expected <- params[3] * sims[, "i_inf"]
    expected_sero = (population - sims[,"state_s"])/population
    
    #POSSON LIKELIHOOD
    # loglik = sum(dpois(observed, lambda = expected+1e-6, log = TRUE), na.rm=T) + #Case data likelihood
    #   1*sum(dbinom(x=observed_sero_pos,size=observed_sero_tot,prob=expected_sero,log=TRUE), na.rm=T)
    
    #NEGBIN LIKELIHOOD
    loglik =  sum(dnbinom(observed, mu = expected+1e-6, size=exp(params[4]),log = TRUE), na.rm=T) + #Case data likelihood
      sum(dbinom(x=observed_sero_pos,size=observed_sero_tot,prob=expected_sero,log=TRUE), na.rm=T)
    
    
    return(loglik) #Sero data likelihood
  }
  
  ##############################################################################
  # Function that checks if parameter value is invalid
  ##############################################################################
  is_invalid <- function(k, value) {
    if ((k == 1) & (value > 0.1)) return(TRUE) # Prop infected at t0
    if ((k == 2) & (value > 10)) return(TRUE) # beta0
    if ((k == 3) & (value > 1)) return(TRUE) # reporting
    if ((k == 4) & (value < log(5))) return(TRUE) # negbin shape
    if ((k == 5) & ((value < 0) | (value>(2*pi)))) return(TRUE) # Peak phase
    FALSE
  }
  
  ##############################################################################
  # Indices to update
  ##############################################################################
  inds_to_update <- c(1,2,3,5)
  
  ##############################################################################
  # Proposals
  ##############################################################################
  proposal_type <- rep("lognorm", 5)
  
  list(simulate = simulate, simulate_forecast=simulate_forecast ,observe = observe, compute_loglik = compute_loglik,
       is_invalid = is_invalid, inds_to_update = inds_to_update,
       proposal_type = proposal_type, params0 = params0)
}
