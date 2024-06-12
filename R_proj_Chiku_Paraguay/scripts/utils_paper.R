################################################################################
# Function to create a seasonal function
################################################################################
sine_scaling <- function(t_date, peak_phase) {
  t_date = as.Date(t_date)
  1+cos(as.numeric(t_date-as.Date("2024-01-01"))/365*2*pi-peak_phase+pi)
}

weekly_scaling <- function(t_date, scaling_vect) {
  return(scaling_vect)
}

################################################################################
# Function to perform the MCMC sampling
################################################################################
run_MCMC <- function(compute_loglik, is_invalid, params0,
                     proposal_type = NULL, inds_to_update = NULL,
                     mcmc_steps = 1000, mcmc_adaptive_steps = 100,
                     sd_proposal = NULL, opt_acceptance = 0.24,
                     verbose = FALSE){
  
  #Debug
  param_set = 1
  compute_loglik = list_models$mod[[param_set]]$compute_loglik
  is_invalid = list_models$mod[[param_set]]$is_invalid
  params0 = list_models$mod[[param_set]]$params0
  proposal_type = list_models$mod[[param_set]]$proposal_type
  inds_to_update = list_models$mod[[param_set]]$inds_to_update
  mcmc_steps = mcmc_steps
  mcmc_adaptive_steps = mcmc_adaptive_steps
  verbose = T
  sd_proposal = NULL
  opt_acceptance = 0.24
  
  n_params <- length(params0)
  
  # Preallocate containers
  loglik <- rep(0, mcmc_steps)
  accept <- matrix(0, ncol = n_params, nrow = mcmc_steps) # Acceptance rates
  params <- matrix(0, ncol = n_params, nrow = mcmc_steps) # Parameters
  params[1, ] <- params0
  
  # Default choices when argument is NULL
  if (is.null(proposal_type)) { # Lognormal proposal for all parameters
    proposal_type <- rep("lognorm", n_params)
  }
  if (is.null(inds_to_update)) { # Update all parameters
    inds_to_update <- 1:n_params
  }
  if(is.null(sd_proposal)) { # Start with 0.1 for all parameters
    sd_proposal <- rep(0.1, n_params)
  }
  
  # MCMC loop start
  step <- 1
  loglik[step] <- compute_loglik(params[step, ])
  accept[step, inds_to_update] <- 1
  accepted <- rep(0, n_params) # Number of accepted moves
  accepted[inds_to_update] <- 1
  for (step in 2:mcmc_steps) {
    #for (step in 2:100) {
    #print(step)
    #step=2
    if (verbose & step %% 100 == 0) print(step)
    params[step, ] <- params[step - 1, ]
    loglik[step] <- loglik[step - 1]
    
    for (k in inds_to_update) { # Parameter update start
      old_param <- params[step, k]
      # Propose new param
      if (proposal_type[k] == "norm") {
        # Normal proposal
        new_param <- old_param + sd_proposal[k] * rnorm(1)
        log_proposal <- 0.0
      } else {
        # Lognormal proposal (default)
        new_param <- old_param * exp(sd_proposal[k] * rnorm(1))
        log_proposal <- log(new_param) - log(old_param)
      }
      # Reject immediately if new param is invalid
      if (is_invalid(k, new_param)) {
        next
      }
      
      params[step, k] <- new_param
      new_loglik <- compute_loglik(params[step, ])
      # Metropolis-Hastings
      log_ratio <- new_loglik - loglik[step] + log_proposal
      if (log(runif(1)) < log_ratio) {
        loglik[step] <- new_loglik
        accepted[k] <- accepted[k] + 1
      } else {
        params[step, k] <- old_param
      }
    } # Parameter update end
    
    # Acceptance rates
    accept[step, ] <- accepted / step
    
    # Adjust proposal sd to get closer to 'opt_acceptance'
    if (step <= mcmc_adaptive_steps) {
      delta <- 1 - step / mcmc_adaptive_steps
      for (k in inds_to_update) {
        diff <- accept[step, k] - opt_acceptance
        sd_proposal[k] <- sd_proposal[k] * (1.0 + delta * diff)
      }
    }
  } # MCMC loop end
  
  list(loglik = loglik, params = params, accept = accept)
}
