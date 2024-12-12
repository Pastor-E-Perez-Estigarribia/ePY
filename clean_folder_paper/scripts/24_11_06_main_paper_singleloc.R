library(tidyverse)
library(dfoptim) # For minimization routines
library(numDeriv) # To compute Hessian matrix
library(lubridate)
#theme_set(theme_bw())
rm(list=ls())

source("scripts/model_paper_singleloc.R")
#source("scripts/scalingFunctions_weekly.R")
source("scripts/utils_paper.R")

gamma = 7/15

#Get_data
case_df = read.csv("data/processed_data/case_data_upto16sep_23_weekly.csv") %>%
  filter(classification=="TOTAL" & name=="Paraguay" & disease == "CHIKUNGUNYA") %>% 
  mutate(date=as.Date(date)) %>% 
  arrange(date)

case_df %>% filter(date<"2022-12-01") %>% ggplot(aes(x=date,y=i_cases))+geom_line()+geom_vline(xintercept=as.Date("2022-09-01"), linetype="dashed")

case_df = case_df %>% filter(date>"2022-09-01")

coeffs_weather = read.csv("data/processed_data/coeffs_weather_weekly.csv", row.names = 1)

#Sero data
sero_df = data.frame(date="2023-08-12",
                     week_n = 428,
                     level = "National",
                     name= "Paraguay",
                     n_pos = 340,
                     n_tot = 1001)

#weigh by pop in each eje
sero_df = read.csv("data/processed_data/seroprevalence_by_eje.csv") %>%
  inner_join(read.csv("data/processed_data/pop_data.csv", row.names=1),by="name") %>%
  mutate(norm_pop = Population/sum(Population),
         n_pos_norm = n_pos*norm_pop/sum(n_tot*norm_pop)*sum(n_tot),
         n_tot_norm = n_tot*norm_pop/sum(n_tot*norm_pop)*sum(n_tot),
         serop = n_pos/n_tot,
         serop_norm = n_pos_norm/n_tot_norm)
sero_df %>% mutate(check_tot = n_tot_norm/sum(n_tot_norm)/norm_pop)
sero_df %>% select(n_pos, n_tot, n_pos_norm, n_tot_norm) %>% colSums()

sero_df = data.frame(date="2023-08-12",
                     week_n = 428,
                     level = "National",
                     name= "Paraguay",
                     n_pos = round(sum(sero_df$n_pos_norm)),
                     n_tot = sum(sero_df$n_tot_norm))

########################### Add in sensitivity, specificity of ELISA kits ##########################
sensitivity = 0.986
specificity = 0.98
n_neg = sero_df$n_tot - sero_df$n_pos
sero_df$n_pos = round(sero_df$n_pos * sensitivity + n_neg * (1-specificity))


#Population data
pop_metropolitano = read.csv("data/processed_data/pop_data.csv", row.names=1) %>%filter(name=="Metropolitano") %>% select(Population) %>% as.numeric()
pop_paraguay = read.csv("data/processed_data/pop_data.csv", row.names=1) %>%filter(level=="National") %>% select(Population) %>% as.numeric()
pop_introduction = 200

# Time points
ts <- sort(as.Date(unique(case_df$date)))

################################################################################
# Introduction functions assumption
################################################################################
no_introduction = function(date){sapply(date, function(x) return(0))}
constant_introduction = function(date){sapply(date, function(x) return(2e3))}
dirac_introduction = function(date) {
  require(lubridate)
  return(ifelse(
    epiweek(date) > 47 &
      epiweek(date) <= 52,
    pop_introduction,
    0
  ))
}

################################################################################
# Inference (MCMC)
################################################################################
mcmc_steps <- 25000
mcmc_adaptive_steps <- 10000
thinning <- seq(mcmc_adaptive_steps + 1, mcmc_steps, by = 10) #10)

input_case = case_df %>% 
  filter(classification=="TOTAL" & name=="Paraguay" & disease == "CHIKUNGUNYA") %>% 
  mutate(date=as.Date(date)) %>% 
  arrange(date) %>%
  mutate(n_pos = NA, n_tot=NA, prop_pos=NA, prop_lo=NA, prop_hi=NA)

idx_date_sero = sapply(sero_df$date, function(date) which(input_case$date==date))
input_case[idx_date_sero,c("n_pos","n_tot","prop_pos","prop_lo","prop_hi")] = cbind(sero_df[,c("n_pos","n_tot")],Hmisc::binconf(x = sero_df$n_pos, n=sero_df$n_tot))

params_fixed = expand.grid(list(immune_0=0.05*pop_metropolitano/pop_paraguay,
                                introduction=c("no_introduction"))) %>%
  mutate(introduction=factor(introduction, levels=c("no_introduction")))

# load("output/list_models.Rdata")
# list_models_prev = list_models

#Output structure
list_lev_2 = lapply(1:nrow(params_fixed), function(x) NULL)
names(list_lev_2)=paste0("rprt_",params_fixed$reporting,"_S0_",params_fixed$immune_0)
list_models = lapply(1:3, function(x) list_lev_2)  
names(list_models)=c("mod","output","fit")

# #Tranfer what has already been done
# for(element in names(list_models_prev$mod)){list_models$mod[element] = list_models_prev$mod[element]} 
# for(element in names(list_models_prev$fit)){list_models$fit[element] = list_models_prev$fit[element]} 
# for(element in names(list_models_prev$output)){list_models$output[element] = list_models_prev$output[element]} 

input_case = input_case %>% mutate(i_cases_raw=i_cases,
                                   i_cases = round(zoo::rollmean(x=i_cases,fill = 3, k=5)))
input_case %>%  tidyr::pivot_longer(cols = c(i_cases, i_cases_raw), names_to = "cat", values_to = "cases") %>%
  ggplot(aes(x=date, y=cases, col=cat))+geom_line()

# #Run MCMC
parm_set = 1:4 #Use param_set as a chain index when running multiple chains
params_fixed = params_fixed[rep(1,length(parm_set)),]

for(param_set in parm_set){
  print(paste0(param_set))
  list_models$mod[[param_set]] <- create_model(input_case, pop_paraguay, fits_coeff = coeffs_weather,
                                               location="National",
                                               prop_immune0 = params_fixed$immune_0[param_set],
                                               sir_gamma =  gamma,
                                               #reporting = params_fixed$reporting[param_set],
                                               #temperature_function = average_temperature,
                                               #rainfall_function = average_rainfall,
                                               scaling_function = weekly_scaling,
                                               introduction_function=no_introduction)
  print(list_models$mod[[param_set]]$params0[1:4])
  list_models$output[[param_set]] <-     tryCatch({run_MCMC(compute_loglik = list_models$mod[[param_set]]$compute_loglik,
                                                            is_invalid = list_models$mod[[param_set]]$is_invalid,
                                                            params0 = list_models$mod[[param_set]]$params0,
                                                            proposal_type = list_models$mod[[param_set]]$proposal_type,
                                                            inds_to_update = list_models$mod[[param_set]]$inds_to_update,
                                                            mcmc_steps = mcmc_steps,
                                                            #sd_proposal = c(0.1,0.1,0.1,0.1,0.1),
                                                            mcmc_adaptive_steps = mcmc_adaptive_steps,
                                                            verbose = T)},
                                                  error=function(e){print(e)})
}

#save(list_models, file="output/list_models_paper_singleloc_smoothed_temp.Rdata")

#load("output/list_models_paper_singleloc_smoothed_temp.Rdata")


#Plot thinned likelihood--------------------------
likelihood = lapply(parm_set, function(chain_id) list_models$output[[chain_id]]$loglik %>% as.data.frame() %>% mutate(chain_id=chain_id, iter=1:n()) %>% relocate(chain_id, iter)) %>%
  do.call(what=rbind)
likelihood %>% tidyr::pivot_longer(cols = -c(chain_id, iter)) %>%
  filter(iter %in% thinning) %>%
  ggplot(aes(x=iter, y=value, col=as.factor(chain_id)))+geom_line()

#Plot chains-------------
 #thinning
chain %>% tidyr::pivot_longer(cols = -c(chain_id, iter)) %>%
  filter(iter%in%thinning) %>%
  ggplot(aes(x=iter, y=value, col=as.factor(chain_id)))+geom_line()+facet_wrap(~name, scales="free_y")

# Plot acceptance rates------------
accept <- lapply(parm_set, function(chain_id) list_models$output[[chain_id]]$accept %>% as.data.frame() %>% mutate(chain_id=chain_id, iter=1:n()) %>% relocate(chain_id, iter)) %>%
  do.call(what=rbind)
colnames(accept)[-c(1,2)] = c_names[1:(-2+ncol(accept))]
accept %>% tidyr::pivot_longer(cols = -c(chain_id, iter)) %>%
  filter(iter%in%thinning) %>%
  ggplot(aes(x=iter, y=value, col=as.factor(chain_id)))+geom_line()+facet_wrap(~name, scales="free_y")


#Get diagnostics------------------
chain_coda = lapply(parm_set, function(ch_id){
  ch = chain %>% filter(chain_id==ch_id) %>% select(-c(chain_id,iter,nb_shape,prop_i0))
  coda::mcmc(data = ch)
})

chain_coda = coda::mcmc.list(chain_coda)
coda::effectiveSize(chain_coda)
coda::gelman.diag(chain_coda)
coda::gelman.plot(chain_coda)

#Model fit
interp_ts <- 0:(nrow(input_case)-1)
ts_sim = input_case$date
param_set = 1
ind = 30

#Rebuilding model to avoid pointer errors-----------
list_models$mod[[param_set]] <- create_model(input_case, pop_paraguay, fits_coeff = coeffs_weather,
                                             location="National",
                                             prop_immune0 = params_fixed$immune_0[param_set],
                                             sir_gamma = gamma,
                                             #reporting = params_fixed$reporting[param_set],
                                             #temperature_function = average_temperature,
                                             #rainfall_function = average_rainfall,
                                             scaling_function = weekly_scaling,
                                             introduction_function=no_introduction)

# Fit to data
# Simulate trajectories
n_sims <- 500
tchain <- lapply(parm_set, function(chain_id) list_models$output[[param_set]]$params[thinning, ]) %>% do.call(what=rbind) #Merge chains
inds <- sample(1:nrow(tchain), n_sims, replace = FALSE)
all_sims <- matrix(NA, nrow = length(interp_ts), ncol = n_sims)
all_sims_susceptible <- matrix(NA, nrow = length(interp_ts), ncol = n_sims)
all_sims_infections <- matrix(NA, nrow = length(interp_ts), ncol = n_sims)
all_sims_infections_cum <- matrix(NA, nrow = length(interp_ts), ncol = n_sims)
all_sims_rt <- matrix(NA, nrow = length(interp_ts), ncol = n_sims)
all_sims_beta <- matrix(NA, nrow = length(interp_ts), ncol = n_sims)
curr <- 0
for (ind in inds) {
  curr <- curr + 1
  curr_params <- as.numeric(as.matrix(tchain[ind, ]))
  curr_sims <- list_models$mod[[param_set]]$simulate(curr_params, interp_ts)
  all_sims[, curr] <-list_models$mod[[param_set]]$observe(curr_sims, curr_params)
  all_sims_susceptible[, curr] <-curr_sims[,"state_s"]
  all_sims_infections[, curr] <-curr_sims[,"i_inf"]
  all_sims_infections_cum[, curr] <-curr_sims[,"state_i"]
  all_sims_beta[, curr] = curr_params[4:(4+length(ts_sim)-1)]
  all_sims_rt[, curr] = curr_sims[,"state_s"] / pop_paraguay * all_sims_beta[, curr] / gamma # S * beta / gamma
}
list_models$fit[[param_set]] = list(obs=all_sims,
                                    susceptible=all_sims_susceptible,
                                    infections=all_sims_infections,
                                    infections_cum=all_sims_infections_cum,
                                    rt=all_sims_rt,
                                    beta=all_sims_beta,
                                    id_chain=inds)

fit_obs = list_models$fit[[param_set]]$obs
fit_susceptible = list_models$fit[[param_set]]$susceptible
fit_infections = list_models$fit[[param_set]]$infections
fit_infections_cum = list_models$fit[[param_set]]$infections_cum
fit_beta = list_models$fit[[param_set]]$beta
fit_rt = list_models$fit[[param_set]]$rt
df_fits = data.frame(
  date = ts_sim,
  prop_immune=params_fixed$immune_0[param_set],
  model= "sine",
  mean_i_cases = apply(fit_obs, 1, mean),
  lower_i_cases = apply(fit_obs, 1, quantile, probs = 0.025),
  upper_i_cases = apply(fit_obs, 1, quantile, probs = 0.975),
  mean_i_susceptible = apply(fit_susceptible, 1, mean),
  lower_i_susceptible = apply(fit_susceptible, 1, quantile, probs = 0.025),
  upper_i_susceptible = apply(fit_susceptible, 1, quantile, probs = 0.975),
  mean_i_infections = apply(fit_infections, 1, mean),
  lower_i_infections = apply(fit_infections, 1, quantile, probs = 0.025),
  upper_i_infections = apply(fit_infections, 1, quantile, probs = 0.975),
  mean_i_infections_cum = apply(fit_infections_cum, 1, mean),
  lower_i_infections_cum = apply(fit_infections_cum, 1, quantile, probs = 0.025),
  upper_i_infections_cum = apply(fit_infections_cum, 1, quantile, probs = 0.975),
  mean_beta = apply(fit_beta, 1, mean),
  lower_beta = apply(fit_beta, 1, quantile, probs = 0.025),
  upper_beta = apply(fit_beta, 1, quantile, probs = 0.975),
  mean_rt = apply(fit_rt, 1, mean),
  lower_rt = apply(fit_rt, 1, quantile, probs = 0.025),
  upper_rt = apply(fit_rt, 1, quantile, probs = 0.975))

#save(list_models, df_fits, input_case, file="output/list_models_paper_singleloc_smoothed.Rdata")

plot((df_fits$mean_i_infections[-1]/df_fits$mean_i_infections[nrow(df_fits)])[1:10])
df_fits$mean_i_infections

df_fits%>%filter(date!="1900-01-01") %>% group_by(date) %>% mutate(chain=1:n()) %>% ungroup() %>%
  #filter(date<"2022-12-01") %>% #Focus no first peak
  ggplot() +
  geom_hline(yintercept = 1, linetype="dashed", col=1) +
  geom_ribbon(aes(x = date, ymin =lower_rt, ymax = upper_rt), fill=rgb(0,1,0,0.2)) +
  geom_line(aes(x = date, y = mean_rt), color=rgb(0,1,0)) +
  xlab("") + ylab("R eff")+
  #facet_grid(~prop_immune,labeller=label_both) +
  expand_limits(y=c(0,1))+
  facet_wrap(~chain)+
  theme(legend.position = "none")+
  scale_x_date(date_breaks="1 month", date_labels="%b %y")+
  ggtitle("Effective reproductive nb.")


###################

# Plot estimates
chain = tchain
c_names <- c("prop_i0","reporting","nb_shape", paste0("scaling_",1:(ncol(chain)-3)))
colnames(chain) <- c_names[1:ncol(chain)]

pars_est = chain %>%
  as_tibble() %>%
  mutate() %>%
  gather(key = "variable", value = "value") %>%
  group_by(variable) %>%
  summarize(the_median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>% filter(!grepl("scaling", variable))

pars_est %>% ggplot() +
  geom_pointrange(aes(x = variable, y = the_median, ymin = lower,
                      ymax = upper)) +
  facet_wrap(~variable, scale = "free") +
  xlab("") + ylab("")

#write.csv(pars_est, file = "data/processed_data/estimated_parameters.csv")

chain %>%
  as_tibble() %>%
  mutate() %>%
  gather(key = "variable", value = "value") %>%
  group_by(variable) %>%
  summarize(the_median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>%
  filter(grepl("scaling", variable)) %>%
  mutate(week = as.numeric(substr(variable,9,length(variable)))) %>%
  ggplot() +
  geom_pointrange(aes(x = week, y = the_median, ymin = lower,
                      ymax = upper)) +
  xlab("") + ylab("")

chain %>%
  as_tibble() %>%
  mutate() %>%
  gather(key = "variable", value = "value") %>%
  group_by(variable) %>%
  summarize(the_median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>%
  filter(variable %in% c("reporting"))




