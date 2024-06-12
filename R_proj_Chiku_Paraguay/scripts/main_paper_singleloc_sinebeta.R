library(tidyverse)
library(dfoptim) # For minimization routines
library(numDeriv) # To compute Hessian matrix
library(lubridate)
theme_set(theme_bw())
rm(list=ls())

source("scripts/model_paper_singleloc.R")
#source("scripts/scalingFunctions_weekly.R")
source("scripts/utils_paper.R")


#Get_data
case_df = read.csv("data/processed_data/case_data_upto16sep_23_weekly.csv") %>%
  filter(date>"2022-09-01")
coeffs_weather = read.csv("data/processed_data/coeffs_weather_weekly.csv", row.names = 1)

#Sero data
sero_df = data.frame(date="2023-08-12",
                     week_n = 428,
                     level = "National",
                     name= "Paraguay",
                     n_pos = 340,
                     n_tot = 1001)

#Population data
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
mcmc_steps <- 15000
mcmc_adaptive_steps <- 5000
thinning <- seq(mcmc_adaptive_steps + 1, mcmc_steps, by = 10)

input_case = case_df %>% 
  filter(classification=="TOTAL" & name=="Paraguay" & disease == "CHIKUNGUNYA") %>% 
  mutate(date=as.Date(date)) %>% 
  arrange(date) %>%
  mutate(n_pos = NA, n_tot=NA, prop_pos=NA, prop_lo=NA, prop_hi=NA)

idx_date_sero = sapply(sero_df$date, function(date) which(input_case$date==date))
input_case[idx_date_sero,c("n_pos","n_tot","prop_pos","prop_lo","prop_hi")] = cbind(sero_df[,c("n_pos","n_tot")],Hmisc::binconf(x = sero_df$n_pos, n=sero_df$n_tot))

params_fixed = expand.grid(list(immune_0=0.37*0.05,
                                introduction=c("no_introduction"))) %>%
  mutate(introduction=factor(introduction, levels=c("no_introduction")))

# load("output/list_models.Rdata")
# list_models_prev = list_models

#Output structure
# list_lev_1 = vector("list", length = 3)
# names(list_lev_1) = c("Lambrechts","Mordecai","Perkins")
list_lev_2 = lapply(1:nrow(params_fixed), function(x) NULL)
names(list_lev_2)=paste0("rprt_",params_fixed$reporting,"_S0_",params_fixed$immune_0)
list_models = lapply(1:3, function(x) list_lev_2)  
names(list_models)=c("mod","output","fit")

# #Tranfer what has already been done
# for(element in names(list_models_prev$mod)){list_models$mod[element] = list_models_prev$mod[element]} 
# for(element in names(list_models_prev$fit)){list_models$fit[element] = list_models_prev$fit[element]} 
# for(element in names(list_models_prev$output)){list_models$output[element] = list_models_prev$output[element]} 

#Run MCMC
for(param_set in 1){
  print(paste0(param_set))
  list_models$mod[[param_set]] <- create_model(input_case, pop_paraguay, fits_coeff = coeffs_weather,
                                               location="National",
                                               prop_immune0 = params_fixed$immune_0[param_set],
                                               sir_gamma = 1 / 2,
                                               #reporting = params_fixed$reporting[param_set],
                                               #temperature_function = average_temperature,
                                               #rainfall_function = average_rainfall,
                                               scaling_function = sine_scaling,
                                               introduction_function=no_introduction)
  
  list_models$output[[param_set]] <-     tryCatch({run_MCMC(compute_loglik = list_models$mod[[param_set]]$compute_loglik,
                                                            is_invalid = list_models$mod[[param_set]]$is_invalid,
                                                            params0 = list_models$mod[[param_set]]$params0,
                                                            proposal_type = list_models$mod[[param_set]]$proposal_type,
                                                            inds_to_update = list_models$mod[[param_set]]$inds_to_update,
                                                            mcmc_steps = mcmc_steps,
                                                            sd_proposal = c(0.1,0.1,0.1,0.1,0.1),
                                                            mcmc_adaptive_steps = mcmc_adaptive_steps,
                                                            verbose = T)},
                                                  error=function(e){print(e)})
}

save(list_models, file="output/list_models_paper_singleloc.Rdata")

#Forecast
interp_ts <- 0:(52*2)
ts_sim = min(as.Date((case_df$date))) + interp_ts *7
param_set = 1

for(param_set in 1:nrow(params_fixed)){
  print(paste0(param_set))
  tryCatch({
    # Fit to data
    # Simulate trajectories
    n_sims <- 50
    tchain <- list_models$output[[param_set]]$params[thinning, ]
    inds <- sample(1:nrow(tchain), n_sims, replace = FALSE)
    all_sims <- matrix(NA, nrow = length(interp_ts), ncol = n_sims)
    all_sims_susceptible <- matrix(NA, nrow = length(interp_ts), ncol = n_sims)
    all_sims_infections <- matrix(NA, nrow = length(interp_ts), ncol = n_sims)
    curr <- 0
    for (ind in inds) {
      curr <- curr + 1
      curr_params <- as.numeric(as.matrix(tchain[ind, ]))
      curr_sims <- list_models$mod[[param_set]]$simulate_forecast(curr_params, interp_ts)
      all_sims[, curr] <-list_models$mod[[param_set]]$observe(curr_sims, curr_params)
      all_sims_susceptible[, curr] <-curr_sims[,"state_s"]
      all_sims_infections[, curr] <-curr_sims[,"i_inf"]
    }
    list_models$fit[[param_set]] = list(obs=all_sims,susceptible=all_sims_susceptible, infections=all_sims_infections)
  },
  error=function(e){})
}

# c_names <- c("prop_i0", "beta0")
# colnames(chain$params) <- c_names
# colnames(chain$accept) <- c_names

#Collate estimates together
df_fits=data.frame(date=as.Date("1900-01-01"),prop_immune=0,model=0,
                   mean_i_cases=0,lower_i_cases=0,upper_i_cases=0,
                   mean_i_susceptible=0,lower_i_susceptible=0,upper_i_susceptible=0,
                   mean_i_infections=0,lower_i_infections=0,upper_i_infections=0)

for(param_set in 1:nrow(params_fixed)){
  tryCatch({
    fit_obs = list_models$fit[[param_set]]$obs
    fit_susceptible = list_models$fit[[param_set]]$susceptible
    fit_infections = list_models$fit[[param_set]]$infections
    df = data.frame(
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
      upper_i_infections = apply(fit_infections, 1, quantile, probs = 0.975))
    df_fits = rbind(df_fits,df)
  },
  error=function(e){})
}

#Get numbers
tot_i = sum(df_fits$mean_i_infections) #tot infections
tot_c = sum(case_df %>% filter(level=="National" & classification == "TOTAL") %>% select(i_cases))
#Reporting rate
tot_c/tot_i*100

#Get trigger date
df_fits = df_fits %>% arrange(date) %>%
  mutate(roll_sum = zoo::rollsum(mean_i_cases, k=7, fill=0, align = "right")) %>%
  mutate(trigger_reactive = cumsum(roll_sum>20)>1,
         trigger_proactive= T)

#Add vaccination ramp up
coverage_vect = c(10,20,40,50)

get_prop_vac = function(trigger_vect, target_cov, date_vector, time_to_coverage = 4){
  if(sum(trigger_vect)==0){
    return(rep(0, length(date_vector)))
  }else{
    start_date = which(trigger_vect)[1]
    Vacc_vect = rep(0, length(date_vector))
    daily_rate = target_cov/time_to_coverage
    Vacc_vect[start_date:(start_date+ceiling(time_to_coverage)-1)] = cumsum(rep(daily_rate, ceiling(time_to_coverage)))
    Vacc_vect[(start_date+ceiling(time_to_coverage)-1):length(Vacc_vect)] = target_cov
    return(Vacc_vect[1:length(date_vector)])
  } #No outbreak detected
}

#Prop vaccinated
df_fits = df_fits %>% 
  mutate(prop_vacc = sapply(coverage_vect, function(cov){get_prop_vac(trigger_vect = trigger_reactive, target_cov = cov, date_vector = date)}) %>% 
           as.data.frame() %>% rename_all(~paste0("reac_cov",coverage_vect))) %>%
  bind_cols(.$prop_vacc) %>%
  select(-prop_vacc)
df_fits[,paste0("proac_cov",coverage_vect)] = matrix(coverage_vect, nrow=1)

#Prop protected (immunogenicity delay)
df_fits = df_fits %>% mutate(across(.cols = contains("reac_cov"),
                                    .fns = ~ c(rep(0,2),.x)[1:length(.x)],
                                    .names = "protected_{.col}"))
df_fits = df_fits %>% mutate(across(.cols = contains("proac_cov"),
                                    .fns = ~ .x,
                                    .names = "protected_{.col}"))


#Deterministic
pop_by_age = readxl::read_xlsx("data/raw_data/PROYECCION_POBLACIÓN_ AL 2025/POB_EDAD_SEXO_PAIS.xlsx") %>%
  filter(AÑO == 2020 & SEXO=="Masculino") %>% select(TOTAL, GRUPO_DE_EDAD)
prop_over12 = (sum(pop_by_age$TOTAL) - sum(pop_by_age$TOTAL[1:2]) -0.5*pop_by_age$TOTAL[3])/sum(pop_by_age$TOTAL)

ve = 0.75
df_fits = df_fits %>% mutate(across(.cols=contains("protected"),
                                    .fns=list(count_unvac = ~ mean_i_infections * (1-.x/100) * prop_over12 + mean_i_infections *  (1-prop_over12),
                                              count_vac = ~ (mean_i_infections * .x/100) * (1-ve) * prop_over12,
                                              count_tot = ~ (mean_i_infections * (1-.x/100) * prop_over12 + mean_i_infections *  (1-prop_over12)) +
                                                ((mean_i_infections * .x/100) * (1-ve) * prop_over12)),
                                    .names = "{.fn}_{.col}"))

names(df_fits) = gsub(pattern = "_protected", replacement = "", x = names(df_fits))


#Nb infections if vaccinated
tot_i = sum(df_fits$mean_i_infections)
tot_i_v = colSums(df_fits %>% select(count_tot_reac_cov40))
i_averted = tot_i-tot_i_v
ndoses = 0.4*pop_paraguay*prop_over12
i_averted
ndoses
i_averted/ndoses*10000
ifr = 0.0024 * 0.054
d_averted = i_averted * ifr
d_averted
d_averted/ndoses*10000

#Curve of daily cases
p_case = df_fits%>%filter(date!="1900-01-01") %>%
  filter(date<"2023-10-01") %>% #Focus no first peak
  ggplot() +
  geom_ribbon(aes(x = date, ymin = lower_i_cases, ymax = upper_i_cases, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = mean_i_cases, color=model)) +
  geom_point(data = input_case, aes(x = date, y = i_cases)) +
  #geom_line(data=scaling_vect, aes(x = time, y = scaling)) +
  xlab("") + ylab("Weekly N. of Cases")+
  #facet_grid(~prop_immune,labeller=label_both)+
  theme(legend.position = "none")+
  #scale_x_date(breaks=as.Date(c("2023-01-01","2024-01-01")), date_labels="%Y")+
  ggtitle("Cases")

p_infections = df_fits%>%filter(date!="1900-01-01") %>% mutate(mean_i_infections = mean_i_infections,
                                                               lower_i_infections = lower_i_infections,
                                                               upper_i_infections = upper_i_infections) %>%
  filter(date<"2023-10-01") %>% #Focus no first peak
  ggplot() +
  geom_ribbon(aes(x = date, ymin = lower_i_infections, ymax = upper_i_infections, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = mean_i_infections, color=model)) +
  xlab("") + ylab("Weekly N. of Infections")+
  #facet_grid(~prop_immune,labeller=label_both)+
  theme(legend.position = "none")+
  #scale_x_date(breaks=as.Date(c("2023-01-01","2024-01-01")), date_labels="%Y")+
  ggtitle("Infections")

p_immunity = df_fits%>%filter(date!="1900-01-01") %>%
  filter(date<"2023-10-01") %>% #Focus no first peak
  ggplot() +
  geom_ribbon(aes(x = date, ymin = 1-lower_i_susceptible/pop_paraguay, ymax = 1-upper_i_susceptible/pop_paraguay, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = 1-mean_i_susceptible/pop_paraguay, color=model)) +
  xlab("") + ylab("Immunity")+
  #facet_grid(~prop_immune,labeller=label_both) +
  geom_pointrange(data = input_case, aes(x = date, y = prop_pos, ymin=prop_lo, ymax=prop_hi)) +
  expand_limits(y=c(0,1))+
  theme(legend.position = "none")+
  #scale_x_date(breaks=as.Date(c("2023-01-01","2024-01-01")), date_labels="%Y")+
  ggtitle("Immunity")

cowplot::plot_grid(p_case,p_infections,p_immunity, ncol=3)


# Plot estimates
thinning <- seq(1, mcmc_steps, by = 1)
thinning <- seq(mcmc_adaptive_steps + 1, mcmc_steps, by = 10)

chain = list_models$output$rprt__S0_0.0185
c_names <- c("prop_i0", "beta0","reporting","nb_shape","peak_phase")
colnames(chain$params) <- c_names
colnames(chain$accept) <- c_names
chain$params[thinning, ] %>%
  as_tibble() %>%
  mutate() %>%
  gather(key = "variable", value = "value") %>%
  group_by(variable) %>%
  summarize(the_median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>%
  ggplot() +
  geom_pointrange(aes(x = variable, y = the_median, ymin = lower,
                      ymax = upper)) +
  facet_wrap(~variable, scale = "free") +
  xlab("") + ylab("")

chain$params[thinning, ] %>%
  as_tibble() %>%
  mutate() %>%
  gather(key = "variable", value = "value") %>%
  group_by(variable) %>%
  summarize(the_median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>%
  filter(variable %in% c("beta0","nb_shap",""))

# Plot chain (thinned)
chain$params[thinning, ]   %>%
  as_tibble() %>%
  mutate(iter = 1:n(),
         exp_nb_shape = exp(nb_shape),
         loglik = chain$loglik[thinning]) %>%
  gather(-iter, key = "variable", value = "value") %>%
  ggplot() +
  geom_line(aes(x = iter, y = value, color = variable)) +
  facet_wrap(~variable, scale = "free") +
  theme(legend.position = "none")

# Plot acceptance rates
chain$accept %>%
  as_tibble() %>%
  mutate(iter = 1:n()) %>%
  gather(-iter, key = "variable", value = "value") %>%
  ggplot() +
  geom_line(aes(x = iter, y = value, color = variable)) +
  facet_wrap(~variable) +
  scale_y_continuous("Value", limits = c(0, 1)) +
  theme(legend.position = "none")







