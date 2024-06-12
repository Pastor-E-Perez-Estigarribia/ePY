library(dplyr)
library(ggplot2)

# rm(list=ls())
# 
# source("scripts/model_weekly.R")
# source("scripts/cluster/model_weekly_estimate_negbin.R")
# source("scripts/utils.R")
# 
# theme_set(theme_bw())
# 
# load("output/list_models_estimate_negbin.Rdata")
# 
# mcmc_steps <- 15000
# mcmc_adaptive_steps <- 5000
# thinning <- seq(mcmc_adaptive_steps + 1, mcmc_steps, by = 100)
# 
# #Get_data
# case_df = read.csv("data/processed_data/case_data_upto16sep_23_weekly.csv") %>%
#   filter(date>"2022-11-01") 
# coeffs_weather = read.csv("data/processed_data/coeffs_weather_weekly.csv", row.names = 1)
# colnames(coeffs_weather) = gsub(pattern="\\.",replacement=" ",x=colnames(coeffs_weather))
# pop_df_eje = read.csv("data/processed_data/pop_data.csv", row.names=1) %>% 
#   filter(level=="Eje")
# 
# #Sero data
# sero_df = data.frame(date="2023-09-02",
#                      level = "Eje",
#                      name= pop_df_eje$name,
#                      n_pos = 173,
#                      n_tot = 455)
# 
# input_case = lapply(1:nrow(pop_df_eje), function(eje_id){
#   df = case_df %>% filter(classification=="TOTAL" & name==pop_df_eje$name[eje_id] & disease=="CHIKUNGUNYA") %>% mutate(date=as.Date(date)) %>% arrange(date) %>%
#     mutate(n_pos = NA, n_tot=NA, prop_pos=NA, prop_lo=NA, prop_hi=NA)
#   sero_df_eje = sero_df %>% filter(name==pop_df_eje$name[eje_id])
#   idx_date_sero = sapply(sero_df_eje$date, function(date) which(df$date==date))
#   df[idx_date_sero,c("n_pos","n_tot","prop_pos","prop_lo","prop_hi")] = cbind(sero_df_eje[,c("n_pos","n_tot")],Hmisc::binconf(x = sero_df_eje$n_pos, n=sero_df_eje$n_tot))
#   df
# })
# 
# n_loc = length(input_case)
# 
# # Time points
# ts <- sort(as.Date(unique(case_df$date)))
# interp_ts <- 0:(52*2)
# ts_sim = min(as.Date((case_df$date))) + interp_ts *7
# 
# #Parameters
# c_names <- c("nb_shape","beta0","peak_phase",paste0("propi0_",1:n_loc),paste0("reporting_",1:n_loc))
# prop_immune_0_vect = list((c(0,0,0,0,0.05)),(rep(0,5)),(rep(0.05,5)))
# params_fixed = expand.grid(list(immune_0=prop_immune_0_vect,
#                                 introduction=c("no_introduction","constant_introduction","dirac_introduction"))) %>%
#   mutate(introduction=factor(introduction, levels=c("no_introduction","constant_introduction","dirac_introduction")))
# 
# 
# pop_size=pop_df_eje$Population
# input_case = lapply(1:nrow(pop_df_eje), function(eje_id){
#   df = case_df %>% filter(classification=="TOTAL" & name==pop_df_eje$name[eje_id] & disease=="CHIKUNGUNYA") %>% mutate(date=as.Date(date)) %>% arrange(date) %>%
#     mutate(n_pos = NA, n_tot=NA, prop_pos=NA, prop_lo=NA, prop_hi=NA)
#   idx_date_sero = sapply(sero_df$date, function(date) which(df$date==date))
#   df[idx_date_sero,c("n_pos","n_tot","prop_pos","prop_lo","prop_hi")] = cbind(sero_df[,c("n_pos","n_tot")],Hmisc::binconf(x = sero_df$n_pos, n=sero_df$n_tot))
#   df
# })
# 
# #Convergence diagnostics (ESS) - TO DO : do Rhat as well
# param_set=2
# mod=2
# burn_in = 1:mcmc_adaptive_steps
# ess = lapply(1:nrow(params_fixed), function(param_set){
#   ess = sapply(1:3, function(mod){
#     #print(paste0("par_",param_set,"_mod_",mod))
#     chain = list_models$output[[param_set]][[mod]]$params
#     if(is.null(chain)) return(rep(NA,length(c_names))) else return(LaplacesDemon::ESS(chain[-burn_in,]))
#   })
#   rownames(ess) = c_names
#   as.data.frame(t(ess)) %>% 
#     mutate(mod = names(list_models$mod[[1]])) %>%
#     tidyr::pivot_longer(cols = -mod, names_to = "parameter", values_to = "ess") %>%
#     mutate(immune_0 = as.character(params_fixed$immune_0[param_set]),
#            introduction= params_fixed$introduction[param_set])
# }) %>% do.call(what=rbind)
# 
# hist(ess$ess)
# range(ess$ess)

#Visualise chains
param_set=1

n_loc = length(input_case)
c_names <- c("nb_shape","beta0","peak_phase",paste0("propi0_",1:n_loc),paste0("reporting_",1:n_loc))

chain_df = lapply(1:nrow(params_fixed), function(param_set){
  
  print(paste0("par_",param_set))
  chain = list_models$output[[param_set]]$params
  chain = if(is.null(chain))as.data.frame(matrix(NA, ncol=length(c_names))) else chain
  colnames(chain) = c_names
  chain = chain %>% as.data.frame() %>%
    mutate(nb_shape = exp(nb_shape),
           loglik =  list_models$output[[param_set]]$loglik) %>% 
    mutate(iter = 1:n()) %>%
    tidyr::pivot_longer(cols = -iter, names_to = "parameter", values_to = "value") %>%
    mutate(prop_immune=as.character(params_fixed$immune_0[param_set]),
           introduction=params_fixed$introduction[param_set],
           model= "sine")
  return(chain) %>% do.call(what=rbind)}) %>% do.call(what=rbind)

chain_df %>% filter(introduction=="no_introduction" & prop_immune=="c(0, 0, 0, 0, 0.05)") %>%
  filter(iter %in% thinning) %>%
  ggplot()+
  geom_line(aes(x=iter,y=value,col=model,group=model))+
  facet_wrap(~parameter, scales="free")


df_fits=data.frame(name=NA, date=as.Date("1900-01-01"),pop_size=0,prop_immune=0,introduction=NA,model=0,
                   mean_i_cases=0,lower_i_cases=0,upper_i_cases=0,
                   mean_i_infected=0,lower_i_infected=0,upper_i_infected=0,
                   mean_i_susceptible=0,lower_i_susceptible=0,upper_i_susceptible=0)

param_set = 1
for(param_set in 1:nrow(params_fixed)){
  print(paste0(param_set))
  tryCatch({
    if(T){
      fit_obs = lapply(list_models$fit[[param_set]], function(x) x$obs)
      fit_infected = lapply(list_models$fit[[param_set]], function(x) x$infected)
      fit_susceptible = lapply(list_models$fit[[param_set]], function(x) x$susceptible)
      df = lapply(1:n_loc, function(i){
        #print(i)
        data.frame(
          name=pop_df_eje$name[i],
          pop_size = pop_df_eje$Population[i],
          date = ts_sim,
          prop_immune=as.character(params_fixed$immune_0[param_set]),
          introduction=params_fixed$introduction[param_set],
          model= "sine",
          mean_i_cases = apply(fit_obs[[i]], 1, mean, na.rm=T),
          lower_i_cases = apply(fit_obs[[i]], 1, quantile, probs = 0.025, na.rm=T),
          upper_i_cases = apply(fit_obs[[i]], 1, quantile, probs = 0.975, na.rm=T),
          mean_i_infected = apply(fit_infected[[i]], 1, mean, na.rm=T),
          lower_i_infected = apply(fit_infected[[i]], 1, quantile, probs = 0.025, na.rm=T),
          upper_i_infected = apply(fit_infected[[i]], 1, quantile, probs = 0.975, na.rm=T),
          mean_i_susceptible = apply(fit_susceptible[[i]], 1, mean, na.rm=T),
          lower_i_susceptible = apply(fit_susceptible[[i]], 1, quantile, probs = 0.025, na.rm=T),
          upper_i_susceptible = apply(fit_susceptible[[i]], 1, quantile, probs = 0.975, na.rm=T))
      }) %>% do.call(what=rbind)
      df_fits = rbind(df_fits,df)
    }
  },
  error=function(e){})
}


#Get scaling parameter
# scaling_vect = data.frame(time = ts_sim,
#                           scaling = max(input_case$i_cases)*mordecai_scaling(temperature = average_temperature(date = ts_sim, coeff_table = coeffs_weather, location = "National"),
#                                                                              rainfall =  average_rainfall(date = ts_sim, coeff_table = coeffs_weather, location = "National")))
#Curve of daily cases
input_case = input_case %>% do.call(what=rbind)

unique(df_fits$prop_immune)
p_case_23 = df_fits%>%filter(date!="1900-01-01") %>% 
  filter(prop_immune=="c(0, 0, 0, 0, 0.05)") %>%
  filter(date<"2023-10-01") %>% #Focus no first peak
  ggplot() +
  geom_ribbon(aes(x = date, ymin = lower_i_cases, ymax = upper_i_cases, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = mean_i_cases, color=model)) +
  geom_point(data = input_case, aes(x = date, y = i_cases)) +
  #geom_line(data=scaling_vect, aes(x = time, y = scaling)) +
  xlab("") + ylab("Weekly N. of Cases")+
  facet_grid(name~introduction,labeller=label_both, scales="free")+
  theme(legend.position = "none")+
  scale_x_date(breaks=as.Date(c("2023-01-01","2024-01-01")), date_labels="%Y")+
  ggtitle("Cases")

p_infections_23 = df_fits%>%filter(date!="1900-01-01") %>% 
  filter(prop_immune=="c(0, 0, 0, 0, 0.05)") %>%
  filter(date<"2023-10-01") %>% #Focus no first peak
  mutate(mean_i_infections = mean_i_infected,
         lower_i_infections = lower_i_infected,
         upper_i_infections = upper_i_infected) %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = lower_i_infections, ymax = upper_i_infections, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = mean_i_infections, color=model)) +
  xlab("") + ylab("Weekly N. of Infections")+
  facet_grid(name~introduction,labeller=label_both, scales="free")+
  theme(legend.position = "none")+
  scale_x_date(breaks=as.Date(c("2023-01-01","2024-01-01")), date_labels="%Y")+
  ggtitle("Infections")

p_immunity_23 = df_fits%>%filter(date!="1900-01-01") %>% 
  filter(prop_immune=="c(0, 0, 0, 0, 0.05)") %>%
  filter(date<"2023-10-01") %>% #Focus no first peak
  ggplot() +
  geom_ribbon(aes(x = date, ymin = 1-lower_i_susceptible/pop_size, ymax = 1-upper_i_susceptible/pop_size, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = 1-mean_i_susceptible/pop_size, color=model)) +
  geom_pointrange(data = input_case, aes(x = date, y = prop_pos, ymin=prop_lo, ymax=prop_hi)) +
  xlab("") + ylab("Immunity")+
  facet_grid(name~introduction,labeller=label_both, scales="free") +
  expand_limits(y=c(0,1))+
  theme(legend.position = "none")+
  scale_x_date(breaks=as.Date(c("2023-01-01","2024-01-01")), date_labels="%Y")+
  ggtitle("Immunity")

p_case_23
cowplot::plot_grid(p_case_23,p_infections_23,p_immunity_23, ncol=3)


p_case = df_fits%>%filter(date!="1900-01-01") %>% 
  filter(prop_immune=="c(0, 0, 0, 0, 0.05)") %>%
  filter(date<"2023-06-01") %>% #Focus no first peak
  ggplot() +
  geom_ribbon(aes(x = date, ymin = lower_i_cases, ymax = upper_i_cases, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = mean_i_cases, color=model)) +
  geom_point(data = input_case, aes(x = date, y = i_cases)) +
  #geom_line(data=scaling_vect, aes(x = time, y = scaling)) +
  xlab("") + ylab("Weekly N. of Cases")+
  facet_grid(name~introduction,labeller=label_both, scales="free")+
  theme(legend.position = "none")+
  scale_x_date(breaks=as.Date(c("2023-01-01","2024-01-01")), date_labels="%Y")+
  ggtitle("Cases")

p_infections = df_fits%>%filter(date!="1900-01-01") %>% 
  filter(prop_immune=="c(0, 0, 0, 0, 0.05)") %>%
  #filter(date>"2023-06-01") %>% #Focus no first peak
  mutate(mean_i_infections = mean_i_infected,
         lower_i_infections = lower_i_infected,
         upper_i_infections = upper_i_infected) %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = lower_i_infections, ymax = upper_i_infections, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = mean_i_infections, color=model)) +
  xlab("") + ylab("Weekly N. of Infections")+
  facet_grid(name~introduction,labeller=label_both, scales="free")+
  theme(legend.position = "none")+
  scale_x_date(breaks=as.Date(c("2023-01-01","2024-01-01")), date_labels="%Y")+
  ggtitle("Infections")

p_immunity = df_fits%>%filter(date!="1900-01-01") %>% 
  filter(prop_immune=="c(0, 0, 0, 0, 0.05)") %>%
  #filter(date>"2023-06-01") %>% #Focus no first peak
  ggplot() +
  geom_ribbon(aes(x = date, ymin = 1-lower_i_susceptible/pop_size, ymax = 1-upper_i_susceptible/pop_size, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = 1-mean_i_susceptible/pop_size, color=model)) +
  geom_pointrange(data = input_case, aes(x = date, y = prop_pos, ymin=prop_lo, ymax=prop_hi)) +
  xlab("") + ylab("Immunity")+
  facet_grid(name~introduction,labeller=label_both, scales="free") +
  expand_limits(y=c(0,1))+
  theme(legend.position = "none")+
  scale_x_date(breaks=as.Date(c("2023-01-01","2024-01-01")), date_labels="%Y")+
  ggtitle("Immunity")

cowplot::plot_grid(p_case,p_infections,p_immunity, ncol=3)

# 
# ggsave(
#   "output/p_case_p_infections_p_immunity_202311021050.pdf",
#   plot = cowplot::plot_grid(p_case,p_infections,p_immunity, ncol=3),
#   width = 22.65248*2.2,
#   height = 14*1.2,
#   units = "cm",
#   dpi = 500,
#   scale=1
# )
# 
# 
# ggsave(
#   "output/p_case_p_infections_p_immunity_202311021050.svg",
#   plot = cowplot::plot_grid(p_case,p_infections,p_immunity, ncol=3),
#   width = 22.65248*2.2,
#   height = 14*1.5,
#   units = "cm",
#   dpi = 500,
#   scale=1
# )
# 


# Plot estimates
df_estimates = as.data.frame(matrix(NA,ncol=7,nrow=1))
colnames(df_estimates) = c("model", "immune_0", "introduction", "variable", "the_median","lower","upper")

param_set = 1
for(param_set in 1:nrow(params_fixed)){
  chain = list_models$output[[param_set]]
  colnames(chain$params) <- c_names
  colnames(chain$accept) <- c_names
  df_estimates = rbind(df_estimates,chain$params[thinning, ] %>%
                         as.data.frame %>%
                         tidyr::gather(key = "variable", value = "value") %>%
                         group_by(variable) %>%
                         summarize(the_median = median(value),
                                   lower = quantile(value, probs = 0.025),
                                   upper = quantile(value, probs = 0.975)) %>%
                         mutate(model="sine",
                                immune_0=as.character(params_fixed$immune_0[param_set]),
                                introduction=params_fixed$introduction[param_set]))
}


df_estimates = df_estimates%>% filter(!is.na(immune_0)) %>%
  mutate(variable_type=ifelse(grepl("propi0",variable),"propi0",ifelse(grepl("reporting",variable),"reporting",variable)))

fig_estimates = df_estimates%>% 
  filter(immune_0=="c(0, 0, 0, 0, 0.05)") %>%
  ggplot(aes(x=variable,y=the_median))+
  geom_pointrange(aes(ymin=lower,ymax=upper, shape=introduction, col=model)) +
  #expand_limits(y=0)+
  facet_wrap(~variable_type, scales="free")+
  ylab(NULL) +
  xlab(NULL)

fig_estimates
# 
# ggsave(
#   "output/fig_estimates_202311021050.pdf",
#   plot = fig_estimates,
#   width = 22.65248*2,
#   height = 14,
#   units = "cm",
#   dpi = 500,
#   scale=1
# )
# 
# 
# ggsave(
#   "output/fig_estimates_202311021050.svg",
#   plot = fig_estimates,
#   width = 22.65248*2,
#   height = 14,
#   units = "cm",
#   dpi = 500,
#   scale=1
# )


